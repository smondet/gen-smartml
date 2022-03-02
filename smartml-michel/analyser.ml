(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils
open Control
open Expr
open Type

(** Make the results of the first algebra accessible from the second
   one. *)
let with_alg alg1 alg2 e = (alg1 (map_expr_f fst e), alg2 e)

(** There are two ways of counting variables: 'x' has two syntactic
   occurrences in 'if a then x else x', but is consumed only once. *)
type 'a var_count_mode =
  { var : 'a
  ; seq : 'a -> 'a -> 'a
  ; par : 'a -> 'a -> 'a
  ; repeat : 'a -> 'a }

let mode_occurrences = {var = 1; seq = ( + ); par = ( + ); repeat = id}

let mode_consumptions =
  let seq = function
    | `Zero, `Zero -> `Zero
    | `Zero, `One | `One, `Zero -> `One
    | _ -> `Unknown
  in
  let par x y = if x = y then x else `Unknown in
  {var = `One; seq = curry seq; par; repeat = (fun _ -> `Unknown)}

(** Count free variable occurrences according to the given mode. *)
let count_free_vars_f {var; repeat; seq; par} =
  let open String.Map in
  let remove_var = Option.cata id remove in
  let remove_vars xs vars =
    List.fold_left ( |> ) vars (List.map remove_var xs)
  in
  let remove_pattern = function
    | P_var x -> remove_var x
    | P_vector xs -> remove_vars xs
  in
  let remove_rp rp = remove_vars (Binary_tree.to_list rp) in
  let ( &> ) = union (fun _ c1 c2 -> Some (seq c1 c2)) in
  let ( &| ) = union (fun _ c1 c2 -> Some (par c1 c2)) in
  function
  | Var v -> singleton v var
  | Let_in (xs, e1, e2) -> e1 &> remove_pattern xs e2
  | Lambda (x, _, _, e) -> remove_var x e
  | Match_record (rp, e1, e2) -> e1 &> remove_rp rp e2
  | Match_variant (e, clauses) ->
      let clauses = Binary_tree.to_list clauses in
      ( match List.map (fun {var; rhs} -> remove_var var rhs) clauses with
      | c :: cs -> e &> List.fold_left ( &| ) c cs
      | [] -> e )
  | If (e1, e2, e3) -> e1 &> (e2 &| e3)
  | If_none (e1, x, e2, e3) -> e1 &> (e2 &| remove_var x e3)
  | If_left (e1, x1, e2, x2, e3) -> e1 &> (remove_var x1 e2 &| remove_var x2 e3)
  | If_cons (e1, x1, x2, e2, e3) -> e1 &> (remove_vars [x1; x2] e2 &| e3)
  | Create_contract ({parameter_and_storage; body}, baker, balance, storage) ->
      remove_var parameter_and_storage body &> baker &> balance &> storage
  | Loop (init, xs, step)
   |Iter_over (init, xs, step)
   |Map_over (init, xs, step) ->
      List.fold_left ( &> ) (String.Map.map repeat (remove_vars xs step)) init
  | e -> fold_expr_f ( &> ) empty e

let free_vars_f e =
  let singl = String.map_of_set (fun _ -> 1) in
  let keys m = m |> String.Map.keys |> String.Set.of_list in
  keys (count_free_vars_f mode_occurrences (map_expr_f singl e))

let count_free_vars mode = cata_expr (count_free_vars_f mode)

let with_count_free_vars mode x = with_alg (count_free_vars_f mode) x

let with_count_free_occurrences x = with_count_free_vars mode_occurrences x

(** Checks that loop bodies do not refer to variables in outer scope. *)
let loops_closed =
  let f e =
    let l =
      match map_expr_f fst e with
      | Loop (_, xs, step) ->
          let f k v =
            assert (v >= 1);
            List.mem (Some k) xs
          in
          String.Map.for_all f step
      | _ -> true
    in
    fold_expr_f ( && ) l (map_expr_f snd e)
  in
  fun e -> snd (cata_expr (with_count_free_occurrences f) e)

(** Checks a given property on the number of variable occurrences
   below each binding site. *)
let check_bound_occs mode check =
  let check_occ m x = check x (String.Map.find_opt x m) in
  let check_var m = Option.cata true (check_occ m) in
  let check_vars m = List.for_all (check_var m) in
  let check_pattern m = function
    | P_var x -> check_var m x
    | P_vector xs -> check_vars m xs
  in
  let check_rp m rp = check_vars m (Binary_tree.to_list rp) in
  let f e =
    let l =
      (* Check all bound variables at this level: *)
      match map_expr_f fst e with
      | Let_in (xs, _, e) -> check_pattern e xs
      | Lambda (x, _, _, e) -> check_var e x
      | Match_record (rp, _, e) -> check_rp e rp
      | Match_variant (_, clauses) ->
          List.for_all
            (fun {var; rhs} -> check_var rhs var)
            (Binary_tree.to_list clauses)
      | Loop (_, xs, e2) -> check_vars e2 xs
      | If_none (_, x, _, e3) -> check_var e3 x
      | If_left (_, x1, e1, x2, e2) -> check_var e1 x1 && check_var e2 x2
      | If_cons (_, x1, x2, e, _) -> check_vars e [x1; x2]
      | Create_contract ({parameter_and_storage; body}, _, _, _) ->
          check_vars body [parameter_and_storage]
      | Iter_over (_, xs, step) | Map_over (_, xs, step) -> check_vars step xs
      | Var _ | Lit _ | Prim0 _ | Prim1 _ | Prim1_fail _ | Prim2 _ | Prim3 _
       |Proj_field _ | Stack_op _ | Record _ | Variant _ | List _ | Set _
       |Map _ | Vector _ | Nth _ | Unpair _ | If _ | Record_of_tree _
       |Comment _ ->
          true
    in
    fold_expr_f ( && ) l (map_expr_f snd e)
  in
  fun e -> snd (cata_expr (with_count_free_vars mode f) e)

(* TODO Apply checks to variables occurring freely as well. *)
let is_linear =
  check_bound_occs mode_consumptions (fun _ ->
      Option.cata false (fun n -> n = `One))

let bindings_used =
  check_bound_occs mode_occurrences (fun _ ->
      Option.cata false (fun n -> n >= 1))

(** How many times is each variable bound? *)
let count_bindings =
  let open String.Map in
  let sum_map = String.Map.union (fun _ x y -> Some (x + y)) in
  let sum_maps = List.fold_left sum_map String.Map.empty in
  let count_var = Option.cata empty (fun x -> singleton x 1) in
  let count_tree f x = Binary_tree.(fold sum_map empty (map f x)) in
  let f = function
    | Let_in (xs, e1, e2) ->
        let xs =
          match xs with
          | P_var x -> [x]
          | P_vector xs -> xs
        in
        sum_maps (e1 :: e2 :: List.map count_var xs)
    | Lambda (x, _, _, e) -> sum_map (count_var x) e
    | Match_record (rp, e1, e2) -> sum_maps [count_tree count_var rp; e1; e2]
    | Match_variant (e, clauses) ->
        let count_clause {var; rhs} = sum_map (count_var var) rhs in
        sum_map e (count_tree count_clause clauses)
    | Loop (init, xs, step)
     |Iter_over (init, xs, step)
     |Map_over (init, xs, step) ->
        sum_maps ((step :: init) @ List.map count_var xs)
    | If_none (e1, x, e2, e3) -> sum_maps [e1; count_var x; e2; e3]
    | If_left (e1, x1, e2, x2, e3) ->
        sum_maps [e1; count_var x1; e2; count_var x2; e3]
    | If_cons (e1, x1, x2, e2, e3) ->
        sum_maps [e1; count_var x1; count_var x2; e2; e3]
    | Create_contract ({parameter_and_storage; body}, e1, e2, e3) ->
        sum_maps [count_var parameter_and_storage; body; e1; e2; e3]
    | e -> fold_expr_f sum_map String.Map.empty e
  in
  cata_expr f

(** Does every variable binding have a unique name? *)
let bindings_unique e = String.Map.for_all (fun _ n -> n = 1) (count_bindings e)

let may_fail =
  cata_expr (function
      | Loop _ -> true
      | Prim1_fail _ -> true
      | Prim2 (Exec, _, _) -> true
      | Prim2 ((Lsl | Lsr | Add | Sub | Mul), _, _) ->
          true (* overflow on some types *)
      | e -> fold_expr_f ( || ) false e)

let may_fail_typed_f =
  let is_tmutez = function
    | Stack_ok (T0 T_mutez) -> true
    | _ -> false
  in
  function
  | Loop _ -> true
  | Prim1_fail _ -> true
  | Prim2 (Exec, _, _) -> true
  | Prim2 ((Lsl | Lsr), _, _) -> true
  | Prim2 ((Add | Sub | Mul), (_, t1), (_, t2)) -> is_tmutez t1 || is_tmutez t2
  | e -> fold_expr_f ( || ) false (map_expr_f fst e)

let always_fails =
  let f = function
    | Prim1_fail _ -> true
    | Match_variant (_, clauses) ->
        List.for_all (fun {rhs} -> rhs) (Binary_tree.to_list clauses)
    | If (_, l, r)
     |If_none (_, _, l, r)
     |If_left (_, _, l, _, r)
     |If_cons (_, _, _, l, r) ->
        l && r
    | Loop _ | Iter_over _ | Map_over _ -> false
    | Lambda _ -> false
    | Create_contract (_, baker, balance, storage) ->
        baker || balance || storage
    | ( Var _ | Let_in _ | Lit _ | Prim0 _ | Prim1 _ | Prim2 _ | Prim3 _
      | Proj_field _ | Stack_op _ | Record _ | Variant _ | List _ | Set _
      | Map _ | Match_record _ | Vector _ | Nth _ | Unpair _ | Record_of_tree _
      | Comment _ ) as e ->
        fold_expr_f ( || ) false e
  in
  cata_expr f
