(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils
open Control
open Type
open Binary_tree
open Expr
open Typing
module TO = Binary_tree.Traversable (Option)

let debug_level = 0

let typecheck_before_and_after = false

let typecheck_after_each_step = false

let check_conditions = false

type transformer =
  | Atom_typed    of (texpr -> expr)
  | Atom_untyped  of (expr -> expr)
  | Fixpoint      of transformer
  | Sequence      of transformer list
  | Label         of string * transformer
  | Precondition  of (expr -> string option) * transformer
  | Postcondition of (expr -> expr -> string option) * transformer

let mk_t label transform conditions =
  Label (label, List.fold_left ( |> ) (Atom_typed transform) conditions)

let mk label transform conditions =
  Label (label, List.fold_left ( |> ) (Atom_untyped transform) conditions)

let _pre (lbl, c) t =
  let c x = if c x then None else Some ("pre: " ^ lbl) in
  Precondition (c, t)

let post (lbl, c) t =
  let c _ y = if c y then None else Some ("post: " ^ lbl) in
  Postcondition (c, t)

let pre_post (lbl, c) t =
  let c x y =
    if c x
    then if c y then None else Some ("post: " ^ lbl)
    else Some ("pre: " ^ lbl)
  in
  Postcondition (c, t)

let preserves (lbl, c) t =
  let c x y = if (not (c x)) || c y then None else Some ("preserves: " ^ lbl) in
  Postcondition (c, t)

let log_time s f x =
  if debug_level > 0
  then (
    let t0 = Unix.gettimeofday () in
    let y = f x in
    let dt = Unix.gettimeofday () -. t0 in
    Printf.eprintf "%s %3.3fms\n" s (dt *. 1000.0);
    flush stderr;
    y )
  else f x

let typecheck_or_fail lbl x =
  let y = log_time (lbl ^ ": typecheck") typecheck_precontract x in
  match y with
  | Ok y -> y
  | Error msg ->
      let pp = print_precontract print_expr in
      failwith (Format.asprintf "@[<v>%s: typecheck: %s\n@]%a" lbl msg pp x)

let typecheck_or_fail_b lbl before after =
  let y = log_time (lbl ^ ": typecheck") typecheck_precontract after in
  match y with
  | Ok y -> y
  | Error msg ->
      let pp = print_precontract print_expr in
      let err =
        Format.asprintf "@[<v>%s: typecheck: %s@;Before:@;%a@;After:@;%a@]"
      in
      failwith (err lbl msg pp before pp after)

let rec run_transformer
    path t ({tparameter; tstorage; parameter_and_storage; body} as x) =
  let path_str = List.fold_left (fun x y -> x ^ " > " ^ y) "." path in
  let ppc = print_precontract print_expr in
  match t with
  | Atom_typed transform ->
      let x' = typecheck_or_fail path_str x in
      let y =
        let body = transform x'.checked_precontract.body in
        {tparameter; tstorage; parameter_and_storage; body}
      in
      if typecheck_after_each_step
      then (
        ignore (typecheck_or_fail_b path_str x y);
        y )
      else y
  | Atom_untyped transform ->
      let y =
        let body = transform body in
        {tparameter; tstorage; parameter_and_storage; body}
      in
      if typecheck_after_each_step
      then (
        ignore (typecheck_or_fail_b path_str x y);
        y )
      else y
  | Fixpoint t ->
      let rec fixpoint n x =
        if n = 0
        then failwith (Printf.sprintf "%s: failed to converge" path_str)
        else
          let y = run_transformer path t x in
          if equal_precontract equal_expr x y then x else fixpoint (n - 1) y
      in
      fixpoint 1000 x
  | Sequence ts -> List.fold_left ( |> ) x (List.map (run_transformer path) ts)
  | Label (lbl, t) ->
      let y =
        log_time
          (path_str ^ " > " ^ lbl)
          (fun () -> run_transformer (path @ [lbl]) t x)
          ()
      in
      if debug_level >= 2 && not (equal_precontract equal_expr x y)
      then Printf.eprintf "  progress %d" (Expr.size_expr y.body);
      y
  | Precondition (_, t) when not check_conditions -> run_transformer path t x
  | Precondition (c, t) ->
    ( match c x.body with
    | None -> run_transformer path t x
    | Some msg ->
        failwith
          (Format.asprintf
             "%s: precondition violated: %s\n%a"
             path_str
             msg
             ppc
             x) )
  | Postcondition (_, t) when not check_conditions -> run_transformer path t x
  | Postcondition (c, t) ->
      let y = run_transformer path t x in
      ( match c body y.body with
      | None -> y
      | Some msg ->
          let msg = path_str ^ ": postcondition violated: " ^ msg in
          failwith
            (Format.asprintf
               "@[<v>%s@;@;Before:@;%a@;After:@;%a@]"
               msg
               ppc
               x
               ppc
               y) )

let run_transformer t c =
  if typecheck_before_and_after
  then ignore (typecheck_or_fail "initial check" c);
  let c = log_time "run_transformer" (run_transformer [] t) c in
  if debug_level >= 1 then prerr_endline "";
  if typecheck_before_and_after then ignore (typecheck_or_fail "final check" c);
  c

type state = {var_counter : int ref}

let fresh {var_counter} x =
  let i = !var_counter in
  incr var_counter;
  x ^ string_of_int i

let freshen st s = List.map (fun _ -> fresh st s)

let sum_map = String.Map.union (fun _ x y -> Some (x + y))

let map_of_list f xs =
  List.fold_left
    (String.Map.union (fun _k x y -> Some (f x y)))
    String.Map.empty
    (List.map (fun (k, v) -> String.Map.singleton k v) xs)

(** How many times is each variable used? *)
let count_occs_f = function
  | Var v -> String.Map.singleton v 1
  | e -> fold_expr_f sum_map String.Map.empty e

let count_occs = cata_expr count_occs_f

let bindings_unique = ("bindings_unique", Analyser.bindings_unique)

let bindings_used = ("bindings_used", Analyser.bindings_used)

let is_linear = ("is_linear", Analyser.is_linear)

let loops_closed = ("loops_closed", Analyser.loops_closed)

let is_pure e = (* TODO may not diverge *) not (Analyser.may_fail e)

let is_simple =
  let f = function
    | Var _ -> true
    | Lit _ -> true
    | Prim0 _ -> true
    | Prim1 ((Car | Cdr), x) -> x
    | Prim2 (Pair _, x, y) -> x && y
    | Record r -> for_all (fun (_, e) -> e) r
    | Vector xs -> List.for_all id xs
    | Variant (_, _, e) -> e
    | _ -> false
  in
  cata_expr f

let is_var = function
  | {expr = Var _} -> true
  | _ -> false

let is_rev_ops = function
  | {expr = Prim2 (Exec, {expr = Var _}, {expr = Var "!rev_ops"})} -> true
  | _ -> false

(** Inlines let-bindings that are used exactly once. *)
let inline_singles =
  let f e subst =
    let unchanged () = {expr = map_expr_f (fun ((_, _, x), _) -> x subst) e} in
    match e with
    | Var v -> Option.default (var v) (String.Map.find_opt v subst)
    | Let_in (P_var (Some x), ((_, may_fail1, e1), _), ((occs2, _, e2), _)) ->
        let e1 = e1 subst in
        let rev_ops = is_rev_ops e1 in
        if (not may_fail1) || rev_ops
        then
          let single_occ = String.Map.lookup ~default:0 x occs2 = 1 in
          let simple = is_simple e1 || rev_ops in
          if (simple || single_occ) && String.sub x 0 2 <> "__"
          then e2 (String.Map.add x e1 subst)
          else let_in_var (Some x) e1 (e2 subst)
        else unchanged ()
    | _ -> unchanged ()
  in
  let f e =
    ( count_occs_f (map_expr_f (fun ((occs, _, _), _) -> occs) e)
    , Analyser.may_fail_typed_f
        (map_expr_f (fun ((_, mf, _), tys) -> (mf, tys)) e)
    , f e )
  in
  let f e =
    let _, _, r = cata_texpr (fun e _ -> f e) e in
    r String.Map.empty
  in
  mk_t "inline" f [preserves bindings_unique; preserves is_linear]

(** Performs two kinds of inlining:

(1) Let-bindings to variables, e.g.
  let x = y in x + x
-->
  y + y

(2) Literals in certain positions, e.g.:
  let x = 2 in x + y
-->
  2 + y

Note that 'let y = 2 in x + y' is left untouched.
*)
let inline_simples =
  let f e subst =
    let lookup_var = function
      | {expr = Var v} as e ->
        ( match String.Map.find_opt v subst with
        | Some ({expr = Var _} as e) -> e
        | _ -> e )
      | e -> e
    in
    let lookup_lit = function
      | {expr = Var v} as e ->
        ( match String.Map.find_opt v subst with
        | Some ({expr = Lit _} as e) -> e
        | _ -> e )
      | e -> e
    in
    let is_lit = function
      | {expr = Lit _} -> true
      | _ -> false
    in
    match e with
    | Let_in (P_var (Some x), e1, e2) ->
        let e1 = e1 subst in
        let e2 = e2 (String.Map.add x e1 subst) in
        let_in_var (Some x) e1 e2
    | _ ->
        let expr = map_expr_f (fun x -> x subst) e in
        ( match expr with
        | Var _ -> lookup_var {expr}
        | Prim1 (p, e1) -> prim1 p (lookup_lit e1)
        | Prim2 (p, e1, e2) when is_lit e1 -> prim2 p e1 (lookup_lit e2)
        | Prim2 (p, e1, e2) -> prim2 p (lookup_lit e1) e2
        | Prim3 (p, e1, e2, e3) when is_lit e1 && is_lit e2 ->
            prim3 p e1 e2 (lookup_lit e3)
        | Prim3 (p, e1, e2, e3) when is_lit e1 -> prim3 p e1 (lookup_lit e2) e3
        | Prim3 (p, e1, e2, e3) -> prim3 p (lookup_lit e1) e2 e3
        | expr -> {expr} )
  in
  let f e = cata_expr f e String.Map.empty in
  mk "inline_simples" f [preserves bindings_unique; preserves is_linear]

(** Replace unused variables bindings with a '_', e.g.
  let x = 2 in 3
-->
  let _ = 2 in 3
*)
let mark_unused =
  let mark_var occs = function
    | Some x when not (String.Map.mem x occs) -> None
    | x -> x
  in
  let mark_rp occs = Binary_tree.map (mark_var occs) in
  let mark_vars occs = List.map (mark_var occs) in
  let mark_pattern occs = function
    | P_var x -> P_var (mark_var occs x)
    | P_vector xs -> P_vector (mark_vars occs xs)
  in
  let mark_clause {cons; var; rhs = occs, rhs} =
    {cons; var = mark_var occs var; rhs}
  in
  let f = function
    | Let_in (xs, (_, e1), (occs, e2)) -> let_in (mark_pattern occs xs) e1 e2
    | Lambda (x, a, b, (occs, e)) -> lambda (mark_var occs x) a b e
    | Match_record (rp, (_, e1), (occs, e2)) ->
        match_record (mark_rp occs rp) e1 e2
    | Match_variant ((_, scrutinee), clauses) ->
        match_variant scrutinee (Binary_tree.map mark_clause clauses)
    | If_none ((_, e1), x, (_, e2), (occs, e3)) ->
        if_none e1 e2 (mark_var occs x, e3)
    | If_left ((_, e1), x1, (occs2, e2), x2, (occs3, e3)) ->
        if_left e1 (mark_var occs2 x1, e2) (mark_var occs3 x2, e3)
    | If_cons ((_, e1), x1, x2, (occs, e2), (_, e3)) ->
        if_cons e1 (mark_var occs x1, mark_var occs x2, e2) e3
    | Loop (init, xs, (occs, step)) ->
        loop (List.map snd init) (mark_vars occs xs) step
    | Iter_over (init, xs, (occs, e)) ->
        iter_over (List.map snd init) (mark_vars occs xs) e
    | Map_over (init, xs, (occs, e)) ->
        map_over (List.map snd init) (mark_vars occs xs) e
    | Create_contract (c, (_, e1), (_, e2), (_, e3)) ->
        let {tparameter; tstorage; parameter_and_storage; body = occs, body} =
          c
        in
        let parameter_and_storage = mark_var occs parameter_and_storage in
        let c = {tparameter; tstorage; parameter_and_storage; body} in
        create_contract c e1 e2 e3
    | e -> {expr = map_expr_f snd e}
  in
  let f e =
    (Analyser.(count_free_vars_f mode_occurrences) (map_expr_f fst e), f e)
  in
  let f e = snd (cata_expr f e) in
  mk "mark_unused" f [preserves bindings_unique; post bindings_used]

let field_access =
  let f = function
    | Prim1 (Car, (x, Stack_ok (Type.T_record (Node (Leaf (Some lbl, _), _)))))
      ->
        proj_field lbl x
    | Prim1 (Cdr, (x, Stack_ok (Type.T_record (Node (_, Leaf (Some lbl, _))))))
      ->
        proj_field lbl x
    | e -> {expr = map_expr_f fst e}
  in
  cata_texpr (fun e _ -> f e)

let field_access = mk_t "field_access" field_access [preserves bindings_unique]

let flatten_matches =
  let f e =
    let occs = count_occs e in
    let extract = function
      | { var = Some var
        ; rhs = {expr = Match_variant ({expr = Var scrutinee'}, clauses')} }
        when var = scrutinee' && String.Map.find_opt var occs = Some 1 ->
          clauses'
      | c -> leaf c
    in
    let f = function
      | Match_variant (scrutinee, clauses) ->
          (* TODO do not transcend type boundaries *)
          {expr = Match_variant (scrutinee, join (map extract clauses))}
      | expr -> {expr}
    in
    cata_expr f e
  in
  mk "flatten_matches" f [preserves bindings_unique]

let fresh_vars st v = List.map (Option.map (fun _ -> fresh st v))

let singleton_or_record = function
  | Leaf (None, x) -> x
  | xs -> {expr = Record xs}

(** A "mostly failing" match is one where at most one branch succeeds. *)
let mostly_fails clauses =
  Option.cata
    0
    size
    (filter (fun {rhs} -> not (Analyser.always_fails rhs)) clauses)
  <= 1

let _let_protect st e f =
  let v = fresh st "p" in
  let_in_var (Some v) e (f (var v))

(** Various simplifications relating to let-expressions.

  let p1 = let p2 = e2; e1; rhs --> let p1 = e1; let p2 = e2; rhs

  let _ = fail; _ --> fail

  let _ = e in rhs --> rhs  (if e cannot fail)

  let x = e in x --> x

  let [x_1; ...; x_n] = [e_1; ...; e_n]; rhs
-->
  let x_1 = e_1; ...; let x_n = e_n; rhs
*)
let flatten_lets =
  let f = function
    | Let_in (p1, {expr = Let_in (p2, e2, e1)}, rhs) ->
        let_in p2 e2 (let_in p1 e1 rhs)
    | Let_in (p1, {expr = Match_record (p2, e2, e1)}, rhs) ->
        match_record p2 e2 (let_in p1 e1 rhs)
    | Match_record (p1, {expr = Let_in (p2, e2, e1)}, rhs) ->
        let_in p2 e2 (match_record p1 e1 rhs)
    | Match_record (p1, {expr = Match_record (p2, e2, e1)}, rhs) ->
        match_record p2 e2 (match_record p1 e1 rhs)
    | Let_in (_, x, _) when Analyser.always_fails x -> x
    | Let_in (P_var None, e, rhs) when not (Analyser.may_fail e) -> rhs
    | Let_in (P_var (Some x), e, {expr = Var x'}) when x = x' -> e
    | Let_in (P_vector xs, {expr = Vector es}, rhs) ->
        lets (List.map2 ~err:"let" (fun x e -> (P_var x, e)) xs es) rhs
    | expr -> {expr}
  in
  mk "flatten_lets" (cata_expr f) [preserves bindings_unique]

(** Deletes bindings to '_'. Linearizes tuple-bindings. Propagates '_'-components in let-bindings below match nodes. *)
let mangle st =
  let f = function
    | Let_in (P_vector [Some x], e1, {expr = Vector [{expr = Var x'}]})
      when x = x' ->
        e1
    | Let_in (P_var (Some x), e1, {expr = Var x'}) when x = x' -> e1
    | Let_in (P_var None, e1, e2) when not (Analyser.may_fail e1) -> e2
    | Let_in (_, ({expr = Prim1_fail _} as fail), _) -> fail
    | Let_in (rp1, {expr = Let_in (rp2, e2, e1)}, e3) ->
        let_in rp2 e2 (let_in rp1 e1 e3)
    | Let_in (rp1, {expr = Match_record (rp2, e2, e1)}, e3) ->
        match_record rp2 e2 (let_in rp1 e1 e3)
    | Match_record (rp1, {expr = Let_in (rp2, e2, e1)}, e3) ->
        let_in rp2 e2 (match_record rp1 e1 e3)
    | Let_in (P_vector [rp1; rp2], {expr = Record (Node (r1, r2))}, e2) ->
        let unwrap = function
          | Leaf (_, x) -> x
          | Node _ as x -> {expr = Record x}
        in
        let_in_var rp1 (unwrap r1) (let_in_var rp2 (unwrap r2) e2)
    | Let_in (rp, {expr = If (c, ({expr = Prim1_fail _} as l), r)}, e2) ->
        if_ c l (let_in rp r e2)
    | Let_in (rp, {expr = If (c, l, ({expr = Prim1_fail _} as r))}, e2) ->
        if_ c (let_in rp l e2) r
    | Let_in (rp, {expr = Match_variant (e1, clauses)}, e2)
      when mostly_fails clauses ->
        (* Pull let into match when at most one branch succeeds. *)
        let mk_clause ({cons; var; rhs} as c) =
          if Analyser.always_fails rhs
          then c
          else {cons; var; rhs = let_in rp rhs e2}
        in
        match_variant e1 (map mk_clause clauses)
    | Let_in (P_vector xs, {expr = Match_variant (e1, clauses)}, e2)
      when List.exists Option.is_none xs ->
        (* Push discarding of unused variables inside clauses. *)
        (* TODO Same thing for Match_record instead of Let_in *)
        let mk_clause ({cons; var; rhs} as c) =
          if Analyser.always_fails rhs
          then c
          else
            let xs = fresh_vars st "s" xs in
            let xs_used =
              vector ((List.map Expr.var) (List.filter_map id xs))
            in
            let rhs = let_in_vector xs rhs xs_used in
            {cons; var; rhs}
        in
        let xs = List.map Option.some (List.somes xs) in
        let_in_vector xs (match_variant e1 (map mk_clause clauses)) e2
    | expr -> {expr}
  in
  mk "mangle" (cata_expr f) [preserves bindings_unique]

let distribute_lets =
  let f = function
    | Let_in
        ( P_var (Some x)
        , {expr = Match_variant (({expr = Var _} as scrutinee), clauses)}
        , { expr =
              Prim2
                ( Pair _
                , ({expr = Prim0 (Nil _toperation)} as l)
                , {expr = Var x'} ) } )
      when x = x' ->
        let f {cons; var; rhs} = {cons; var; rhs = pair None None l rhs} in
        let clauses = Binary_tree.map f clauses in
        match_variant scrutinee clauses
    | Let_in (xs, {expr = Match_variant (scrutinee, clauses)}, e)
      when is_var scrutinee && is_simple e ->
        let f {cons; var; rhs} = {cons; var; rhs = let_in xs rhs e} in
        let clauses = Binary_tree.map f clauses in
        match_variant scrutinee clauses
    | Prim2 (Pair (a1, a2), l, {expr = Match_variant (scrutinee, clauses)})
      when is_var scrutinee && is_simple l ->
        let f {cons; var; rhs} = {cons; var; rhs = pair a1 a2 l rhs} in
        let clauses = Binary_tree.map f clauses in
        match_variant scrutinee clauses
    | expr -> {expr}
  in
  (* Does not preserve 'bindings_unique'. *)
  mk "distribute_lets" (cata_expr f) []

let unfold_getn_and_updaten st =
  let f = function
    | Prim1 (Getn 0, x) -> x
    | Prim1 (Getn 1, x) -> prim1 Car x
    | Prim1 (Getn k, x) when k >= 2 -> prim1 (Getn (k - 2)) (prim1 Cdr x)
    | Prim2 (Updaten 0, r, x) -> let_in_var None x r
    | Prim2 (Updaten 1, r, x) ->
        let x1 = fresh st "x" in
        let x2 = fresh st "x" in
        let_in_vector
          [Some x1; Some x2]
          {expr = Unpair (2, x)}
          (let_in_var None (var x1) (pair None None r (var x2)))
    | Prim2 (Updaten k, r, x) when k >= 2 ->
        let x1 = fresh st "x" in
        let x2 = fresh st "x" in
        let_in_vector
          [Some x1; Some x2]
          {expr = Unpair (2, x)}
          (pair None None (var x1) (prim2 (Updaten (k - 2)) r (var x2)))
    | expr -> {expr}
  in
  mk "unfold_getn_and_updaten" (cata_expr f) [preserves bindings_unique]

(** Folds operations on values known at compile time, e.g.:
  2 + 3
-->
  5

  TODO Properly treat integer overflows.
*)
let fold_constants =
  let f1 e subst =
    match e with
    | Let_in (P_var (Some x), e1, e2) ->
        let e1 = e1 subst in
        let subst =
          if Analyser.may_fail e1 then subst else String.Map.add x e1 subst
        in
        let_in_var (Some x) e1 (e2 subst)
    | e ->
        let e = map_expr_f (fun e -> e subst) e in
        let lookup_var = function
          | {expr = Var x} as e -> String.Map.lookup ~default:e x subst
          | e -> e
        in
        let open Big_int in
        let bool b = lit (Bool b) in
        let nat x = lit (Nat x) in
        let int x = lit (Int x) in
        ( match map_expr_f lookup_var e with
        | Prim1 (Car, {expr = Prim2 (Pair _, x, _)}) -> x
        | Prim1 (Cdr, {expr = Prim2 (Pair _, _, y)}) -> y
        | Prim1 (op, {expr = Lit x}) ->
          ( match (op, x) with
          | Not, Bool x -> bool (not x)
          | Abs, Int x -> nat (abs_big_int x)
          | Eq, Int x -> bool (eq_big_int x zero_big_int)
          | Neq, Int x -> bool (not (eq_big_int x zero_big_int))
          | Lt, Int x -> bool (lt_big_int x zero_big_int)
          | Gt, Int x -> bool (gt_big_int x zero_big_int)
          | Le, Int x -> bool (le_big_int x zero_big_int)
          | Ge, Int x -> bool (ge_big_int x zero_big_int)
          | _ -> {expr = e} )
        | Prim2 (op, {expr = Lit x}, {expr = Lit y}) ->
          ( match (op, x, y) with
          | And, Bool x, Bool y -> bool (x && y)
          | Or, Bool x, Bool y -> bool (x || y)
          | Add, Nat x, Nat y -> nat (add_big_int x y)
          | Add, Int x, Int y -> int (add_big_int x y)
          | Add, Mutez x, Mutez y -> int (add_big_int x y)
          | Mul, Nat x, Nat y -> nat (mult_big_int x y)
          | Mul, Int x, Int y -> int (mult_big_int x y)
          | Compare, Mutez x, Mutez y
           |Compare, Nat x, Nat y
           |Compare, Int x, Int y ->
              int (big_int_of_int (compare_big_int x y))
          | _ -> {expr = e} )
        | Unpair (2, {expr = Prim2 (Pair _, x, y)}) -> vector [x; y]
        | If ({expr = Lit (Bool b)}, x, y) -> if b then x else y
        | Iter_over
            ( {expr = List (_, [x0]) | Prim2 (Cons, x0, {expr = Prim0 (Nil _)})}
              :: init
            , xs
            , body ) ->
            let f x e = Option.map (fun x -> (x, e)) x in
            let s = List.somes (List.map2 f xs (x0 :: init)) in
            Expr.substitute s body
        | _ -> {expr = e} )
  in
  let f2 = function
    | Prim1 (Getn 0, x) -> x
    | Prim1 (Getn 1, {expr = Prim2 (Pair _, x, _)}) -> x
    | Prim1 (Getn k, {expr = Prim2 (Pair _, _, y)}) when k >= 2 ->
        prim1 (Getn (k - 2)) y
    | Prim2 (Updaten 0, r, x) -> let_in_var None x r
    | Prim2 (Updaten 1, r, {expr = Prim2 (Pair (a1, a2), x1, x2)}) ->
        let_in_var None x1 (pair a1 a2 r x2)
    | Prim2 (Updaten k, r, {expr = Prim2 (Pair (a1, a2), x1, x2)}) when k >= 2
      ->
        pair a1 a2 x1 (prim2 (Updaten (k - 2)) r x2)
    | expr -> {expr}
  in
  let f1 e = cata_expr f1 e String.Map.empty in
  let f2 e = cata_expr f2 e in
  mk "fold_constants" (fun x -> f2 (f1 x)) [preserves bindings_unique]

let pp_tree_skel x =
  print
    (fun ppf -> Format.fprintf ppf " | ")
    (fun ppf _ -> Format.fprintf ppf "_")
    x

let infer_constructors =
  let open Type in
  let f = function
    | Match_variant ((scrutinee, Stack_ok (T_variant r_scrutinee)), clauses) ->
      ( match matches clauses r_scrutinee with
      | None ->
          failwith
            (Format.asprintf
               "infer_constructors:\n  <%a>\n  (%a)"
               (print_row ~sep:";")
               r_scrutinee
               pp_tree_skel
               clauses)
      | Some clauses ->
          let put_cons = function
            | {cons = None; var; rhs = rhs, _}, Leaf (Some cons, _) ->
                {cons = Some cons; var; rhs}
            | {cons; var; rhs = rhs, _}, _ -> {cons; var; rhs}
          in
          let clauses = map put_cons clauses in
          {expr = Match_variant (scrutinee, clauses)} )
    | e -> {expr = map_expr_f fst e}
  in
  let f = cata_texpr (fun e _ -> f e) in
  mk_t "infer_constructors" f [preserves bindings_unique]

let cons x xs = x :: xs

(** Returns the right-hand side of a chain of lets, a list of
   variables bound on the way, and a function that puts the same chain
   onto any expression. For example, on

     let x1 = e1
     let x2 = e2
     rhs

   it returns ([x1,x2], rhs, close), where 'close' is
   (fun <e> -> let x1 = e1; let x2 = 2; <e>).

   Furthermore, it traverses comments and "mostly failing" matches and
   conditionals. A mostly failing conditional is one that fails on all
   except for one clause. *)
let lens_rhs =
  let rec lens bound close {expr} =
    let lens x = lens (List.somes x @ bound) in
    match expr with
    | Comment (xs, e) ->
        let close e' = close (comment xs e') in
        lens [] close e
    | Let_in (p, e1, e2) ->
        let close e2' = close (let_in p e1 e2') in
        let xs =
          match p with
          | P_var x -> [x]
          | P_vector xs -> xs
        in
        lens xs close e2
    | Match_record (rp, e1, e2) ->
        let close e2' = close (match_record rp e1 e2') in
        lens (Binary_tree.to_list rp) close e2
    | Match_variant (e, clauses) ->
      ( match
          List.filter
            (fun {rhs} -> not (Analyser.always_fails rhs))
            (to_list clauses)
        with
      | [{var; rhs}] ->
          let close rhs' =
            let f ({cons; var; rhs} as c) =
              if Analyser.always_fails rhs then c else {cons; var; rhs = rhs'}
            in
            close (match_variant e (map f clauses))
          in
          lens [var] close rhs
      | _ -> (bound, {expr}, close) )
    | If (s, l, r) ->
      ( match (Analyser.always_fails l, Analyser.always_fails r) with
      | true, false -> lens [] (fun r -> close (if_ s l r)) r
      | false, true -> lens [] (fun l -> close (if_ s l r)) l
      | _ -> (bound, {expr}, close) )
    | If_none (s, xr, l, r) ->
      ( match (Analyser.always_fails l, Analyser.always_fails r) with
      | true, false -> lens [] (fun r -> close (if_none s l (xr, r))) r
      | false, true -> lens [xr] (fun l -> close (if_none s l (xr, r))) l
      | _ -> (bound, {expr}, close) )
    | If_left (s, xl, l, xr, r) ->
      ( match (Analyser.always_fails l, Analyser.always_fails r) with
      | true, false -> lens [xr] (fun r -> close (if_left s (xl, l) (xr, r))) r
      | false, true -> lens [xl] (fun l -> close (if_left s (xl, l) (xr, r))) l
      | _ -> (bound, {expr}, close) )
    | If_cons (s, xl1, xl2, l, r) ->
      ( match (Analyser.always_fails l, Analyser.always_fails r) with
      | true, false -> lens [] (fun r -> close (if_cons s (xl1, xl2, l) r)) r
      | false, true ->
          lens [xl1; xl2] (fun l -> close (if_cons s (xl1, xl2, l) r)) l
      | _ -> (bound, {expr}, close) )
    | _ -> (bound, {expr}, close)
  in
  fun e -> lens [] id e

let apply_deep f x =
  let _, focus, close = lens_rhs x in
  close (f focus)

(** Factor any parts of the loop state that are invariant, e.g.
  map [i1; i2; i3] step x1; x2; x3 -> [x1 + 1; x2; x2 * x3]
-->
  let [r1; r3] = map [i1; i3] step x1; x3 -> [x1 + 1; i2 * x3]
  [r1; i2; r3]

Note that only pure parts of the state can be factored out (lest
we risk changing the evaluation order).

Also:
  map_over [i1; i2; i3] (fun [x1; x2; x3] -> vector [x1 + 1; e2; 2 * x3])
-->
  (same result as above)
*)
let factor_invariant_loop_state st =
  let name = "factor_invariant_loop_state" in
  let handle_loop init xs ys =
    let vars =
      let f i x y =
        match (i, x, y) with
        | i, Some x, {expr = Var y'} when y' = x || (equal_expr y i && is_pure i)
          ->
            (None, None, None, None, i, Some (x, i))
        | i, x, y ->
            let r = fresh st "r" in
            (Some (Some r), Some i, Some x, Some y, var r, None)
      in
      List.map3 ~err:name f init xs ys
    in
    let rs = List.map_some (fun (r, _, _, _, _, _) -> r) vars in
    let init = List.map_some (fun (_, i, _, _, _, _) -> i) vars in
    let xs' = List.map_some (fun (_, _, x, _, _, _) -> x) vars in
    let ys = List.map_some (fun (_, _, _, y, _, _) -> y) vars in
    let ris = List.map (fun (_, _, _, _, ri, _) -> ri) vars in
    let s = List.map_some (fun (_, _, _, _, _, s) -> s) vars in
    if List.length xs' < List.length xs
    then Some (rs, init, xs', ys, ris, s)
    else None
  in
  let f = function
    | Map_over (i0 :: init, x0 :: xs, body) as expr ->
      ( match lens_rhs body with
      | _bound, {expr = Vector (y0 :: ys)}, close_body ->
        ( match handle_loop init xs ys with
        | None -> {expr}
        | Some (rs, init, xs, ys, ris, s) ->
            let r0 = fresh st "r" in
            let body = close_body (vector (y0 :: ys)) in
            let body = Expr.substitute s body in
            let loop = map_over (i0 :: init) (x0 :: xs) body in
            let_in_vector (Some r0 :: rs) loop (vector (var r0 :: ris)) )
      | _ -> {expr} )
    | Iter_over (i0 :: init, x0 :: xs, body) as expr ->
      ( match lens_rhs body with
      | _bound, {expr = Vector ys}, close_body ->
        ( match handle_loop init xs ys with
        | None -> {expr}
        | Some (rs, init, xs, ys, ris, s) ->
            let body = close_body (vector ys) in
            let body = Expr.substitute s body in
            let loop = iter_over (i0 :: init) (x0 :: xs) body in
            let_in_vector rs loop (vector ris) )
      | _ -> {expr} )
    | Loop (i0 :: init, xs, body) as expr ->
      ( match lens_rhs body with
      | _bound, {expr = Vector (y0 :: ys)}, close_body ->
        ( match handle_loop init xs ys with
        | None -> {expr}
        | Some (rs, init, xs, ys, ris, s) ->
            let body = close_body (vector (y0 :: ys)) in
            let body = Expr.substitute s body in
            let loop = loop (i0 :: init) xs body in
            let_in_vector rs loop (vector ris) )
      | _ -> {expr} )
    | expr -> {expr}
  in
  mk name (cata_expr f) [preserves bindings_unique]

(* Remove unused parts from the loop state. *)
let unused_loop_state st =
  let name = "unused_loop_state" in
  let f = function
    | Let_in
        (P_vector (r0 :: rs), {expr = Map_over (i0 :: init, x0 :: xs, step)}, e)
      as expr ->
        let f r i x =
          match (r, x) with
          | None, None when is_pure i -> (None, None, None, None, None)
          | r, x ->
              let t = fresh st "t" in
              (Some r, Some i, Some x, Some t, Some (var t))
        in
        let vars = List.map3 ~err:name f rs init xs in
        let rs = r0 :: List.map_some (fun (r, _, _, _, _) -> r) vars in
        let init = i0 :: List.map_some (fun (_, i, _, _, _) -> i) vars in
        let t0 = fresh st "t" in
        let xs = x0 :: List.map_some (fun (_, _, x, _, _) -> x) vars in
        let ts = Some t0 :: List.map (fun (_, _, _, t, _) -> t) vars in
        let us = var t0 :: List.map_some (fun (_, _, _, _, u) -> u) vars in
        if List.(length xs < length vars)
        then
          let_in_vector
            rs
            (map_over init xs (let_in_vector ts step (vector us)))
            e
        else {expr}
    | Let_in (P_vector rs, {expr = Iter_over (i0 :: init, x0 :: xs, step)}, e)
      as expr ->
        let f r i x =
          match (r, x) with
          | None, None when is_pure i -> (None, None, None, None, None)
          | r, x ->
              let t = fresh st "t" in
              (Some r, Some i, Some x, Some t, Some (var t))
        in
        let vars = List.map3 ~err:name f rs init xs in
        let rs = List.map_some (fun (r, _, _, _, _) -> r) vars in
        let init = i0 :: List.map_some (fun (_, i, _, _, _) -> i) vars in
        let t0 = fresh st "t" in
        let xs = x0 :: List.map_some (fun (_, _, x, _, _) -> x) vars in
        let ts = Some t0 :: List.map (fun (_, _, _, t, _) -> t) vars in
        let us = var t0 :: List.map_some (fun (_, _, _, _, u) -> u) vars in
        if List.(length xs < length vars)
        then
          let_in_vector
            rs
            (iter_over init xs (let_in_vector ts step (vector us)))
            e
        else {expr}
    | Let_in (P_vector rs, {expr = Loop (i0 :: init, xs, step)}, e) as expr ->
        let f r i x =
          match (r, x) with
          | None, None when is_pure i -> (None, None, None, None, None)
          | r, x ->
              let t = fresh st "t" in
              (Some r, Some i, Some x, Some t, Some (var t))
        in
        let vars = List.map3 ~err:name f rs init xs in
        let rs = List.map_some (fun (r, _, _, _, _) -> r) vars in
        let init = i0 :: List.map_some (fun (_, i, _, _, _) -> i) vars in
        let t0 = fresh st "t" in
        let xs = List.map_some (fun (_, _, x, _, _) -> x) vars in
        let ts = Some t0 :: List.map (fun (_, _, _, t, _) -> t) vars in
        let us = var t0 :: List.map_some (fun (_, _, _, _, u) -> u) vars in
        if List.(length xs < length vars)
        then
          let_in_vector rs (loop init xs (let_in_vector ts step (vector us))) e
        else {expr}
    | expr -> {expr}
  in
  mk name (cata_expr f) [preserves bindings_unique]

let unify_fields x y =
  match (x, y) with
  | Some x, Some y -> if x = y then Some x else failwith "unify_fields"
  | x, None -> x
  | None, y -> y

let common_parts x y =
  common x y
  |> map (function
         | Leaf (fld_x, Some x), Leaf (fld_y, Some y) when equal_expr x y ->
             Leaf (unify_fields fld_x fld_y, Some x)
         | Leaf (fld_x, _), Leaf (fld_y, _) ->
             Leaf (unify_fields fld_x fld_y, None)
         | _ -> Leaf (None, None))
  |> join

let prune pat x =
  matches pat x
  |> Option.of_some ~msg:"prune"
  |> map_some (function p, x -> if p then Some x else None)
  |> Option.map join

let factorise_clauses st =
  let f = function
    | Match_variant (scrutinee, clauses) as expr ->
        let open Option.Monad_syntax in
        (let* results =
           (* TODO use lens_rhs *)
           TO.map
             (function
               | {rhs = {expr = Record r}} -> Some (Some r)
               | {rhs = {expr = Prim1_fail _}} -> Some None
               | _ -> None)
             clauses
         in
         let* results = map_some id results in
         let* () = if mostly_fails clauses then None else Some () in
         (* This avoids pulling out bound variables. TODO Be more precise. *)
         let shared =
           fold1 common_parts (map (map (map_snd Option.some)) results)
         in
         let b = exists (fun x -> Option.is_some (snd x)) shared in
         let* _ = if b then Some () else None in
         let open Either in
         let out =
           shared
           |> map (function
                  | _, None -> Left (fresh st "o")
                  | fld, Some x -> Right (fld, x))
         in
         let x =
           map_some of_left out |> Option.cata (Leaf None) (map Option.some)
         in
         let clauses =
           let pat = map Either.is_left out in
           let prune_rhs = function
             | {expr = Record r} ->
                 singleton_or_record
                   (prune pat r |> Option.default (Leaf (None, unit)))
             | {expr = Prim1_fail _} as rhs -> rhs
             | _ -> assert false
           in
           clauses |> map (map_match_clause prune_rhs)
         in
         let out =
           out
           |> map (function
                  | Left x -> (None, var x)
                  | Right e -> e)
         in
         Some
           (match_record
              x
              (match_variant scrutinee clauses)
              (singleton_or_record out)))
        |> Option.default {expr}
    | expr -> {expr}
  in
  mk "factorise_clauses" (cata_expr f) [preserves bindings_unique]

let reify_booleans =
  let f = function
    | Match_variant
        ( x
        , Node
            ( Leaf {cons = Some "True"; var = None; rhs = r1}
            , Leaf {cons = Some "False"; var = None; rhs = r2} ) ) as expr ->
      ( match () with
      | _ when equal_expr r2 false_ -> prim2 And x r1
      | _ when equal_expr r1 true_ -> prim2 Or x r1
      | _ -> {expr} )
    | expr -> {expr}
  in
  mk "reify_booleans" (cata_expr f) [preserves bindings_unique]

let binarize_matches st =
  let f = function
    | Match_variant (s, Node ((Node _ as l), r)) ->
        let ls = fresh st "l" in
        let rhs = {expr = Match_variant ({expr = Var ls}, l)} in
        let l = Leaf {cons = None; var = Some ls; rhs} in
        match_variant s (Node (l, r))
    | Match_variant (s, Node (l, (Node _ as r))) ->
        let rs = fresh st "r" in
        let rhs = {expr = Match_variant ({expr = Var rs}, r)} in
        let r = Leaf {cons = None; var = Some rs; rhs} in
        match_variant s (Node (l, r))
    | expr -> {expr}
  in
  mk "binarize_matches" (cata_expr f) [preserves bindings_unique]

let two_clauses c1 c2 = Binary_tree.Node (Leaf c1, Leaf c2)

let unfold_ifs =
  let f = function
    | If_left (scrutinee, xl, l, xr, r) ->
        match_variant
          scrutinee
          (two_clauses
             {cons = None; var = xl; rhs = l}
             {cons = None; var = xr; rhs = r})
    | If_none (scrutinee, xr, l, r) ->
        match_variant
          scrutinee
          (two_clauses
             {cons = Some "None"; var = None; rhs = l}
             {cons = Some "Some"; var = xr; rhs = r})
    | expr -> {expr}
  in
  mk "unfold_ifs" (cata_expr f) [preserves bindings_unique]

(** Push bindings into mostly failing ifs, i.e. for failing b:
  let x = if c then a else b in e
-->
  if c then let x = a in e else b
*)
let nest_ifs =
  let f = function
    | Let_in (x, {expr = If (c, a, b)}, e) when Analyser.always_fails b ->
        {expr = If (c, {expr = Let_in (x, a, e)}, b)}
    | expr -> {expr}
  in
  mk "nest_ifs" (cata_expr f) [preserves bindings_unique]

(** Factorise Failwith expressions under conditionals:
  if c (let ... in failwith x) (let ... in failwith y)
-->
  failwith (if c (let ... in x) (let ... in y))
*)
let factorise_failwith =
  let f = function
    | If (c, x, y) as expr ->
        let _, x, close_x = lens_rhs x in
        let _, y, close_y = lens_rhs y in
        ( match (x.expr, y.expr) with
        | Prim1_fail (Failwith, x), Prim1_fail (Failwith, y) ->
            let cond = if_ c (close_x x) (close_y y) in
            {expr = Prim1_fail (Failwith, cond)}
        | _ -> {expr} )
    | If_left (c, l, x, r, y) as expr ->
        let _, x, close_x = lens_rhs x in
        let _, y, close_y = lens_rhs y in
        ( match (x.expr, y.expr) with
        | Prim1_fail (Failwith, x), Prim1_fail (Failwith, y) ->
            let cond = if_left c (l, close_x x) (r, close_y y) in
            {expr = Prim1_fail (Failwith, cond)}
        | _ -> {expr} )
    | expr -> {expr}
  in
  mk "factorise_failwith" (cata_expr f) [preserves bindings_unique]

(** Pull bindings out of mostly failing ifs, i.e. for failing b:
  if c then let x = a in e else b
-->
  let x = if c then a else b in e
*)
let flatten_ifs =
  let f = function
    | If (c, {expr = Let_in (x, a, e)}, b) when Analyser.always_fails b ->
        {expr = Let_in (x, {expr = If (c, a, b)}, e)}
    | expr -> {expr}
  in
  mk "flatten_ifs" (cata_expr f) [preserves bindings_unique]

let unvectorize_conds st =
  let un1 e =
    match e.expr with
    | Prim1_fail _ -> e
    | _ ->
        let s = fresh st "s" in
        let_in_vector [Some s] e (var s)
  in
  let f e tys =
    match tys with
    | Stack_ok (T_vector [_]) ->
      ( match e with
      | If ((c, _), (l, _), (r, _)) -> vector [if_ c (un1 l) (un1 r)]
      | Match_variant ((scrutinee, _), clauses) ->
          let clauses =
            Binary_tree.map (map_match_clause (fun (b, _) -> un1 b)) clauses
          in
          vector [match_variant scrutinee clauses]
      | _ -> {expr = map_expr_f fst e} )
    | _ -> {expr = map_expr_f fst e}
  in
  mk_t "unvectorize_conds" (cata_texpr f) [preserves bindings_unique]

let single_lets st =
  let f = function
    | Match_record ((Node _ as rp), e1, e2) ->
        let x = fresh st "x" in
        let rec mk_vars v = function
          | Leaf n -> [(Option.default (fresh st "x") n, v)]
          | Node (x, y) -> mk_vars (prim1 Car v) x @ mk_vars (prim1 Cdr v) y
        in
        let bindings = (x, e1) :: mk_vars (var x) rp in
        List.fold_right (fun (x, e) r -> let_in_var (Some x) e r) bindings e2
    | expr -> {expr}
  in
  mk "single_lets" (cata_expr f) [preserves bindings_unique]

let binarize_records =
  let f = function
    | Record xs ->
        let pair a b = Expr.tuple [a; b] in
        Binary_tree.cata snd pair xs
    | expr -> {expr}
  in
  mk "binarize_records" (cata_expr f) [preserves bindings_unique]

let binarize_projs =
  let f e _t =
    match e with
    | Proj_field (fld, (e, Stack_ok (T_record r))) ->
        let rec proj r p other =
          match r with
          | Leaf (Some n, _) when fld = n -> p
          | Leaf _ -> other ()
          | Node (x, y) ->
              proj
                x
                {expr = Prim1 (Car, p)}
                (fun () -> proj y {expr = Prim1 (Cdr, p)} other)
        in
        proj r e (fun () -> assert false)
    | e -> {expr = map_expr_f fst e}
  in
  mk_t "binarize_projs" (cata_texpr f) [preserves bindings_unique]

let name_all st =
  let name_var = function
    | None -> Some (fresh st "m")
    | x -> x
  in
  let name_rp = Binary_tree.map name_var in
  let name_pattern = function
    | P_var x -> P_var (name_var x)
    | P_vector x -> P_vector (List.map name_var x)
  in
  let f = function
    | Let_in (p, e1, e2) -> {expr = Let_in (name_pattern p, e1, e2)}
    | Match_variant (scrutinee, clauses) ->
        let f c = {c with var = name_var c.var} in
        {expr = Match_variant (scrutinee, map f clauses)}
    | Match_record (rp, e1, e2) -> {expr = Match_record (name_rp rp, e1, e2)}
    | Lambda (rp, a, b, e) -> {expr = Lambda (name_var rp, a, b, e)}
    | Loop (init, xs, step) -> {expr = Loop (init, List.map name_var xs, step)}
    | expr -> {expr}
  in
  mk "name_all" (cata_expr f) [preserves bindings_unique]

let subst_fresh st xs =
  xs |> String.Set.elements |> List.map (fun x -> (x, fresh st (x ^ "d")))

let build_dup_chain st x xs =
  let open List in
  let tmps = init (length xs - 2) (fun _ -> fresh st "tmp") in
  let xs', xs_last = unsnoc xs in
  let ys = tmps @ [xs_last] in
  let zs = x :: tmps in
  map3
    ~err:"build_dup_chain"
    (fun x y z -> (P_vector [Some x; Some y], dup 1 [var z]))
    xs'
    ys
    zs

(* Converts a list of replacements into let-bindings and puts them in
   front of the given expression. *)
let make_dups st bs e =
  let chains =
    bs
    |> List.map (map_snd (fun x -> [x]))
    |> map_of_list ( @ )
    |> String.Map.bindings
    |> List.map (fun (x, xs) -> build_dup_chain st x xs)
    |> List.concat
  in
  lets chains e

(* Add elements to the end of a vector. *)
let extend st xs len e =
  if len = 1
  then vector (e :: xs)
  else
    match e.expr with
    | Vector es -> vector (es @ xs)
    | _ ->
        let es = List.init len (fun _ -> fresh st "e") in
        let_in_vector
          (List.map Option.some es)
          e
          (vector (List.map var es @ xs))

let extend st xs len e = apply_deep (extend st xs len) e

(* List.take on vectors *)
let take_vector st n len xs =
  if n = len
  then xs
  else
    let es = List.init n (fun _ -> fresh st "t") in
    let ignored = List.init (len - n) (fun _ -> None) in
    let_in_vector
      (List.map Option.some es @ ignored)
      xs
      (vector (List.map var (List.take n es)))

let set_of_map m =
  let f (k, v) =
    assert (v >= 0);
    if v > 0 then Some k else None
  in
  String.Set.of_list (List.filter_map f (String.Map.bindings m))

(* Modify loops so that their bodies don't refer to the outer scope. *)
let close_loops st =
  let f e =
    let open Analyser in
    let open String.Set in
    let generic () =
      ( count_free_vars_f mode_occurrences (map_expr_f fst e)
      , {expr = map_expr_f snd e} )
    in
    match e with
    | Loop (init, xs, (occs_step, step)) ->
        let n = List.length xs in
        let outer =
          let xs = of_list (List.somes xs) in
          elements (diff (set_of_map occs_step) xs)
        in
        if outer = []
        then generic ()
        else
          let outer_fresh = List.map (fun x -> (x, fresh st "v")) outer in
          let init = List.map var outer @ List.map snd init in
          let xs = xs @ List.map (fun (_, v) -> Option.some v) outer_fresh in
          let step =
            let sigma = List.map (fun (x, v) -> (x, var v)) outer_fresh in
            substitute sigma step
          in
          let step =
            extend st (List.map (fun (_, v) -> var v) outer_fresh) (n + 1) step
          in
          let e = take_vector st n (List.length xs) (loop init xs step) in
          (count_free_vars mode_occurrences e, e)
    | _ -> generic ()
  in
  let f e = snd (cata_expr f e) in
  mk "close_loops" f [preserves bindings_unique; post loops_closed]

let drop_vars xs =
  List.fold_right
    (fun x e -> let_in_var None (var x) e)
    (String.Set.elements xs)

let without fs xs =
  let open String.Set in
  diff fs (of_list (List.somes xs))

(** Insert 'let _ = x in ...' for unused variables. *)
let drop_unused =
  let open String.Set in
  let cond bound_l (frees_l, l) bound_r (frees_r, r) =
    let xsl = of_list (List.somes bound_l) in
    let xsr = of_list (List.somes bound_r) in
    let dl = diff (union xsl (diff frees_r xsr)) frees_l in
    let dr = diff (union xsr (diff frees_l xsl)) frees_r in
    (drop_vars dl l, drop_vars dr r)
  in
  let bound_pattern = function
    | P_var x -> Option.cata empty singleton x
    | P_vector xs -> of_list (List.somes xs)
  in
  let f = function
    | Match_variant _ -> assert false
    | Let_in (p, (_, e1), (frees2, e2)) ->
        let d = diff (bound_pattern p) frees2 in
        Let_in (p, e1, drop_vars d e2)
    | Lambda (x, t1, t2, (frees, body)) ->
        let d = diff (Option.cata empty singleton x) frees in
        Lambda (x, t1, t2, drop_vars d body)
    | Loop (init, rp, (frees, step)) ->
        let d = diff (of_list (List.somes rp)) frees in
        Loop (List.map snd init, rp, drop_vars d step)
    | Iter_over (init, xs, (frees, body)) ->
        let d = diff (of_list (List.somes xs)) frees in
        Iter_over (List.map snd init, xs, drop_vars d body)
    | If ((_, s), l, r) ->
        let l, r = cond [] l [] r in
        If (s, l, r)
    | If_none ((_, s), xr, l, r) ->
        let l, r = cond [] l [xr] r in
        If_none (s, xr, l, r)
    | If_left ((_, s), xl, l, xr, r) ->
        let l, r = cond [xl] l [xr] r in
        If_left (s, xl, l, xr, r)
    | If_cons ((_, s), xl, xsl, l, r) ->
        let l, r = cond [xl; xsl] l [] r in
        If_cons (s, xl, xsl, l, r)
    | ( Var _ | Map_over _ | Match_record _ | Create_contract _ | Lit _
      | Prim0 _ | Prim1 _ | Prim1_fail _ | Prim2 _ | Prim3 _ | Proj_field _
      | Stack_op _ | Record _ | Comment _ | Variant _ | List _ | Set _ | Map _
      | Vector _ | Nth _ | Unpair _ | Record_of_tree _ ) as e ->
        map_expr_f snd e
  in
  let f x = (Analyser.free_vars_f (map_expr_f fst x), {expr = f x}) in
  let f e =
    let fr, r = cata_expr f e in
    if String.Set.mem "ps0" fr
    then r
    else drop_vars (String.Set.singleton "ps0") r
  in
  (* TODO precondition: loops have closed bodies *)
  mk "drop_unused" f [preserves bindings_unique]

(* Returns the expression wrapped in the appropriate chain
   of DUPs. *)
let make_unique st =
  let open String.Set in
  let cond mk (fs, s) xsl (fl, l) xsr (fr, r) subst =
    (* Variables that are used both in the scrutinee and at least one
       of the branches: *)
    let dups = inter fs (union (without fl xsl) (without fr xsr)) in
    let subst_scrut = subst_fresh st dups in
    let subst_branches = subst_fresh st dups in
    let s = s (subst @ subst_scrut) in
    let l = l (subst @ subst_branches) in
    let r = r (subst @ subst_branches) in
    let expr = mk s l r in
    make_dups st (subst_scrut @ subst_branches) {expr}
  in
  let f = function
    | Var x ->
        fun subst -> {expr = Var (Option.default x (List.assoc_opt x subst))}
    | Match_variant ((scrutinee_frees, scrutinee), clauses) ->
        fun subst ->
          let rhs_frees =
            clauses
            |> Binary_tree.map (fun {var; rhs = frees, _} ->
                   Option.cata id remove var frees)
            |> Binary_tree.fold union empty
          in
          let dups = inter scrutinee_frees rhs_frees in
          let subst_scrut = subst_fresh st dups in
          let subst_rhs = subst_fresh st dups in
          let scrutinee = scrutinee (subst @ subst_scrut) in
          let clauses =
            Binary_tree.map
              (fun {cons; var; rhs = _, e} ->
                {cons; var; rhs = e (subst @ subst_rhs)})
              clauses
          in
          let expr = Match_variant (scrutinee, clauses) in
          make_dups st (subst_scrut @ subst_rhs) {expr}
    | If (s, l, r) ->
        let mk s l r = If (s, l, r) in
        cond mk s [] l [] r
    | If_none (s, xr, l, r) ->
        let mk s l r = If_none (s, xr, l, r) in
        cond mk s [] l [xr] r
    | If_left (s, xl, l, xr, r) ->
        let mk s l r = If_left (s, xl, l, xr, r) in
        cond mk s [xl] l [xr] r
    | If_cons (s, xl, xsl, l, r) ->
        let mk s l r = If_cons (s, xl, xsl, l, r) in
        cond mk s [xl; xsl] l [] r
    | ( Loop _ | Map_over _ | Iter_over _ | Let_in _ | Match_record _
      | Create_contract _ | Lambda _ | Lit _ | Prim0 _ | Prim1 _ | Prim1_fail _
      | Prim2 _ | Prim3 _ | Proj_field _ | Stack_op _ | Record _ | Comment _
      | Variant _ | List _ | Set _ | Map _ | Vector _ | Nth _ | Unpair _
      | Record_of_tree _ ) as e ->
        fun subst ->
          (* Generates and applies replacements for variables that occur both in
             e (i.e. in occs) and elsewhere (i.e. in all_occs). *)
          let all_occs =
            map_expr_f (fun (occs, _) -> String.map_of_set (fun _ -> 1) occs) e
            |> fold_expr_f sum_map String.Map.empty
            |> String.Map.filter (fun _ v -> v > 1)
            |> set_of_map
          in
          let dup_expr occs = subst_fresh st (String.Set.inter all_occs occs) in
          let e = map_expr_f (fun (fs, e) -> (dup_expr fs, e)) e in
          let bs = fold_expr_f ( @ ) [] (map_expr_f fst e) in
          let expr = map_expr_f (fun (dups, e) -> e (subst @ dups)) e in
          make_dups st bs {expr}
  in
  let f x = (Analyser.free_vars_f (map_expr_f fst x), f x) in
  let f e = snd (cata_expr f e) [] in
  mk "make_unique" f [preserves bindings_unique]

let _linearize st =
  post
    is_linear
    (Sequence [Fixpoint (make_unique st); drop_unused; close_loops st])

let _linearize_tuples st =
  let f = function
    | Match_record (Node (rp1, rp2), e1, e2) ->
        let x = fresh st "t" in
        let_in_var
          (Some x)
          e1
          (match_record
             rp1
             (prim1 Car (var x))
             (match_record rp2 (prim1 Cdr (var x)) e2))
    | Record t -> Binary_tree.cata snd (pair None None) t
    | expr -> {expr}
  in
  mk "linearize_tuples" (cata_expr f) [preserves bindings_unique]

(** Unfold stack operations, e.g.:
   let x1; x2 = swap [e1; e2]
-->
   let x2; x1 = [e1; e2]

If 'full' is not specified, only unfold if there is a '_' on the lhs
(for certain operations, in certain positions).
*)
let unfold_stack_ops ~full =
  let err = "unfold_stack_ops" in
  let f expr =
    match expr with
    | Let_in (P_vector (None :: xs), {expr = Stack_op (Dup _, es)}, rhs) ->
        let_in_vector xs (vector es) rhs
    | Let_in (P_vector (x0 :: xs), {expr = Stack_op (Dup n, es)}, rhs) ->
      ( match List.split_at (n - 1) xs with
      | hi, x :: lo ->
        ( match x with
        | None -> let_in_vector (hi @ (x0 :: lo)) (vector es) rhs
        | Some x when full ->
            let_in_vector xs (vector es) (let_in_var x0 (var x) rhs)
        | _ -> {expr} )
      | _, [] -> assert false )
    | Let_in (P_vector xs, {expr = Stack_op (Dig n, es)}, rhs)
      when full || List.exists Option.is_none xs ->
      ( match List.split_at ~err (n + 1) xs with
      | x0 :: xs1, xs2 -> let_in_vector (xs1 @ (x0 :: xs2)) (vector es) rhs
      | _ -> assert false )
    | Let_in (P_vector xs, {expr = Stack_op (Dug n, es)}, rhs)
      when full || List.exists Option.is_none xs ->
      ( match List.split_at ~err n xs with
      | xs1, x :: xs2 -> let_in_vector ((x :: xs1) @ xs2) (vector es) rhs
      | _ -> assert false )
    | Let_in
        (P_vector (x1 :: x2 :: xs as xxs), {expr = Stack_op (Swap, es)}, rhs)
      when full || List.exists Option.is_none xxs ->
        let_in_vector (x2 :: x1 :: xs) (vector es) rhs
    | Let_in (P_vector xs, {expr = Stack_op (Drop n, es)}, rhs) ->
        let_in_vector (List.(replicate n None) @ xs) (vector es) rhs
    | expr -> {expr}
  in
  mk "unfold_stack_ops" (cata_expr f) [pre_post bindings_unique]

(** Chop the invariant part of stack operations, e.g.:
  let [x1, x2, x3, x4] = swap [e1, e2, e3, e4] in ...
-->
  let [x1, x2] = swap [e1, e2] in
  let [x3, x4] = [e3, e4] in ...
 *)
let chop_stack_ops =
  let err = "chop_stack_ops" in
  let f = function
    | Let_in (P_vector xs, {expr = Stack_op (op, es)}, e) as expr ->
        let a_in, a_out =
          match op with
          | Swap -> (2, 2)
          | Dup n -> (n, n + 1)
          | Dug n -> (n + 1, n + 1)
          | Dig n -> (n + 1, n + 1)
          | Drop n -> (n, 0)
        in
        if List.length xs > a_out
        then
          let es_hi, es_lo = List.split_at ~err a_in es in
          let xs_hi, xs_lo = List.split_at ~err a_out xs in
          let_in_vector
            xs_hi
            (stack_op op es_hi)
            (let_in_vector xs_lo (vector es_lo) e)
        else {expr}
    | expr -> {expr}
  in
  mk "chop_stack_ops" (cata_expr f) [pre_post bindings_unique]

let factor_ifs st =
  let f expr =
    let handle st cond c (x_var0, x_rhs0) (y_var0, y_rhs0) =
      let x_bound, x_rhs, close_x = lens_rhs x_rhs0 in
      let y_bound, y_rhs, close_y = lens_rhs y_rhs0 in
      let x_bound = Option.cata id cons x_var0 x_bound in
      let y_bound = Option.cata id cons y_var0 y_bound in
      let can_factor xs = function
        | {expr = Var x} -> not (List.mem x xs)
        | {expr = Lit _} -> true
        | _ -> false
      in
      let pull1 bound x =
        if can_factor bound x
        then (None, None, x)
        else
          let r = fresh st "r" in
          (Some (Some r), Some x, var r)
      in
      let pull2 x y =
        if equal_expr x y && can_factor x_bound x && can_factor y_bound y
        then (None, None, None, x)
        else
          let r = fresh st "r" in
          (Some (Some r), Some x, Some y, var r)
      in
      match (x_rhs, y_rhs) with
      | {expr = Vector xs}, y when Analyser.always_fails y ->
          let vars = List.map (pull1 x_bound) xs in
          let rs = List.map_some (fun (r, _, _) -> r) vars in
          let xs = List.map_some (fun (_, x, _) -> x) vars in
          let rcs = List.map (fun (_, _, rc) -> rc) vars in
          let x = close_x (vector xs) in
          if List.length rs < List.length vars
          then let_in_vector rs (cond c x y_rhs0) (vector rcs)
          else {expr}
      | x, {expr = Vector ys} when Analyser.always_fails x ->
          let vars = List.map (pull1 y_bound) ys in
          let rs = List.map_some (fun (r, _, _) -> r) vars in
          let ys = List.map_some (fun (_, y, _) -> y) vars in
          let rcs = List.map (fun (_, _, rc) -> rc) vars in
          let y = close_y (vector ys) in
          if List.length rs < List.length vars
          then let_in_vector rs (cond c x_rhs0 y) (vector rcs)
          else {expr}
      | {expr = Vector xs}, {expr = Vector ys} ->
          let vars = List.map2 ~err:"common" pull2 xs ys in
          let rs = List.map_some (fun (r, _, _, _) -> r) vars in
          let xs = List.map_some (fun (_, x, _, _) -> x) vars in
          let ys = List.map_some (fun (_, _, y, _) -> y) vars in
          let rcs = List.map (fun (_, _, _, rc) -> rc) vars in
          let x = close_x (vector xs) in
          let y = close_y (vector ys) in
          if List.length rs < List.length vars
          then let_in_vector rs (cond c x y) (vector rcs)
          else {expr}
      | _ -> {expr}
    in
    match expr with
    | If (c, x, y) -> handle st if_ c (None, x) (None, y)
    | If_left (c, xl, l, xr, r) ->
        let close c l r = if_left c (xl, l) (xr, r) in
        handle st close c (xl, l) (xr, r)
    | If_none (c, xr, l, r) ->
        let close c l r = if_none c l (xr, r) in
        handle st close c (None, l) (xr, r)
    | expr -> {expr}
  in
  mk "factor_ifs" (cata_expr f) [pre_post bindings_unique]

exception No_label

let tree_of_record tree e =
  let leaf = function
    | None -> raise No_label
    | Some fld -> proj_field fld e
  in
  try Binary_tree.cata leaf (pair None None) tree with
  | No_label -> e

let eta_expand_parameter_and_storage st {tstorage; parameter_and_storage} =
  let pre, post =
    match tstorage with
    | T_record r ->
        let r = Binary_tree.map fst r in
        let post body =
          let ops' = fresh st "ops" in
          let s' = fresh st "s" in
          match_record
            Binary_tree.(Node (Leaf (Some ops'), Leaf (Some s')))
            body
            (tuple [var ops'; record_of_tree r (var s')])
        in
        (tree_of_record r, post)
    | _ -> (id, id)
  in
  let f body =
    match parameter_and_storage with
    | None -> body
    | Some ps ->
        let gp = "__parameter" in
        let s = "__storage" in
        let body =
          substitute [(ps, pair None None (var gp) (pre (var s)))] body
        in
        let body = post body in
        let body = let_in_var (Some s) (prim1 Cdr (var ps)) body in
        let body = let_in_var (Some gp) (prim1 Car (var ps)) body in
        body
  in
  mk "eta_expand_parameter_and_storage" f [preserves bindings_unique]

let erase_comments =
  let f = function
    | Comment (_, e) -> e
    | expr -> {expr}
  in
  mk "tuplefy" (cata_expr f) [preserves bindings_unique]

let tuplefy =
  let has_missing_label r =
    List.exists (fun (lbl, _) -> Option.is_none lbl) (Binary_tree.to_list r)
  in
  let f = function
    | Record r when has_missing_label r ->
        Binary_tree.cata (fun (_, t) -> t) (Expr.pair None None) r
    | expr -> {expr}
  in
  mk "tuplefy" (cata_expr f) [preserves bindings_unique]

let recordify st =
  let f lhs rhs e =
    let fresh = Binary_tree.map (fun x -> (x, fresh st "x")) lhs in
    let lhs = Binary_tree.map (fun (_, x) -> Some x) fresh in
    let subst =
      let g = function
        | Some x, x' -> Some (x, Expr.var x')
        | _ -> None
      in
      List.map_some g (Binary_tree.to_list fresh)
    in
    let e = substitute subst e in
    match rhs with
    | {expr = Prim1_fail _} -> rhs (* TODO look at stack type, not syntax *)
    | _ -> match_record lhs rhs e
  in
  let f = function
    | Match_record (lhs, {expr = If (s, l, r)}, e) ->
        if_ s (f lhs l e) (f lhs r e)
    | Match_record (lhs, {expr = Match_variant (s, clauses)}, e) ->
        let clauses =
          Binary_tree.map (fun c -> {c with rhs = f lhs c.rhs e}) clauses
        in
        match_variant s clauses
    | Match_record (Node (Leaf x, Leaf y), {expr = Prim2 (Pair (_, _), t, u)}, e)
      ->
        let_in_var x t (let_in_var y u e)
    (* ------------------------------------------------------------------------ *)
    (* Distribute record_of_tree: *)
    | Let_in
        ( P_var (Some x)
        , e1
        , { expr =
              Record
                Binary_tree.(
                  Node
                    ( Leaf
                        ( _
                        , ({expr = Prim0 (Nil _)} as e2)
                        (* TODO generalize to any expression not mentioning x *)
                        )
                    , Leaf (_, {expr = Record_of_tree (row, {expr = Var x'})})
                    )) } )
      when x = x' ->
        let_in_var (Some x) (record_of_tree row e1) (tuple [e2; var x'])
    | Record_of_tree (row, {expr = Match_variant (scrutinee, clauses)}) ->
        let f c = {c with rhs = record_of_tree row c.rhs} in
        match_variant scrutinee (Binary_tree.map f clauses)
    | Record_of_tree (_row, ({expr = Prim1_fail _} as e)) -> e
    (* Fallback: convert to record if possible: *)
    | Record_of_tree (row, tree) as expr ->
        let open Option in
        let rec parse_tree row tree =
          match (row, tree.expr) with
          | Leaf l, _ -> Some (Binary_tree.Leaf (l, tree))
          | Node (r1, r2), Prim2 (Pair _, x1, x2) ->
              let* x1 = parse_tree r1 x1 in
              let* x2 = parse_tree r2 x2 in
              Some (Binary_tree.Node (x1, x2))
          | _ -> None
        in
        Option.cata {expr} record (parse_tree row tree)
    | expr -> {expr}
  in
  mk "recordify" (cata_expr f) [preserves bindings_unique]

let unfold_unpair =
  let f = function
    | Unpair (2, x) -> vector [prim1 Car x; prim1 Cdr x]
    | Prim1 (Cast _, x) -> x
    | expr -> {expr}
  in
  mk "unfold_unpair" (cata_expr f) [preserves bindings_unique]

let reify_rev_ops =
  let f = function
    | Iter_over
        ( [l; {expr = Prim0 (Nil (T0 T_operation))}]
        , [x; xs]
        , { expr =
              Vector [{expr = Prim2 (Cons, {expr = Var x'}, {expr = Var xs'})}]
          } )
      when (x, xs) = (Some x', Some xs') ->
        vector [prim2 Exec l (var "!rev_ops")]
    | Prim2 (Exec, x, {expr = Var "!rev_ops"}) ->
        (* FIXME Don't simply delete this. *) x
    | expr -> {expr}
  in
  mk "reify_rev_ops" (cata_expr f) [preserves bindings_unique]

let simplify st =
  let main =
    Sequence
      [ unfold_ifs
      ; unfold_stack_ops ~full:true
      ; inline_singles
      ; fold_constants
      ; mark_unused
      ; unvectorize_conds st
      ; field_access
      ; flatten_matches
      ; Label ("fix_mangle", Fixpoint (mangle st))
      ; infer_constructors
      ; factor_invariant_loop_state st
      ; flatten_lets
      ; unused_loop_state st
      ; factorise_clauses st
      ; reify_booleans ]
  in
  let t = Fixpoint main in
  run_transformer t

let smartMLify st pc =
  let t =
    Sequence
      [ erase_comments
      ; unfold_ifs
      ; Fixpoint (Sequence [unfold_stack_ops ~full:true; chop_stack_ops])
      ; unfold_unpair
      ; Fixpoint
          (Sequence
             [ inline_singles
             ; fold_constants
             ; mark_unused
             ; unvectorize_conds st
             ; flatten_lets
             ; field_access
             ; flatten_matches
             ; Label ("fix_mangle", Fixpoint (mangle st))
             ; infer_constructors
             ; factor_invariant_loop_state st
             ; unused_loop_state st
             ; factorise_clauses st
             ; reify_booleans ])
      ; binarize_matches st
      ; binarize_projs
      ; single_lets st
      ; binarize_records
      ; eta_expand_parameter_and_storage st pc
      ; flatten_matches
      ; Fixpoint reify_rev_ops
      ; Fixpoint
          (Sequence
             [ recordify st
             ; fold_constants
             ; inline_singles
             ; distribute_lets
             ; unvectorize_conds st
             ; flatten_lets
             ; factor_invariant_loop_state st
             ; unused_loop_state st
             ; tuplefy ])
      ; Fixpoint (unfold_getn_and_updaten st) ]
  in
  run_transformer t pc

let michelsonify st =
  let t =
    Sequence
      [ chop_stack_ops
      ; Fixpoint
          (Sequence
             [ mark_unused
             ; inline_simples
             ; factor_invariant_loop_state st
             ; unused_loop_state st
             ; unfold_stack_ops ~full:false
             ; fold_constants
             ; factor_ifs st
             ; flatten_lets ])
      ; nest_ifs
      ; name_all st
      ; drop_unused
        (* NB We don't need full-blown linearization here because
           none of the above transformations create variables with
           several occurrences. *)
      ; Fixpoint (Sequence [factorise_failwith; flatten_ifs]) ]
  in
  run_transformer t
