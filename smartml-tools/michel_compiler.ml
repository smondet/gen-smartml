(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils
open Control
open Michelson
open Michel.Expr
open Michel.Type

let rec compile_type ?with_annots = function
  | T0 t -> mt0 t
  | T1 (t, t1) -> mt1 t (compile_type t1)
  | T2 (t, t1, t2) -> mt2 t (compile_type t1) (compile_type t2)
  | T_record r ->
      let mt_pair (annot_fst, t1) (annot_snd, t2) =
        if with_annots = Some ()
        then (None, mt_pair ?annot_fst ?annot_snd t1 t2)
        else (None, mt_pair t1 t2)
      in
      ( match Binary_tree.cata (map_snd compile_type) mt_pair r with
      | None, t -> t
      | Some _, _ -> assert false )
  | T_variant r as t ->
    ( match view_option t with
    | Some t -> mt_option (compile_type t)
    | None ->
        let mt_or (annot_left, t1) (annot_right, t2) =
          if with_annots = Some ()
          then (None, mt_or ?annot_left ?annot_right t1 t2)
          else (None, mt_or t1 t2)
        in
        ( match Binary_tree.cata (map_snd compile_type) mt_or r with
        | None, t -> t
        | Some _, _ -> assert false ) )
  | T_vector _ -> assert false
  | T_missing s -> mt_var s

let compile_lit = function
  | Unit -> MI0 Unit_
  | Nat i -> MIpush (mt_nat, MLiteral.int i)
  | Int i -> MIpush (mt_int, MLiteral.int i)
  | Mutez i -> MIpush (mt_mutez, MLiteral.int i)
  | String s -> MIpush (mt_string, MLiteral.string s)
  | Key_hash s -> MIpush (mt_key_hash, MLiteral.string s)
  | Bytes s -> MIpush (mt_bytes, MLiteral.bytes s)
  | Chain_id s -> MIpush (mt_chain_id, MLiteral.bytes s)
  | Address s -> MIpush (mt_address, MLiteral.string s)
  | Timestamp s -> MIpush (mt_timestamp, MLiteral.string s)
  | Bool b -> MIpush (mt_bool, MLiteral.bool b)
  | Bls12_381_g1 s -> MIpush (mt_bls12_381_g1, MLiteral.bytes s)
  | Bls12_381_g2 s -> MIpush (mt_bls12_381_g2, MLiteral.bytes s)
  | Bls12_381_fr s -> MIpush (mt_bls12_381_fr, MLiteral.bytes s)
  | Signature s -> MIpush (mt_signature, MLiteral.string s)

let compile_prim1 = function
  | Read_ticket -> assert false
  | Car -> MIfield [A]
  | Cdr -> MIfield [D]
  | Concat1 -> MI1 Concat1
  | e -> MI1 (map_prim1 compile_type e)

type stack =
  | Stack_ok     of string option list
  | Stack_failed
[@@deriving show {with_path = false}]

type state =
  { stack : stack
  ; instrs : instr list }

module S = State (struct
  type t = state
end)

open S

let stack_apply f =
  modify (fun s ->
      match s.stack with
      | Stack_ok stack -> {s with stack = f stack}
      | Stack_failed -> failwith "stack_apply")

let stack_replace i xs =
  modify (fun s ->
      match s.stack with
      | Stack_ok stack ->
          let lo = List.drop i stack in
          {s with stack = Stack_ok (xs @ lo)}
      | Stack_failed -> failwith "stack_replace")

let stack_remove i =
  modify (fun s ->
      let err () =
        failwith (Format.asprintf "stack_remove %d %a" i pp_stack s.stack)
      in
      match s.stack with
      | Stack_ok stack ->
        ( match List.split_at ~err:"stack_remove" i stack with
        | s1, _ :: s2 -> {s with stack = Stack_ok (s1 @ s2)}
        | _ -> err () )
      | _ -> err ())

let emit instr = modify (fun s -> {s with instrs = s.instrs @ [{instr}]})

let get_stack =
  let* {stack} = get in
  return stack

let mi_dig i =
  let* () =
    match i with
    | 0 -> return ()
    | 1 -> emit MIswap
    | _ -> emit (MIdig i)
  in
  stack_remove i >> stack_replace 0 [None]

let run_with x =
  let* stack = get_stack in
  let s = {instrs = []; stack} in
  let (), c = run x s in
  return ({instr = MIseq c.instrs}, c.stack)

let join_stacks stack1 stack2 =
  match (stack1, stack2) with
  | Stack_failed, Stack_ok stack | Stack_ok stack, Stack_failed ->
      modify (fun s -> {s with stack = Stack_ok stack})
  | Stack_failed, Stack_failed ->
      modify (fun s -> {s with stack = Stack_failed})
  | Stack_ok stack1, Stack_ok stack2
    when List.length stack1 = List.length stack2 ->
      let f x y =
        match (x, y) with
        | Some x, Some y when x = y -> Some x
        | _ -> None
      in
      let stack = Stack_ok (List.map2 f stack1 stack2) in
      modify (fun s -> {s with stack})
  | _ ->
      emit
        (MIerror
           (Format.asprintf
              "join_stacks: %a vs. %a"
              pp_stack
              stack1
              pp_stack
              stack2))

let apply_tags xs =
  let rec f i = function
    | [] -> return ()
    | None :: xs -> mi_dig i >> emit MIdrop >> f i xs
    | Some _ :: xs -> f (i + 1) xs
  in
  let* () = f 0 xs in
  stack_replace (List.length xs) (List.filter Option.is_some xs)

let add_and_apply xs = stack_replace 0 xs >> apply_tags xs

let _comment_expr e = emit (MIcomment [Format.asprintf "%a" print_expr e; ""])

let _comment_stack =
  let* stack = get_stack in
  emit (MIcomment [Format.asprintf "%a" pp_stack stack; ""])

let compile_stack_op = function
  | Dup n ->
      let f s =
        let hi, lo = List.split_at ~err:"compile_stack_op dup" (n - 1) s in
        match lo with
        | _x :: lo -> Stack_ok ((None :: hi) @ (None :: lo))
        | [] -> failwith "dup"
      in
      emit (MIdup n) >> stack_apply f
  | Swap ->
      let f = function
        | x0 :: x1 :: xs -> Stack_ok (x1 :: x0 :: xs)
        | _ -> failwith "swap"
      in
      emit MIswap >> stack_apply f
  | Dig n ->
      let f s =
        let hi, lo = List.split_at ~err:"compile_stack_op dig" n s in
        match lo with
        | x :: lo -> Stack_ok ((x :: hi) @ lo)
        | [] -> failwith "dig"
      in
      emit (MIdig n) >> stack_apply f
  | Dug n ->
      let f s =
        assert (List.length s > n);
        match List.split_at ~err:"compile_stack_op dug" (n + 1) s with
        | x :: hi, lo -> Stack_ok (hi @ (x :: lo))
        | [], _ -> failwith "dug"
      in
      emit (MIdug n) >> stack_apply f
  | Drop n -> emit (MIdropn n) >> stack_replace n []

let compile_permutation (current : string list) desired =
  let dig n =
    match List.split_at n current with
    | hi, x :: lo -> (x :: hi) @ lo
    | _ -> assert false
  in
  if current = desired
  then Some (return ())
  else
    let ixs = List.mapi (fun i _ -> i) current in
    Option.map mi_dig (List.find_opt (fun i -> dig i = desired) ixs)

let rec compile_expr e =
  match e.expr with
  | Var x ->
      get_stack
      >>= (function
      | Stack_ok stack ->
        ( match List.find_ix (Some x) stack with
        | None -> failwith (Format.asprintf "Variable not found: %s" x)
        | Some i -> mi_dig i )
      | Stack_failed -> assert false)
  | Let_in _ ->
      let rec unroll_lets r = function
        | {expr = Let_in (vs, e1, e2)} -> unroll_lets ((vs, e1) :: r) e2
        | e -> (List.rev r, e)
      in
      let bs, r = unroll_lets [] e in
      let f (xs, e) =
        let xs =
          match xs with
          | P_var x -> [x]
          | P_vector xs -> xs
        in
        let* () = compile_vector [e] in
        apply_tags xs
      in
      let* () = iter_list f bs in
      compile_vector [r]
  | Lambda (x, t1, t2, e) ->
      let (), {instrs} =
        let stack = Stack_ok [x] in
        run (compile_expr e) {stack; instrs = []}
      in
      let code = {instr = MIseq instrs} in
      emit (MIlambda (compile_type t1, compile_type t2, code))
      >> stack_replace 0 [None]
  | Lit l -> emit (compile_lit l) >> stack_replace 0 [None]
  | Prim0 p -> emit (MI0 (map_prim0 compile_type p)) >> stack_replace 0 [None]
  | Prim1_fail (p, e1) ->
      compile_vector [e1]
      >> emit (MI1_fail p)
      >> modify (fun s -> {s with stack = Stack_failed})
  | Unpair (n, e) ->
      compile_vector [e]
      >> emit (MIunpair (List.replicate n true))
      >> stack_replace 1 (List.replicate n None)
  | Prim1 (Read_ticket, e1) ->
      compile_vector [e1]
      >> emit (MI1 Read_ticket)
      >> stack_replace 1 [None; None]
  | Prim1 (p, e1) ->
      compile_vector [e1] >> emit (compile_prim1 p) >> stack_replace 1 [None]
  | Prim2 (p, e1, e2) ->
      compile_vector [e1; e2]
      >> emit (MI2 (map_prim2 compile_type p))
      >> stack_replace 2 [None]
  | Prim3 (Get_and_update, e1, e2, e3) ->
      compile_vector [e1; e2; e3]
      >> emit (MI3 Get_and_update)
      >> stack_replace 3 [None; None]
  | Prim3 (p, e1, e2, e3) ->
      compile_vector [e1; e2; e3] >> emit (MI3 p) >> stack_replace 3 [None]
  | Proj_field _ -> assert false
  | Create_contract
      ({tparameter; tstorage; parameter_and_storage; body}, e1, e2, e3) ->
      let (), {instrs} =
        let stack = Stack_ok [parameter_and_storage] in
        run (compile_expr body) {stack; instrs = []}
      in
      let tparameter = map_fst compile_type tparameter in
      let tstorage = compile_type tstorage in
      let code = {instr = MIseq instrs} in
      compile_vector [e1; e2; e3]
      >> emit (MIcreate_contract {tparameter; tstorage; code})
      >> stack_replace 3 [None; None]
  | Stack_op (op, es) -> compile_vector es >> compile_stack_op op
  | Record xs ->
      let xs = Binary_tree.to_list xs in
      let n = List.length xs in
      let* () = compile_vector (List.map snd xs) in
      let* () = emit (MIpairn n) in
      stack_replace n [None]
  | Variant _ -> assert false
  | List (t, xs) ->
      let mk x =
        compile_vector [x] >> emit (MI2 Cons) >> stack_replace 2 [None]
      in
      emit (MI0 (Nil (compile_type t)))
      >> stack_replace 0 [None]
      >> iter_list mk (List.rev xs)
  | Set (t, xs) ->
      let mk x =
        emit (MIpush (mt_bool, MLiteral.bool true))
        >> compile_vector [x]
        >> emit (MI3 Update)
        >> stack_replace 2 [None]
      in
      emit (MI0 (Empty_set (compile_type t)))
      >> stack_replace 0 [None]
      >> iter_list mk (List.rev xs)
  | Map (tk, tv, entries) ->
      let mk_entry (k, v) =
        compile_vector [v]
        >> emit (MI1 Some_)
        >> compile_vector [k]
        >> emit (MI3 Update)
        >> stack_replace 3 [None]
      in
      emit (MI0 (Empty_map (compile_type tk, compile_type tv)))
      >> stack_replace 0 [None]
      >> iter_list mk_entry (List.rev entries)
  | Match_variant (_scrutinee, _clauses) -> assert false
  | If (c, l, r) ->
      let* () = compile_vector [c] in
      let* () = stack_remove 0 in
      let* l, sl = run_with (compile_vector [l]) in
      let* r, sr = run_with (compile_vector [r]) in
      let* () = join_stacks sl sr in
      emit (MIif (l, r))
  | If_none (c, x, l, r) ->
      let* () = compile_vector [c] in
      let* () = stack_remove 0 in
      let* l, sl = run_with (compile_vector [l]) in
      let* r, sr = run_with (add_and_apply [x] >> compile_vector [r]) in
      let* () = join_stacks sl sr in
      emit (MIif_none (l, r))
  | If_cons (c, x, xs, l, r) ->
      let* () = compile_vector [c] in
      let* () = stack_remove 0 in
      let* l, sl = run_with (add_and_apply [x; xs] >> compile_vector [l]) in
      let* r, sr = run_with (compile_vector [r]) in
      let* () = join_stacks sl sr in
      emit (MIif_cons (l, r))
  | If_left (c, xl, l, xr, r) ->
      let* () = compile_vector [c] in
      let* () = stack_remove 0 in
      let* l, sl = run_with (add_and_apply [xl] >> compile_vector [l]) in
      let* r, sr = run_with (add_and_apply [xr] >> compile_vector [r]) in
      let* () = join_stacks sl sr in
      emit (MIif_left (l, r))
  | Loop (init, xs, step) ->
      let* () = compile_vector init in
      let* () = stack_remove 0 in
      let* instr, _stack_out =
        run_with (apply_tags xs >> compile_vector [step])
      in
      emit (MIloop instr)
  | Map_over (init, xs, step) ->
      let* () = compile_vector init in
      let* instr, _stack_out =
        run_with (apply_tags xs >> compile_vector [step])
      in
      emit (MImap instr)
  | Iter_over (init, xs, step) ->
      let* () = compile_vector init in
      let* instr, _stack_out =
        run_with (apply_tags xs >> compile_vector [step])
      in
      emit (MIiter instr) >> stack_replace 1 []
  | Match_record _ -> assert false
  | Vector es -> compile_vector es
  | Nth (_, _) -> assert false
  | Comment (c, e) -> emit (MIcomment c) >> compile_expr e
  | Record_of_tree _ -> assert false

and compile_vector es =
  let* s0 =
    get_stack
    >>= function
    | Stack_ok s -> return (List.rev s)
    | Stack_failed -> assert false
  in
  let es0 = List.rev es in
  let rec compile s es =
    match (s, es) with
    | [], [] -> return ()
    | Some x :: s, {expr = Var x'} :: es when x = x' -> compile s es
    | s, es ->
        let r =
          let open Option in
          let get_var = function
            | {expr = Var x} -> return x
            | _ -> None
          in
          let* () =
            if List.length s = List.length es then return () else None
          in
          let* s = sequence_list (List.rev s) in
          let* es = map_list get_var (List.rev es) in
          compile_permutation s es
        in
        ( match r with
        | Some r -> r
        | None -> iter_list compile_expr es0 )
  in
  compile s0 es0

let compile_expr stack e =
  let (), {stack; instrs} = run (compile_expr e) {stack; instrs = []} in
  (instrs, stack)

let compile_contract
    ?storage
    ?lazy_entry_points
    ~views
    Michel.Typing.
      {checked_precontract = {tparameter; tstorage; parameter_and_storage; body}}
    =
  let stack = Stack_ok [parameter_and_storage] in
  let instrs, _ = compile_expr stack (erase_types body) in
  let tparameter = map_fst (compile_type ~with_annots:()) tparameter in
  let tstorage = compile_type ~with_annots:() tstorage in
  let code = {instr = MIseq instrs} in
  let views = List.map Michelson.(map_view erase_types_instr) views in
  (* FIXME: also simplify views *)
  let contract : Michelson.contract =
    {tparameter; tstorage; lazy_entry_points; storage; code; views}
  in
  Michelson.typecheck_contract ~strict_dup:false contract
