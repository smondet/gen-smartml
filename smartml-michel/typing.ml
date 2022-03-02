(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils
open Control
open Expr
open Type

type checked_precontract = {checked_precontract : texpr precontract}
[@@deriving eq]

let erase_types_precontract {tparameter; tstorage; parameter_and_storage; body}
    =
  let body = erase_types body in
  {tparameter; tstorage; parameter_and_storage; body}

let print_checked_precontract ppf {checked_precontract} =
  print_precontract print_expr ppf (erase_types_precontract checked_precontract)

let show_checked_precontract = Format.asprintf "%a" print_checked_precontract

let typecheck_lit = function
  | Unit -> t_unit
  | Int _ -> t_int
  | Nat _ -> t_nat
  | Mutez _ -> t_mutez
  | String _ -> t_string
  | Key_hash _ -> t_key_hash
  | Bytes _ -> t_bytes
  | Chain_id _ -> t_chain_id
  | Address _ -> t_address
  | Timestamp _ -> t_timestamp
  | Bool _ -> t_bool
  | Bls12_381_g1 _ -> t_bls12_381_g1
  | Bls12_381_g2 _ -> t_bls12_381_g2
  | Bls12_381_fr _ -> t_bls12_381_fr
  | Signature _ -> t_signature

let ok1 x = Ok (Stack_ok x)

let typecheck_prim0_f ~tparameter = function
  | Amount | Balance -> ok1 t_mutez
  | Level -> ok1 t_nat
  | Now -> ok1 t_timestamp
  | Self_address -> ok1 t_address
  | Self None -> ok1 (t_contract (fst tparameter))
  | Self (Some ep) ->
    ( match tparameter with
    | T_variant row, _annot ->
      (* TODO single entry point annotation *)
      ( match List.assoc_opt (Some ep) (Binary_tree.to_list row) with
      | Some t -> ok1 (t_contract t)
      | None -> Error "Self: entry-point not found" )
    | _ -> Error "Self: parameter not a variant" )
  | Sender | Source -> ok1 t_address
  | Chain_id -> ok1 t_chain_id
  | Total_voting_power -> ok1 t_nat
  | Sapling_empty_state {memo} -> ok1 (t_sapling_state memo)
  | Unit_ -> ok1 t_unit
  | None_ t -> ok1 (t_option t)
  | Nil t -> ok1 (t_list t)
  | Empty_set t -> ok1 (t_set t)
  | Empty_map (tk, tv) -> ok1 (t_map tk tv)
  | Empty_bigmap (tk, tv) -> ok1 (t_big_map tk tv)

let with_lub err t1 t2 k =
  match Type.lub t1 t2 with
  | None -> err
  | Some t -> k t

let rec typecheck_getn err n = function
  | t when n = 0 -> ok1 t
  | T_record (Node (Leaf (_, t), _)) when n = 1 -> ok1 t
  | T_record (Node (r, _)) when n = 1 -> ok1 (T_record r)
  | T_record (Node (_, Leaf (_, t))) -> typecheck_getn err (n - 2) t
  | T_record (Node (_, r)) -> typecheck_getn err (n - 2) (T_record r)
  | _ -> err

let rec typecheck_updaten err n t1 t2 = function
  | t when n = 0 -> with_lub err t1 t (fun _ -> ok1 t2)
  | T_record (Node (Leaf (_, t), _)) when n = 1 ->
      with_lub err t1 t (fun _ -> ok1 t2)
  | T_record (Node (r, _)) when n = 1 ->
      with_lub err t1 (T_record r) (fun _ -> ok1 t2)
  | T_record (Node (_, Leaf (_, t))) -> typecheck_updaten err (n - 2) t1 t2 t
  | T_record (Node (_, r)) -> typecheck_updaten err (n - 2) t1 t2 (T_record r)
  | _ -> err

let typecheck_prim1_f p t1 : tys Result.t =
  let err =
    Result.error
      (Format.asprintf "type error: %a %a" (pp_prim1 print_ty) p print_ty t1)
  in
  match (p, t1) with
  | Cast t, t1 -> if Type.compatible t t1 then ok1 t else err
  | Rename _, _ -> ok1 t1
  | Contract (_, t), T0 T_address -> ok1 (t_option (t_contract t))
  | (Eq | Neq | Le | Lt | Ge | Gt), T0 T_int -> ok1 t_bool
  | Car, T_record (Node (Leaf (_, t), _)) -> ok1 t
  | Car, T_record (Node (r, _)) -> ok1 (T_record r)
  | Cdr, T_record (Node (_, Leaf (_, t))) -> ok1 t
  | Cdr, T_record (Node (_, r)) -> ok1 (T_record r)
  | Getn n, t -> typecheck_getn err n t
  | Right (a1, a2, tl), tr | Left (a1, a2, tr), tl ->
      ok1 (T_variant (Node (Leaf (a1, tl), Leaf (a2, tr))))
  | Some_, t ->
      ok1 (T_variant (Node (Leaf (Some "None", t_unit), Leaf (Some "Some", t))))
  | Abs, T0 T_int -> ok1 t_nat
  | (Neg | Not), T0 (T_int | T_nat) -> ok1 t_int
  | Neg, T0 T_bls12_381_g1 -> ok1 t_bls12_381_g1
  | Neg, T0 T_bls12_381_g2 -> ok1 t_bls12_381_g2
  | Neg, T0 T_bls12_381_fr -> ok1 t_bls12_381_fr
  | Int, T0 T_nat -> ok1 t_int
  | Int, T0 T_bls12_381_fr -> ok1 t_int
  | IsNat, T0 T_int -> ok1 (t_option t_nat)
  | Not, b when equal_ty b t_bool -> ok1 t_bool
  | Concat1, T1 (T_list, T0 T_string) -> ok1 t_string
  | Concat1, T1 (T_list, T0 T_bytes) -> ok1 t_bytes
  | Size, (T0 (T_string | T_bytes) | T1 ((T_list | T_set), _) | T2 (T_map, _, _))
    ->
      ok1 t_nat
  | Address, T1 (T_contract, _) -> ok1 t_address
  | Implicit_account, T0 T_key_hash -> ok1 (t_contract t_unit)
  | Pack, _ -> ok1 t_bytes
  | Unpack t, T0 T_bytes -> ok1 (t_option t)
  | Hash_key, T0 T_key -> ok1 t_key_hash
  | (Blake2b | Sha256 | Sha512 | Sha3 | Keccak), T0 T_bytes -> ok1 t_bytes
  | Set_delegate, t when equal_ty t (t_option t_key_hash) -> ok1 t_operation
  | Read_ticket, T1 (T_ticket, t) ->
      Ok (Stack_ok (T_vector [t_pair t_address (t_pair t t_nat); t_ticket t]))
  | ( Join_tickets
    , T_record
        (Node (Leaf (None, T1 (T_ticket, t1)), Leaf (None, T1 (T_ticket, t2))))
    ) ->
      with_lub err t1 t2 (fun t -> ok1 (t_option (t_ticket t)))
  | ( Pairing_check
    , T1
        ( T_list
        , T_record
            Binary_tree.(
              Node (Leaf (_, T0 T_bls12_381_g1), Leaf (_, T0 T_bls12_381_g2)))
        ) ) ->
      ok1 t_bool
  | Voting_power, T0 T_key_hash -> ok1 t_nat
  | p, t1 ->
      Error
        (Format.asprintf "type error: %a %a" (pp_prim1 print_ty) p print_ty t1)

let typecheck_prim2_f p t1 t2 : tys Result.t =
  let err =
    Result.error
      (Format.asprintf
         "type error: %a %a %a"
         (pp_prim2 print_ty)
         p
         print_ty
         t1
         print_ty
         t2)
  in
  match (p, t1, t2) with
  | Pair (annot1, annot2), t1, t2 -> ok1 (t_pair ?annot1 ?annot2 t1 t2)
  | (Add | Mul | Sub), T0 T_int, T0 T_int -> ok1 t_int
  | (Add | Mul | Sub), T0 T_int, T0 T_nat -> ok1 t_int
  | (Add | Mul | Sub), T0 T_nat, T0 T_int -> ok1 t_int
  | (Add | Mul | Lsl | Lsr | Or | And | Xor), T0 T_nat, T0 T_nat -> ok1 t_nat
  | (Or | And | Xor), b1, b2 when equal_ty b1 t_bool && equal_ty b2 t_bool ->
      ok1 t_bool
  | Sub, T0 T_nat, T0 T_nat -> ok1 t_int
  | And, T0 T_int, T0 T_nat -> ok1 t_nat
  | Add, T0 T_timestamp, T0 T_int -> ok1 t_timestamp
  | Add, T0 T_int, T0 T_timestamp -> ok1 t_timestamp
  | Add, T0 T_bls12_381_g1, T0 T_bls12_381_g1 -> ok1 t_bls12_381_g1
  | Add, T0 T_bls12_381_g2, T0 T_bls12_381_g2 -> ok1 t_bls12_381_g2
  | Add, T0 T_bls12_381_fr, T0 T_bls12_381_fr -> ok1 t_bls12_381_fr
  | Sub, T0 T_timestamp, T0 T_int -> ok1 t_timestamp
  | Sub, T0 T_timestamp, T0 T_timestamp -> ok1 t_int
  | (Add | Sub), T0 T_mutez, T0 T_mutez -> ok1 t_mutez
  | Mul, T0 T_mutez, T0 T_nat -> ok1 t_mutez
  | Mul, T0 T_nat, T0 T_mutez -> ok1 t_mutez
  | Mul, T0 T_bls12_381_g1, T0 T_bls12_381_fr -> ok1 t_bls12_381_g1
  | Mul, T0 T_bls12_381_g2, T0 T_bls12_381_fr -> ok1 t_bls12_381_g2
  | Mul, T0 T_bls12_381_fr, T0 T_bls12_381_fr -> ok1 t_bls12_381_fr
  | Mul, T0 (T_nat | T_int), T0 T_bls12_381_fr -> ok1 t_bls12_381_fr
  | Mul, T0 T_bls12_381_fr, T0 (T_nat | T_int) -> ok1 t_bls12_381_fr
  | Ediv, T0 T_mutez, T0 T_nat -> ok1 (t_option (t_pair t_mutez t_mutez))
  | Ediv, T0 T_mutez, T0 T_mutez -> ok1 (t_option (t_pair t_nat t_mutez))
  | Ediv, T0 T_int, T0 T_int -> ok1 (t_option (t_pair t_int t_nat))
  | Ediv, T0 T_int, T0 T_nat -> ok1 (t_option (t_pair t_int t_nat))
  | Ediv, T0 T_nat, T0 T_int -> ok1 (t_option (t_pair t_int t_nat))
  | Ediv, T0 T_nat, T0 T_nat -> ok1 (t_option (t_pair t_nat t_nat))
  | Cons, t1, T1 (T_list, t2) -> with_lub err t1 t2 (fun t -> ok1 (t_list t))
  | Compare, t1, t2 ->
      with_lub err t1 t2 (fun _t -> ok1 t_int)
      (* TODO: check that _t is comparable. *)
  | Concat2, T0 T_bytes, T0 T_bytes -> ok1 t_bytes
  | Concat2, T0 T_string, T0 T_string -> ok1 t_string
  | Get, k1, T2 ((T_map | T_big_map), k2, v) ->
      with_lub err k1 k2 (fun _k -> ok1 (t_option v))
  | Mem, k1, T2 ((T_map | T_big_map), k2, _v) ->
      with_lub err k1 k2 (fun _k -> ok1 t_bool)
  | Mem, el1, T1 (T_set, el2) -> with_lub err el1 el2 (fun _el -> ok1 t_bool)
  | Exec, a1, T2 (T_lambda, a2, b) -> with_lub err a1 a2 (fun _el -> ok1 b)
  | Apply, a1, T2 (T_lambda, T_record (Node (Leaf (_, a2), b)), c) ->
      with_lub err a1 a2 (fun _a -> ok1 (t_lambda (T_record b) c))
  | Updaten n, t1, t2 -> typecheck_updaten err n t1 t2 t2
  | ( Sapling_verify_update
    , T0 (T_sapling_transaction {memo = m1})
    , T0 (T_sapling_state {memo = m2}) ) ->
      if m1 <> m2
      then err
      else ok1 (t_option (t_pair t_int (t_sapling_state m2)))
  | Ticket, t, T0 T_nat -> Ok (Stack_ok (t_ticket t))
  (* TODO check that a is packable *)
  | ( Split_ticket
    , T1 (T_ticket, t)
    , T_record (Node (Leaf (None, T0 T_nat), Leaf (None, T0 T_nat))) ) ->
      ok1 (t_option (t_pair (t_ticket t) (t_ticket t)))
  | _ -> err

let un_option = function
  | Binary_tree.Node (Leaf (Some "None", T0 T_unit), Leaf (Some "Some", t)) ->
      Some t
  | _ -> None

let typecheck_prim3_f p t1 t2 t3 : tys Result.t =
  let err =
    Result.error
      (Format.asprintf
         "type error: %a %a %a %a"
         pp_prim3
         p
         print_ty
         t1
         print_ty
         t2
         print_ty
         t3)
  in
  match (p, t1, t2, t3) with
  | Slice, T0 T_nat, T0 T_nat, T0 T_string -> ok1 (t_option t_string)
  | Slice, T0 T_nat, T0 T_nat, T0 T_bytes -> ok1 (t_option t_bytes)
  | Update, e, b, T1 (T_set, e') when equal_ty b t_bool ->
    ( match Type.lub e e' with
    | Some e -> ok1 (t_set e)
    | None -> err )
  | Update, k, T_variant ov, T2 (((T_map | T_big_map) as m), k', v') ->
    ( match (Type.lub k k', Option.(un_option ov >>= Type.lub v')) with
    | Some k, Some v ->
        let map = if m = T_map then t_map else t_big_map in
        ok1 (map k v)
    | _ -> err )
  | Get_and_update, k, T_variant ov, T2 (((T_map | T_big_map) as m), k', v') ->
    ( match (Type.lub k k', Option.(un_option ov >>= Type.lub v')) with
    | Some k, Some v ->
        let map = if m = T_map then t_map else t_big_map in
        Ok (Stack_ok (T_vector [t_option v; map k v]))
    | _ -> err )
  | Transfer_tokens, p, T0 T_mutez, T1 (T_contract, p') ->
      if Type.compatible p p' then ok1 t_operation else err
  | Check_signature, T0 T_key, T0 T_signature, T0 T_bytes -> ok1 t_bool
  | Open_chest, T0 T_chest_key, T0 T_chest, T0 T_nat ->
      ok1 (t_or t_bytes t_bool)
  | _ -> err

let typecheck_stack_op op ts =
  match (op, ts) with
  | Swap, T_vector (t1 :: t2 :: ts) -> Ok (Stack_ok (T_vector (t2 :: t1 :: ts)))
  | Dup n, T_vector ts ->
    ( match List.split_at ~err:"typing: dup" (n - 1) ts with
    | hi, x :: lo -> Ok (Stack_ok (T_vector ((x :: hi) @ (x :: lo))))
    | _, [] -> Error "dup" )
  | Dig n, T_vector ts ->
    ( match List.split_at ~err:"typing: dig" n ts with
    | hi, x :: lo -> Ok (Stack_ok (T_vector ((x :: hi) @ lo)))
    | _, [] -> Error "dig" )
  | Dug n, T_vector ts ->
    ( match List.split_at ~err:"typing: dug" (n + 1) ts with
    | x :: hi, lo when List.length ts > n ->
        Ok (Stack_ok (T_vector (hi @ (x :: lo))))
    | _ -> Error "dug" )
  | Drop n, T_vector ts when n <= List.length ts ->
      Ok (Stack_ok (T_vector (List.drop n ts)))
  | _ ->
      Result.error
        (Format.asprintf "type error: %a %a" pp_stack_op op print_ty ts)

let variant_or_single = function
  | Binary_tree.Leaf (_cons, t) -> t
  | r -> T_variant r

let bind_record rp t =
  let err () =
    Result.error
      (Format.asprintf
         "pattern %a cannot record-match expression of type %a"
         print_record_pattern
         rp
         print_ty
         t)
  in
  let open Binary_tree in
  let rec f = function
    | Leaf None, _ -> Ok []
    | Leaf (Some x), Leaf (_, t) -> Ok [(x, t)]
    | Leaf (Some x), t -> Ok [(x, T_record t)]
    | (Node _ as p), Leaf (_, T_record t) -> f (p, t)
    | Node (p1, p2), Node (t1, t2) ->
        Result.(( @ ) <$> f (p2, t2) <*> f (p1, t1))
    | _ -> err ()
  in
  f (rp, Leaf (None, t))

let bind_var x t = Option.cata [] (fun x -> [(x, t)]) x

let bind_vars xs ts =
  if List.length xs = List.length ts
  then Ok (List.flatten (List.map2 bind_var xs ts))
  else
    let p = Format.asprintf "pattern %a cannot match expressions of types %a" in
    Result.error (p print_vars xs (List.pp print_ty) ts)

let add_to_env x t env = Option.cata env (fun x -> (x, t) :: env) x

let typecheck_f ~tparameter e =
  let open Result in
  let module BTR = Binary_tree.Traversable (Result) in
  match e with
  | Let_in (p, e1, e2) ->
      fun env ->
        let* e1 = e1 env in
        ( match (p, e1.tys) with
        | P_vector xs, Stack_ok (T_vector ts) ->
            let* bs = bind_vars xs ts in
            let* e2 = e2 (bs @ env) in
            Ok {texpr = Let_in (p, e1, e2); tys = e2.tys}
        | P_var x, Stack_ok t ->
            let* e2 = e2 (bind_var x t @ env) in
            Ok {texpr = Let_in (p, e1, e2); tys = e2.tys}
        | xs, t ->
            let p =
              Format.asprintf
                "let: pattern %a cannot match expression of type %a"
            in
            Result.error (p print_pattern xs print_tys t) )
  | Lambda (rp, a, b, body) ->
      fun _env ->
        let bs =
          match rp with
          | None -> []
          | Some x -> [(x, a)]
        in
        let* body = body bs in
        let ok () =
          Ok
            { texpr = Lambda (rp, a, b, body)
            ; tys = Stack_ok (T2 (T_lambda, a, b)) }
        in
        ( match body.tys with
        | Stack_ok r when compatible r b -> ok ()
        | Stack_failed -> ok ()
        | s ->
            let open Format in
            let msg =
              asprintf
                "lambda:@\n  expected :  %a@\n  got      :  %a"
                pp_ty
                b
                pp_tys
                s
            in
            Error msg )
  | Match_record (rp, e1, e2) ->
      fun env ->
        let* e1 = e1 env in
        let* t = Type.get1 e1.tys in
        let* bs = bind_record rp t in
        let* e2 = e2 (bs @ env) in
        Ok {texpr = Match_record (rp, e1, e2); tys = e2.tys}
  | Match_variant (scrutinee, clauses) ->
      fun env ->
        let* scrutinee = scrutinee env in
        ( match scrutinee.tys with
        | Stack_ok (T_variant r) ->
          ( match Binary_tree.matches clauses r with
          | Some clauses ->
              let* clauses =
                clauses
                |> BTR.map (fun ({cons; var; rhs}, var_r) ->
                       let* () =
                         match (cons, var_r) with
                         | Some c, Binary_tree.Leaf (Some c', _) when c <> c' ->
                             Error
                               (Printf.sprintf
                                  "match: wrong constructor %s %s"
                                  c
                                  c')
                         | _ -> Ok ()
                       in
                       let env =
                         match var with
                         | None -> env
                         | Some var -> (var, variant_or_single var_r) :: env
                       in
                       let+ rhs = rhs env in
                       {cons; var; rhs})
              in
              ( match Binary_tree.to_list clauses with
              | [] -> assert false
              | x :: xs ->
                ( match
                    List.fold_left
                      Option.(fun x y -> x >>= Type.lubs y.rhs.tys)
                      (Some x.rhs.tys)
                      xs
                  with
                | None ->
                    Error
                      (Format.asprintf
                         "match: branches mismatch while matching %a:\n%a"
                         print_expr
                         (erase_types scrutinee)
                         (Format.pp_print_list (fun ppf x ->
                              Format.fprintf ppf "  %a" print_tys x.rhs.tys))
                         (x :: xs))
                | Some tys ->
                    Ok {texpr = Match_variant (scrutinee, clauses); tys} ) )
          | None ->
              failwith
                (Format.asprintf
                   "match: incompatible shape %a"
                   (print_row ~sep:"|")
                   r) )
        | tys ->
            Error
              (Format.asprintf
                 "match on non-variant %a : %a"
                 print_expr
                 (erase_types scrutinee)
                 print_tys
                 tys) )
  | Loop (init, rp, step) ->
      fun env ->
        let* init = map_list (fun e -> e env) init in
        let* init_ts = map_list (fun e -> get1 e.tys) init in
        ( match init_ts with
        | cond :: tail when equal_ty cond t_bool ->
            let* xs = bind_vars rp tail in
            let* step = step (xs @ env) in
            ( match step.tys with
            | Stack_ok (T_vector (cond' :: tail')) when equal_ty cond' t_bool ->
                let tail = Stack_ok (T_vector tail) in
                let tail' = Stack_ok (T_vector tail') in
                ( match Type.lubs tail tail' with
                | None ->
                    let msg = Format.asprintf "loop: step mismatch: %a -> %a" in
                    Error (msg print_tys tail print_tys tail')
                | Some tys -> Ok {texpr = Loop (init, rp, step); tys} )
            | tys ->
                Error (Format.asprintf "loop: step result: %a" print_tys tys) )
        | _ -> Error "loop" )
  | Map_over (input, xs, step) ->
      fun env ->
        let* input = map_list (fun x -> x env) input in
        let* close, top, ts =
          let* input_tys = map_list (fun x -> get1 x.tys) input in
          match input_tys with
          | T1 (T_list, a) :: ts -> Ok (Type.t_list, a, ts)
          | T2 (T_map, k, v) :: ts -> Ok (Type.t_map k, Type.t_pair k v, ts)
          | _ -> Error "map_over"
        in
        let* bs = bind_vars xs (top :: ts) in
        let* step = step (bs @ env) in
        let texpr = Map_over (input, xs, step) in
        ( match step.tys with
        | Stack_ok (T_vector (b :: bs')) when List.for_all2 compatible bs' ts ->
            Ok {texpr; tys = Stack_ok (T_vector (close b :: ts))}
        | _ -> Error "map_over: mismatch" )
  | Iter_over (input, xs, step) ->
      fun env ->
        let* input = map_list (fun x -> x env) input in
        let* top, ts =
          let* input_tys = map_list (fun x -> get1 x.tys) input in
          match input_tys with
          | T1 (T_list, a) :: ts | T1 (T_set, a) :: ts -> Ok (a, ts)
          | T2 (T_map, k, v) :: ts -> Ok (Type.t_pair k v, ts)
          | _ -> Error "iter_over"
        in
        let* bs = bind_vars xs (top :: ts) in
        let* step = step (bs @ env) in
        let texpr = Iter_over (input, xs, step) in
        ( match step.tys with
        | Stack_ok (T_vector bs') ->
            if List.for_all2 compatible bs' ts
            then Ok {texpr; tys = Stack_ok (T_vector ts)}
            else
              let pp = List.pp Type.print_ty in
              Error (Format.asprintf "iter_over: %a vs. %a" pp bs' pp ts)
        | Stack_ok _ -> Error "iter_over: non-vector"
        | Stack_failed -> Error "iter_over: FAILED" )
  | If (cond, l, r) ->
      fun env ->
        let* cond = cond env in
        let* cond_t = get1 cond.tys in
        if equal_ty cond_t t_bool
        then
          let* l = l env in
          let* r = r env in
          match Type.lubs l.tys r.tys with
          | Some tys -> Ok {texpr = If (cond, l, r); tys}
          | None ->
              Error
                (Format.asprintf
                   "If: branch mismatch: %a vs. %a"
                   print_tys
                   l.tys
                   print_tys
                   r.tys)
        else Error "If: not a bool"
  | If_none (scrutinee, xr, l, r) ->
      fun env ->
        let* scrutinee = scrutinee env in
        let* scrutinee_t = get1 scrutinee.tys in
        ( match view_option scrutinee_t with
        | Some t ->
            let* l = l env in
            let* r = r (add_to_env xr t env) in
            ( match Type.lubs l.tys r.tys with
            | Some tys -> Ok {texpr = If_none (scrutinee, xr, l, r); tys}
            | None ->
                Error
                  (Format.asprintf
                     "If_none: branch mismatch: %a vs. %a"
                     print_tys
                     l.tys
                     print_tys
                     r.tys) )
        | _ ->
            Error
              (Format.asprintf
                 "If_none: not an option: %a"
                 print_ty
                 scrutinee_t) )
  | If_left (scrutinee, xl, l, xr, r) ->
      fun env ->
        let* scrutinee = scrutinee env in
        let* scrutinee_t = get1 scrutinee.tys in
        ( match view_or scrutinee_t with
        | Some (tl, tr) ->
            let* l = l ((Option.cata env (fun xl -> (xl, tl) :: env)) xl) in
            let* r = r ((Option.cata env (fun xr -> (xr, tr) :: env)) xr) in
            ( match Type.lubs l.tys r.tys with
            | Some tys -> Ok {texpr = If_left (scrutinee, xl, l, xr, r); tys}
            | None ->
                Error
                  (Format.asprintf
                     "If_left: branch mismatch: %a vs. %a"
                     print_tys
                     l.tys
                     print_tys
                     r.tys) )
        | _ ->
            Error
              (Format.asprintf
                 "If_left: not an or-type: %a"
                 print_ty
                 scrutinee_t) )
  | If_cons (scrutinee, x, xs, l, r) ->
      fun env ->
        let* scrutinee = scrutinee env in
        let* scrutinee_t = get1 scrutinee.tys in
        ( match scrutinee_t with
        | T1 (T_list, t) ->
            let* l = l (add_to_env x t (add_to_env xs (t_list t) env)) in
            let* r = r env in
            ( match Type.lubs l.tys r.tys with
            | Some tys -> Ok {texpr = If_cons (scrutinee, x, xs, l, r); tys}
            | None -> Error "If_cons: branch mismatch" )
        | _ -> Error "If_cons: not an option" )
  | Create_contract
      ( {tparameter; tstorage; parameter_and_storage; body}
      , baker
      , balance
      , storage ) ->
      fun env ->
        let* body =
          body
            (Option.cata
               []
               (fun ps ->
                 [ ( ps
                   , let t, annot1 = tparameter in
                     t_pair ?annot1 t tstorage ) ])
               parameter_and_storage)
        in
        let* baker = baker env in
        let* balance = balance env in
        let* storage = storage env in
        (* TODO check body result *)
        let* tbaker = get1 baker.tys in
        let* tbalance = get1 balance.tys in
        let* tstorage' = get1 storage.tys in
        ( match (tbaker, tbalance) with
        | T_variant option_key_hash, T0 T_mutez
          when compatible tstorage tstorage' ->
          ( match un_option option_key_hash with
          | Some (T0 T_key_hash) ->
              let texpr =
                Create_contract
                  ( {tparameter; tstorage; parameter_and_storage; body}
                  , baker
                  , balance
                  , storage )
              in
              Ok {texpr; tys = Stack_ok (T_vector [t_operation; t_address])}
          | _ -> error "create_contract: illegal argument types" )
        | _ -> error "create_contract: illegal argument types" )
  | e ->
      fun env ->
        let module TR = Traversable (Result) in
        let* texpr = TR.sequenceA (map_expr_f (fun x -> x env) e) in
        let tys =
          match map_expr_f (fun {tys} -> tys) texpr with
          | Var "!rev_ops" ->
              ok1 Type.(t_lambda (t_list t_operation) (t_list t_operation))
          | Var x ->
            ( match List.assoc_opt x env with
            | Some x -> ok1 x
            | None -> Error (Printf.sprintf "Unbound variable: %S" x) )
          | Lit l -> ok1 (typecheck_lit l)
          | Prim0 p -> typecheck_prim0_f ~tparameter p
          | Prim1 (p, t1) ->
              let* t1 = get1 t1 in
              typecheck_prim1_f p t1
          | Prim1_fail (Failwith, t1) ->
              let* _t1 = get1 t1 in
              Ok Stack_failed
          | Prim1_fail (Never, t1) ->
              let* t1 = get1 t1 in
              if equal_ty t1 t_never
              then Ok Stack_failed
              else Error "Argument to Never must be of type never"
          | Prim2 (p, t1, t2) ->
              let* t1 = get1 t1 in
              let* t2 = get1 t2 in
              typecheck_prim2_f p t1 t2
          | Prim3 (p, t1, t2, t3) ->
              let* t1 = get1 t1 in
              let* t2 = get1 t2 in
              let* t3 = get1 t3 in
              typecheck_prim3_f p t1 t2 t3
          | Proj_field (fld, t1) ->
              get1 t1
              >>= (function
              | T_record r ->
                  let r = Binary_tree.to_list r in
                  ( match List.assoc_opt (Some fld) r with
                  | Some t -> ok1 t
                  | None ->
                      let pp = List.pp String.pp in
                      let r =
                        List.map (fun (lbl, _) -> Option.default "_" lbl) r
                      in
                      Error
                        (Format.asprintf
                           "Unknown field: %S on record %a"
                           fld
                           pp
                           r) )
              | _ -> Error "proj field on non-record")
          | Stack_op (op, xs) ->
              let* xs = map_list get1 xs in
              typecheck_stack_op op (T_vector xs)
          | List (t, xs) ->
              let* xs = map_list get1 xs in
              ( match List.find_opt (fun t' -> not (compatible t t')) xs with
              | Some _ -> error "list element has unexpected type"
              | None -> ok1 (t_list t) )
          | Set (t, xs) ->
              let* xs = map_list get1 xs in
              ( match List.find_opt (fun t' -> not (compatible t t')) xs with
              | Some _ -> error "set element has unexpected type"
              | None -> ok1 (t_set t) )
          | Map (tk, tv, entries) ->
              let* entries =
                map_list
                  (fun (k, v) -> Control.pair <$> get1 k <*> get1 v)
                  entries
              in
              ( match
                  List.find_opt
                    (fun (tk', tv') ->
                      not (compatible tk tk' && compatible tv tv'))
                    entries
                with
              | Some _ -> error "map entry has unexpected type"
              | None -> ok1 (t_map tk tv) )
          | Record xs ->
              let* xs =
                TR.BT.map (fun (lbl, t) -> Control.pair lbl <$> get1 t) xs
              in
              ok1 (t_record xs)
          | Variant (lbl, rc, t) ->
              let* t = get1 t in
              ok1 (T_variant (Binary_tree.fill (Leaf (lbl, t)) rc))
          | Vector ts ->
              let* ts = Result.map_list get1 ts in
              return (Stack_ok (T_vector ts))
          | Nth (n, x) ->
              let* t = get1 x in
              ( match t with
              | T_vector ts when n < List.length ts ->
                  return (Stack_ok (List.nth ts n))
              | _ -> error "not a vector or too short" )
          | Unpair ((n as n0), x) ->
              assert (n >= 2);
              let* t = get1 x in
              let get_type = function
                | Binary_tree.Leaf (_, t) -> t
                | r -> t_record r
              in
              let rec unpair (n, r) =
                match (n, r) with
                | 1, r -> return [get_type r]
                | n, Binary_tree.Node (l, r) ->
                    let* ts = unpair (n - 1, r) in
                    return (get_type l :: ts)
                | _, Binary_tree.Leaf (_, T_record r) -> unpair (n, r)
                | _, Binary_tree.Leaf _ ->
                    error
                      (Format.asprintf
                         "Unpair %d on non-pair: %a"
                         n0
                         print_ty
                         t)
              in

              ( match t with
              | T_record r ->
                  let* ts = unpair (n, r) in
                  return (Stack_ok (T_vector ts))
              | T2 (T_pair _, t1, t2) -> return (Stack_ok (T_vector [t1; t2]))
              | _ ->
                  error
                    (Format.asprintf "Unpair %d on non-pair: %a" n print_ty t)
              )
          | Record_of_tree (_lbls, Stack_failed) -> return Stack_failed
          | Record_of_tree (lbls, t) ->
              let* t = get1 t in
              let rec parse_tree lbls t =
                match ((lbls : _ Binary_tree.t), t) with
                | Leaf x, t -> Ok (Binary_tree.Leaf (x, t))
                | ( Node (l, r)
                  , T_record (Node (Leaf (None, tl), Leaf (None, tr))) ) ->
                    let* l = parse_tree l tl in
                    let* r = parse_tree r tr in
                    Ok (Binary_tree.Node (l, r))
                | Node (l, r), T_record (Node (tl, tr)) ->
                    let* l = parse_tree l (T_record tl) in
                    let* r = parse_tree r (T_record tr) in
                    Ok (Binary_tree.Node (l, r))
                | _ -> error "record_of_tree: wrong shape"
              in
              let* row = parse_tree lbls t in
              ok1 (T_record row)
          | Comment (_, ts) -> return ts
          | Match_record _ | Let_in _ | Lambda _ | Match_variant _ | Loop _
           |If _ | If_none _ | If_left _ | If_cons _ | Map_over _
           |Iter_over _ | Create_contract _ ->
              assert false
        in
        let+ tys = tys in
        {texpr; tys}

let typecheck ~tparameter env e = cata_expr (typecheck_f ~tparameter) e env

let typecheck_precontract {tparameter; tstorage; parameter_and_storage; body} =
  let open Result in
  let env =
    Option.cata
      []
      (fun ps ->
        [ ( ps
          , let t, annot1 = tparameter in
            t_pair ?annot1 t tstorage ) ])
      parameter_and_storage
  in
  let* body = typecheck ~tparameter env body in
  let* () =
    match body.tys with
    | Stack_ok out when compatible out (t_pair (t_list t_operation) tstorage) ->
        Ok ()
    | Stack_failed -> Ok ()
    | s ->
        Error
          (Format.asprintf
             "typecheck_precontract: invalid final stack %a for storage %a"
             Type.print_tys
             s
             Type.print_ty
             tstorage)
  in
  Ok {checked_precontract = {tparameter; tstorage; parameter_and_storage; body}}
