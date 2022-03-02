(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils
open Control
open Michel.Expr
open Michel.Type
open Michel.Transformer

let of_mtype_f : ty Michelson.mtype_f -> ty = function
  | MT0 t -> T0 t
  | MT1 (T_option, t) -> t_option t
  | MT1 (t, t1) -> T1 (t, t1)
  | MT2 (T_pair {annot_fst; annot_snd}, fst, snd) ->
      let f = function
        | None, T_record r -> r
        | lbl, t -> Leaf (lbl, t)
      in
      t_record_node (f (annot_fst, fst)) (f (annot_snd, snd))
  | MT2 (T_or {annot_left; annot_right}, left, right) ->
      let f = function
        | None, T_variant r -> r
        | lbl, t -> Leaf (lbl, t)
      in
      t_variant_node (f (annot_left, left)) (f (annot_right, right))
  | MT2 (t, t1, t2) -> T2 (t, t1, t2)
  | MT_var s -> t_missing s

let of_mtype = Michelson.cata_mtype_stripped of_mtype_f

type line =
  | Comment of string list
  | Binding of pattern * expr

type stack_ok = string list [@@deriving show {with_path = false}]

type stack =
  | Stack_ok     of stack_ok
  | Stack_failed
[@@deriving show {with_path = false}]

type code = stack_ok -> line list * stack

let var_vector_expr xs = vector (List.map var xs)

let rec lets_some bs r =
  match bs with
  | [] -> r
  | Comment c :: bs -> comment c (lets_some bs r)
  | Binding (p, e) :: bs -> let_in p e (lets_some bs r)

let p_vector_some xs = P_vector (List.map Option.some xs)

let op_vector st (n_in, n_out) op : code =
 fun xs ->
  let hi, lo = List.split_at ~err:"op_vector" n_in xs in
  let hi' = List.(map (fresh st) (replicate n_out "x")) in
  ( [ Binding
        (P_vector (List.map Option.some hi'), {expr = op (List.map var hi)}) ]
  , Stack_ok (hi' @ lo) )

let op_var st n_in op xs =
  let hi, lo = List.split_at ~err:"op_var" n_in xs in
  let out = fresh st "x" in
  ( [Binding (P_var (Some out), {expr = op (List.map var hi)})]
  , Stack_ok (out :: lo) )

let op_vector_full st (n_in, n_out) op : code =
 fun xs ->
  let n_out' = List.length xs - n_in + n_out in
  let xs' = List.(map (fun _ -> fresh st "x") (replicate n_out' "x")) in
  ( [ Binding
        (P_vector (List.map Option.some xs'), {expr = op (List.map var xs)}) ]
  , Stack_ok xs' )

let op0 st op =
  op_var st 0 (function
      | [] -> op
      | _ -> assert false)

let op1 st op =
  op_var st 1 (function
      | [x] -> op x
      | _ -> assert false)

let op2 st op =
  op_var st 2 (function
      | [x1; x2] -> op x1 x2
      | _ -> assert false)

let op3 st op =
  op_var st 3 (function
      | [x1; x2; x3] -> op x1 x2 x3
      | _ -> assert false)

let lets_some_last bs =
  match List.unsnoc bs with
  | bs, Binding (_, r) -> lets_some bs r
  | _ -> assert false

let p_dummy = P_vector []

let combine_branches st (bs1, s1) (bs2, s2) =
  match (s1, s2) with
  | Stack_ok s1, Stack_ok s2 when List.length s1 = List.length s2 ->
      let s = freshen st "s" s1 in
      ( p_vector_some s
      , lets_some bs1 (var_vector_expr s1)
      , lets_some bs2 (var_vector_expr s2)
      , Stack_ok s )
  | Stack_failed, Stack_ok s2 ->
      let s = freshen st "s" s2 in
      ( p_vector_some s
      , lets_some_last bs1
      , lets_some bs2 (var_vector_expr s2)
      , Stack_ok s )
  | Stack_ok s1, Stack_failed ->
      let s = freshen st "s" s1 in
      ( p_vector_some s
      , lets_some bs1 (var_vector_expr s1)
      , lets_some_last bs2
      , Stack_ok s )
  | Stack_failed, Stack_failed ->
      (p_dummy, lets_some_last bs1, lets_some_last bs2, Stack_failed)
  | s1, s2 ->
      let e1 = lets_some bs1 (var "...") in
      let e2 = lets_some bs2 (var "...") in
      failwith
        (Format.asprintf
           "combine_branches:\n  %a\n  %a\n\n%a\n%a"
           pp_stack
           s1
           pp_stack
           s2
           print_expr
           e1
           print_expr
           e2)

let if_ st (l : code) (r : code) : code = function
  | cond :: tail ->
      let s, l, r, stack = combine_branches st (l tail) (r tail) in
      ([Binding (s, if_ (var cond) l r)], stack)
  | [] -> failwith "if_"

let if_left st (l : code) (r : code) : code = function
  | cond :: tail ->
      let xl = fresh st "l" in
      let xr = fresh st "r" in
      let s, l, r, stack =
        combine_branches st (l (xl :: tail)) (r (xr :: tail))
      in
      ([Binding (s, if_left (var cond) (Some xl, l) (Some xr, r))], stack)
  | [] -> failwith "if_left"

let if_none st (l : code) (r : code) : code = function
  | cond :: tail ->
      let some = fresh st "s" in
      let s, l, r, stack = combine_branches st (l tail) (r (some :: tail)) in
      ([Binding (s, if_none (var cond) l (Some some, r))], stack)
  | [] -> failwith "if_none"

let if_cons st (l : code) (r : code) : code = function
  | cond :: tail ->
      let x = fresh st "x" in
      let xs = fresh st "xs" in
      let s, l, r, stack = combine_branches st (l (x :: xs :: tail)) (r tail) in
      ([Binding (s, if_cons (var cond) (Some x, Some xs, l) r)], stack)
  | [] -> failwith "if_cons"

let loop st (body : code) : code =
 fun init ->
  match freshen st "s" init with
  | [] -> assert false
  | _ :: s' ->
      let bs, s_out = body s' in
      ( match s_out with
      | Stack_ok step ->
          let r = freshen st "r" (List.tl init) in
          assert (List.length step = List.length init);
          ( [ Binding
                ( p_vector_some r
                , loop
                    (List.map var init)
                    (List.map Option.some s')
                    (lets_some bs (var_vector_expr step)) ) ]
          , Stack_ok r )
      | Stack_failed -> (bs, Stack_failed) )

let map_over st (body : code) : code = function
  | [] -> failwith "map_over"
  | input ->
      let s = freshen st "s" input in
      let bs, s_out = body s in
      ( match s_out with
      | Stack_ok step ->
          let r = freshen st "r" input in
          ( [ Binding
                ( p_vector_some r
                , map_over
                    (List.map var input)
                    (List.map Option.some s)
                    (lets_some bs (var_vector_expr step)) ) ]
          , Stack_ok r )
      | Stack_failed -> (bs, Stack_failed) )

let iter_over st (body : code) : code = function
  | [] -> failwith "iter_over"
  | input ->
      let s = freshen st "s" input in
      let bs, s_out = body s in
      ( match s_out with
      | Stack_ok step ->
          let r = freshen st "r" step in
          ( [ Binding
                ( p_vector_some r
                , iter_over
                    (List.map var input)
                    (List.map Option.some s)
                    (lets_some bs (var_vector_expr step)) ) ]
          , Stack_ok r )
      | Stack_failed -> (bs, Stack_failed) )

let lambda st a b body s =
  let x = fresh st "x" in
  let bs, s' = body [x] in
  match s' with
  | Stack_ok [y] ->
      let expr =
        Lambda (Some x, of_mtype a, of_mtype b, lets_some bs (var y))
      in
      op0 st expr s
  | Stack_ok _ -> assert false
  | Stack_failed ->
      let body = lets_some_last bs in
      let expr = Lambda (Some x, of_mtype a, of_mtype b, body) in
      op0 st expr s

let dip n body s =
  let hi, lo = Base.List.split_n s n in
  let bs, lo = body lo in
  ( bs
  , match lo with
    | Stack_ok lo -> Stack_ok (hi @ lo)
    | s -> s )

let sp_failwith : code = function
  | e :: _ -> ([Binding (p_dummy, prim1_fail Failwith (var e))], Stack_failed)
  | _ -> assert false

let never : code = function
  | e :: _ -> ([Binding (p_dummy, prim1_fail Never (var e))], Stack_failed)
  | _ -> assert false

let drop st n = op_vector_full st (n, 0) (fun xs -> Stack_op (Drop n, xs))

let of_mliteral ~t literal =
  let t = Result.get_ok_exn t in
  let open Michelson in
  let open Michel.Expr in
  match (t.mt, (literal : _ Michelson.literal_f)) with
  | MT0 T_unit, Unit -> lit Unit
  | MT0 T_bool, Bool true -> true_
  | MT0 T_bool, Bool false -> false_
  | MT2 (T_pair {annot_fst; annot_snd}, _, _), Pair (l1, l2) ->
      pair annot_fst annot_snd l1 l2
  | MT1 (T_option, t), None_ -> none (of_mtype t)
  | MT0 T_bytes, Bytes b -> lit (Bytes b)
  | MT0 T_string, String b -> lit (String b)
  | MT0 T_key_hash, String b -> lit (Key_hash b)
  | MT0 T_address, String x -> lit (Address x)
  | MT0 T_timestamp, String x -> lit (Timestamp x)
  | MT0 T_nat, Int i -> lit (Nat i)
  | MT0 T_int, Int i -> lit (Int i)
  | MT0 T_mutez, Int i -> lit (Mutez i)
  | MT0 T_bls12_381_fr, Bytes b -> lit (Bls12_381_fr b)
  | MT0 T_bls12_381_g1, Bytes b -> lit (Bls12_381_g1 b)
  | MT0 T_bls12_381_g2, Bytes b -> lit (Bls12_381_g2 b)
  | MT0 T_signature, String x -> lit (Signature x)
  | MT1 (T_option, _), Some_ x -> some x
  | MT1 (T_list, t), Seq xs -> michel_list (of_mtype t) xs
  | MT1 (T_set, t), Seq xs -> michel_set (of_mtype t) xs
  | MT2 (T_map, tk, tv), Seq [] -> michel_map (of_mtype tk) (of_mtype tv) []
  | MT2 (T_map, _tk, _tv), Seq _ -> assert false
  | MT2 (T_map, tk, tv), AnyMap xs -> michel_map (of_mtype tk) (of_mtype tv) xs
  | MT2 (T_or {annot_left; annot_right}, _, tr), Left x ->
      left annot_left annot_right (of_mtype tr) x
  | MT2 (T_or {annot_left; annot_right}, tl, _), Right x ->
      right annot_left annot_right (of_mtype tl) x
  | _ -> assert false

let lets_some_var bs s =
  match s with
  | Stack_ok [r] -> lets_some bs (var r)
  | Stack_failed -> lets_some_last bs
  | _ -> assert false

let decompile_f ~stack_in:_ ~stack_out:_ st :
    (code, _) Michelson.instr_f -> code =
  let prim0 st f = op0 st (Prim0 f) in
  let prim1 st f = op1 st (fun x -> Prim1 (f, x)) in
  let prim2 st f = op2 st (fun x1 x2 -> Prim2 (f, x1, x2)) in
  let prim3 st f = op3 st (fun x1 x2 x3 -> Prim3 (f, x1, x2, x3)) in
  function
  | MI0 p -> prim0 st (map_prim0 of_mtype p)
  | MI1 Read_ticket ->
      op_vector st (1, 2) (function
          | [v] -> Prim1 (Read_ticket, v)
          | _ -> assert false)
  | MI1 p -> prim1 st (map_prim1 of_mtype p)
  | MI1_fail Failwith -> sp_failwith
  | MI1_fail Never -> never
  | MI2 p -> prim2 st (map_prim2 of_mtype p)
  | MI3 Get_and_update ->
      op_vector st (3, 2) (function
          | [m; k; v] -> Prim3 (Get_and_update, m, k, v)
          | _ -> assert false)
  | MI3 p -> prim3 st p
  | MIerror _ -> assert false
  | MIcomment c -> fun s -> ([Comment c], Stack_ok s)
  | MImich {name = "CONCAT"; typesIn = [_]; typesOut = [_]} -> prim1 st Concat1
  | MImich {name = "CONCAT"; typesIn = [_; _]; typesOut = [_]} ->
      prim2 st Concat2
  | MImich _x as instr ->
      failwith
        ( "Michel_decompiler TODO: inline Michelson: "
        ^ Michelson.show_instr {instr} )
  | MIdip instr -> dip 1 instr
  | MIdipn (n, instr) -> dip n instr
  | MIloop body -> loop st body
  | MIloop_left _body -> failwith "Michel_decompiler TODO: LOOP_LEFT"
  | MIiter body -> iter_over st body
  | MImap body -> map_over st body
  | MIdrop -> drop st 1
  | MIdropn n -> drop st n
  | MIdup n -> op_vector st (n, n + 1) (fun xs -> Stack_op (Dup n, xs))
  | MIdig n -> op_vector st (n + 1, n + 1) (fun xs -> Stack_op (Dig n, xs))
  | MIdug n -> op_vector st (n + 1, n + 1) (fun xs -> Stack_op (Dug n, xs))
  | MIif (a, b) -> if_ st a b
  | MIif_left (a, b) -> if_left st a b
  | MIif_none (a, b) -> if_none st a b
  | MIif_cons (a, b) -> if_cons st a b
  | MIpush (_, l) -> op0 st l.expr
  | MIseq xs ->
      fun s ->
        List.fold_left
          (fun (bs, s) f ->
            match s with
            | Stack_ok s ->
                let bs', s = f s in
                (bs @ bs', s)
            | s -> (bs, s))
          ([], Stack_ok s)
          xs
  | MIswap -> op_vector st (2, 2) (fun xs -> Stack_op (Swap, xs))
  | MIfield p ->
      let rec field p x =
        match p with
        | [] -> x
        | Michelson.A :: p -> field p (Michel.Expr.prim1 Car x)
        | Michelson.D :: p -> field p (Michel.Expr.prim1 Cdr x)
      in
      op1 st (fun x -> (field p x).expr)
  | MIunpair xs when List.exists not xs ->
      failwith "decompiler: UNPAIR with gaps"
  | MIunpair xs ->
      let n = List.length xs in
      op_vector st (1, n) (function
          | [x] -> Unpair (n, x)
          | _ -> assert false)
  | MIpairn n -> op_var st n (fun x -> (tuple x).expr)
  | MIsetField _ -> failwith "decompiler: SET_FIELD"
  | MIconcat_unresolved -> failwith "decompile: unresolved CONCAT arity"
  | MIlambda (a, b, x) -> lambda st a b x
  | MIcreate_contract {tparameter; tstorage; code} ->
      op_vector st (3, 2) (function
          | [baker; balance; storage] ->
              let ps = fresh st "ps" in
              let bs, s = code [ps] in
              let tparameter = map_fst of_mtype tparameter in
              let tstorage = of_mtype tstorage in
              let body = lets_some_var bs s in
              let c =
                {tparameter; tstorage; parameter_and_storage = Some ps; body}
              in
              Create_contract (c, baker, balance, storage)
          | _ -> assert false)

let decompile st i =
  Michelson.cata_tinstr {f_tinstr = decompile_f st; f_tliteral = of_mliteral} i

let decompile_contract st ({tparameter; tstorage; code} : Michelson.tcontract) =
  let tparameter = map_fst of_mtype tparameter in
  let tstorage = of_mtype tstorage in
  let ps = fresh st "ps" in
  ( match Michelson.has_error ~accept_missings:false code with
  | [] -> ()
  | _ -> failwith "Michel_decompiler: refusing to run on erroneous contract" );
  let bs, s = decompile st code [ps] in
  let body = lets_some_var bs s in
  {tparameter; tstorage; parameter_and_storage = Some ps; body}
