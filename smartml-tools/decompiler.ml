(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics
open Untyped
open Utils
open Control
open Rewriter
open Michel.Expr
open Michel.Type
open Michel.Typing

let line_no = []

let unit = Expr.cst ~line_no:[] Literal.unit

let err_expr msg =
  let err =
    Expr.cst ~line_no (Literal.string (Format.sprintf "[Error: %s]" msg))
  in
  let fail = Command.sp_failwith ~line_no err in
  let l =
    Expr.lambda
      ~line_no
      "error"
      fail
      ~clean_stack:false
      ~with_storage:None
      ~with_operations:false
  in
  Expr.call_lambda ~line_no unit l

let sp_fst = function
  | Basics.{e = ETuple [x; _]} -> x (* FIXME This discards failwith. *)
  | x -> Expr.first ~line_no x

let sp_snd = function
  | Basics.{e = ETuple [_; x]} -> x (* FIXME This discards failwith. *)
  | x -> Expr.second ~line_no x

exception Missing_label

let rec smartML_of_type ~with_records t =
  let smartML_of_type = smartML_of_type ~with_records:false in
  match t with
  | T0 T_nat -> Type.nat ()
  | T0 T_int -> Type.int ()
  | T0 (T_sapling_state {memo}) -> Type.sapling_state (Some memo)
  | T0 (T_sapling_transaction {memo}) -> Type.sapling_transaction (Some memo)
  | T0 t -> Type.mk0 t
  | T_missing _ -> Type.unit
  | T1 (T_option, t) -> Type.option (smartML_of_type t)
  | T1 (t, t1) -> Type.mk1 t (smartML_of_type t1)
  | T2 (T_lambda, t1, t2) ->
      Type.(lambda no_effects) (smartML_of_type t1) (smartML_of_type t2)
  | T2 (T_pair _, t1, t2) -> Type.pair (smartML_of_type t1) (smartML_of_type t2)
  | T2 (T_or _, t1, t2) -> Type.tor (smartML_of_type t1) (smartML_of_type t2)
  | T2 (T_map, t1, t2) ->
      Type.map
        ~big:false
        ~tkey:(smartML_of_type t1)
        ~tvalue:(smartML_of_type t2)
  | T2 (T_big_map, t1, t2) ->
      Type.map ~big:true ~tkey:(smartML_of_type t1) ~tvalue:(smartML_of_type t2)
  | T_record r ->
      let as_tuple =
        Binary_tree.cata (fun (_, t) -> smartML_of_type t) Type.pair
      in
      let f = function
        | Some lbl, t -> (lbl, smartML_of_type t)
        | _ -> raise Missing_label
      in
      if with_records
      then
        try
          let r = Binary_tree.map f r in
          let f (lbl, _) = Layout.{source = lbl; target = lbl} in
          let layout = Binary_tree.map f r in
          Type.record (Unknown.value layout) (Binary_tree.to_list r)
        with
        | Missing_label -> as_tuple r
      else as_tuple r
  | T_variant Binary_tree.(Node (Leaf (Some "None", _), Leaf (Some "Some", t)))
    ->
      Type.option (smartML_of_type t)
  | T_variant Binary_tree.(Node (Leaf (Some "True", _), Leaf (Some "False", _)))
    ->
      Type.bool
  | T_variant r -> Binary_tree.cata (fun (_, t) -> smartML_of_type t) Type.tor r
  | T_vector ts -> Type.tuple (List.map smartML_of_type ts)

let smartML_of_prim0 = function
  | Sender -> Expr.sender ~line_no
  | Source -> Expr.source ~line_no
  | Amount -> Expr.amount ~line_no
  | Balance -> Expr.balance ~line_no
  | Level -> Expr.level ~line_no
  | Now -> Expr.now ~line_no
  | Self _ -> Expr.self ~line_no (* TOOD self entry point *)
  | Self_address -> Expr.self_address ~line_no
  | Chain_id -> Expr.chain_id ~line_no
  | Total_voting_power -> Expr.total_voting_power ~line_no
  | Sapling_empty_state {memo} -> Expr.sapling_empty_state memo
  | Unit_ -> unit
  | None_ _ -> Expr.none ~line_no
  | Nil _ -> Expr.build_list ~line_no ~elems:[]
  | Empty_set _ -> Expr.build_set ~line_no ~entries:[]
  | Empty_map _ -> Expr.build_map ~line_no ~big:false ~entries:[]
  | Empty_bigmap _ -> Expr.build_map ~line_no ~big:true ~entries:[]

let zero = function
  | T0 T_int -> Literal.small_int 0
  | T0 T_nat -> Literal.small_nat 0
  | T0 T_mutez -> Literal.mutez (Bigint.of_int 0)
  | _ -> assert false

let smartML_of_prim1 (x, tys) =
  let tx =
    match tys with
    | Michel.Type.Stack_ok t -> t
    | _ -> assert false
  in
  function
  | Car -> sp_fst x
  | Cdr -> sp_snd x
  | Some_ -> Expr.some ~line_no x
  | Eq -> Expr.bin_op ~op:BEq ~line_no x (Expr.cst ~line_no (zero tx))
  | Abs -> Expr.absE ~line_no x
  | Neg -> Expr.negE ~line_no x
  | Int -> Expr.to_int ~line_no x
  | IsNat -> Expr.is_nat ~line_no x
  | Neq -> Expr.bin_op ~op:BNeq ~line_no x (Expr.cst ~line_no (zero tx))
  | Le -> Expr.bin_op ~op:BLe ~line_no x (Expr.cst ~line_no (zero tx))
  | Lt -> Expr.bin_op ~op:BLt ~line_no x (Expr.cst ~line_no (zero tx))
  | Ge -> Expr.bin_op ~op:BGe ~line_no x (Expr.cst ~line_no (zero tx))
  | Gt -> Expr.bin_op ~op:BGt ~line_no x (Expr.cst ~line_no (zero tx))
  | Not -> Expr.notE ~line_no x
  | Set_delegate -> Expr.set_delegate ~line_no x
  (*
  | Concat1
  | Size
   *)
  | Address -> Expr.to_address ~line_no x
  (*
  | Implicit_account
   *)
  | Contract (a, t) ->
      Expr.contract ~line_no a (smartML_of_type ~with_records:false t) x
  (*
  | Pack
  | Unpack           of ty
  | Hash_key
  | Blake2b
  | Sha256
  | Sha512
  | Set_delegate
 *)
  | p -> err_expr ("prim1: " ^ show_prim1 pp_ty p)

let smartML_of_prim2 x y = function
  | Add -> Expr.bin_op ~op:BAdd ~line_no x y
  | Mul -> Expr.bin_op ~op:(BMul {overloaded = false}) ~line_no x y
  | Sub -> Expr.bin_op ~op:BSub ~line_no x y
  | Lsr -> Expr.lsr_ ~line_no x y
  | Lsl -> Expr.lsl_ ~line_no x y
  | Xor -> Expr.bin_op ~op:BXor ~line_no x y
  | Ediv -> Expr.bin_op ~op:BEDiv ~line_no x y
  | And -> Expr.bin_op ~op:BAnd ~line_no x y
  | Or -> Expr.bin_op ~op:BOr ~line_no x y
  | Cons -> Expr.cons ~line_no x y
  | Sapling_verify_update -> Expr.sapling_verify_update ~line_no x y
  | Pair _ -> Expr.tuple ~line_no [x; y]
  | Compare -> Expr.bin_op ~op:BAdd ~line_no x (Expr.negE ~line_no y)
  (*
 | Concat2
   *)
  | Get -> Expr.some ~line_no (Expr.item ~line_no y x None None)
  (* FIXME return None if element absent *)
  (*
 | Mem
   *)
  | Exec -> Expr.call_lambda ~line_no x y
  | Apply -> Expr.apply_lambda ~line_no x y
  | p -> err_expr ("Decompiler TODO prim2: " ^ show_prim2 pp_ty p)

let smartML_of_prim3 x y (z, tz) =
  let tz =
    match tz with
    | Michel.Type.Stack_ok t -> t
    | _ -> assert false
  in
  function
  | Slice -> Expr.slice ~line_no ~offset:x ~length:y ~buffer:z
  | Update ->
    ( match tz with
    | T2 ((T_map | T_big_map), _tk, _tv) -> Expr.updateMap ~line_no x y z
    | T1 (T_set, _) -> err_expr "Decompiler TODO UPDATE on sets"
    | t -> failwith ("prim3 Update" ^ show_ty t) )
  | Check_signature -> Expr.check_signature ~line_no x y z
  | Transfer_tokens -> Expr.transfer ~line_no ~arg:x ~amount:y ~destination:z
  | Get_and_update -> Expr.get_and_update ~line_no x y z
  | Open_chest -> Expr.open_chest ~line_no x y z

let res = Command.result ~line_no

let rec bind_as ?force_bind x c f =
  let open Basics in
  match c.c with
  | CResult e when force_bind <> Some () -> f e
  | CBind (y, a, b) -> Command.bind ~line_no y a (bind_as ?force_bind x b f)
  | _ -> Command.bind ~line_no (Some x) c (f (Expr.local ~line_no x))

let bind_fresh ?force_bind st =
  bind_as ?force_bind (Michel.Transformer.fresh st "x")

let fmap2 st f c1 c2 =
  bind_fresh st c1 (fun x1 -> bind_fresh st c2 (fun x2 -> res (f x1 x2)))

let fmap3 st f c1 c2 c3 =
  bind_fresh st c1 (fun x1 ->
      bind_fresh st c2 (fun x2 -> bind_fresh st c3 (fun x3 -> res (f x1 x2 x3))))

let rec fmapN st f xs = function
  | [] -> res (f (List.rev xs))
  | c :: cs -> bind_fresh st c (fun x -> fmapN st f (x :: xs) cs)

let fmapN st f = fmapN st f []

let seq = Command.seq ~line_no

let set lhs rhs =
  let open Basics in
  match (lhs.e, rhs.e) with
  | EVar (l, _), EVar (r, _) when l = r -> res unit
  | EVar ("__operations__", _), EList [] ->
      res unit
      (* FIXME This may be overzealous when operations are managed manually. *)
  | _ -> Command.set ~line_no lhs rhs

let _comment = Command.comment ~line_no

type decompiler_state =
  { st : Michel.Transformer.state
  ; env : (string * Basics.Untyped.expr) list }

let rec with_cmd ds ?force_bind e = bind_fresh ?force_bind ds.st (to_cmd ds e)

and with_cmd2 ds e1 e2 k = fmap2 ds.st k (to_cmd ds e1) (to_cmd ds e2)

and with_cmd3 ds e1 e2 e3 k =
  fmap3 ds.st k (to_cmd ds e1) (to_cmd ds e2) (to_cmd ds e3)

and _with_cmdN ds es k = fmapN ds.st k (List.map (to_cmd ds) es)

and to_cmd ds e =
  let on e x = [Command.define_local ~line_no x e true] in
  match e.texpr with
  | Comment (_, e) -> to_cmd ds e
  | Var x ->
    ( match List.assoc_opt x ds.env with
    | Some x -> res x
    | None -> res (Expr.local ~line_no x) )
  | Lit x ->
      ( match x with
      | Unit -> Literal.unit
      | Bytes s -> Literal.bytes s
      | Chain_id s -> Literal.chain_id s
      | String s -> Literal.string s
      | Key_hash s -> Literal.key_hash s
      | Int s -> Literal.int s
      | Nat s -> Literal.nat s
      | Mutez x -> Literal.mutez x
      | Address x -> Literal.address x
      | Timestamp x ->
          let x = Big_int.big_int_of_string x in
          Literal.timestamp x
      | Bool b -> Literal.bool b
      | Bls12_381_g1 s -> Literal.bls12_381_g1 s
      | Bls12_381_g2 s -> Literal.bls12_381_g2 s
      | Bls12_381_fr s -> Literal.bls12_381_fr s
      | Signature s -> Literal.signature s )
      |> Expr.cst ~line_no
      |> res
  | Let_in (P_vector [y1], {texpr = Loop ([c0; x0_1], [vx1], step)}, rest) ->
      let y1 = Option.default (Michel.Transformer.fresh ds.st "y") y1 in
      let vc = Michel.Transformer.fresh ds.st "c" in
      let vc' = Expr.local ~line_no vc in
      let step =
        Michel.Expr.tsubstitute
          [(Option.default "" vx1, {texpr = Michel.Expr.Var y1; tys = x0_1.tys})]
          step
      in
      seq
        [ with_cmd ds c0 (fun e -> Command.define_local ~line_no vc e true)
        ; with_cmd ds x0_1 (fun e -> Command.define_local ~line_no y1 e true)
        ; Command.while_loop
            ~line_no
            (Expr.local ~line_no vc)
            (assign_to ds [Some vc'; Some (Expr.local ~line_no y1)] step)
        ; to_cmd ds rest ]
  | Let_in
      ( P_vector [y1; y2]
      , {texpr = Loop ([c0; x0_1; x0_2], [vx1; vx2], step)}
      , rest ) ->
      let y1 = Option.default (Michel.Transformer.fresh ds.st "y") y1 in
      let y2 = Option.default (Michel.Transformer.fresh ds.st "y") y2 in
      let vc = Michel.Transformer.fresh ds.st "c" in
      let vc' = Expr.local ~line_no vc in
      let step =
        Michel.Expr.tsubstitute
          [ (Option.default "" vx1, {texpr = Michel.Expr.Var y1; tys = x0_1.tys})
          ; (Option.default "" vx2, {texpr = Michel.Expr.Var y2; tys = x0_2.tys})
          ]
          step
      in
      seq
        [ with_cmd ds c0 (fun e -> Command.define_local ~line_no vc e true)
        ; with_cmd ds x0_1 (fun e -> Command.define_local ~line_no y1 e true)
        ; with_cmd ds x0_2 (fun e -> Command.define_local ~line_no y2 e true)
        ; Command.while_loop
            ~line_no
            (Expr.local ~line_no vc)
            (assign_to
               ds
               [ Some vc'
               ; Some (Expr.local ~line_no y1)
               ; Some (Expr.local ~line_no y2) ]
               step)
        ; to_cmd ds rest ]
  | Let_in (P_var x, e1, e2) ->
      bind_fresh ds.st (to_cmd ds e1) (fun e ->
          seq (Option.cata [] (on e) x @ [to_cmd ds e2]))
  | Let_in (P_vector [x; y], e1, e2) ->
      (* TODO Do this as a Michel-to-Michel transformation
         instead. Translate vectors to n-tuples. *)
      bind_fresh ds.st (to_cmd ds e1) (fun xy ->
          seq
            ( Option.cata [] (on (sp_fst xy)) x
            @ Option.cata [] (on (sp_snd xy)) y
            @ [to_cmd ds e2] ))
  | Match_record (Binary_tree.Leaf x, e1, e2) ->
      bind_fresh ds.st (to_cmd ds e1) (fun e ->
          seq (Option.cata [] (on e) x @ [to_cmd ds e2]))
  | Match_record (Binary_tree.(Node (Leaf x, Leaf y)), e1, e2) ->
      bind_fresh ds.st (to_cmd ds e1) (fun xy ->
          seq
            ( Option.cata [] (on (sp_fst xy)) x
            @ Option.cata [] (on (sp_snd xy)) y
            @ [to_cmd ds e2] ))
  | If (c, l, r) -> decompile_if ds to_cmd c l r
  | Match_variant (scrutinee, clauses) ->
      decompile_match_variant ds to_cmd scrutinee clauses
  | Record Binary_tree.(Node (Leaf (None, e1), Leaf (None, e2))) ->
      with_cmd2 ds e1 e2 (fun e1 e2 -> Expr.tuple ~line_no [e1; e2])
  | Record r ->
      let f = function
        | Some lbl, t -> (lbl, t)
        | _ -> raise Missing_label
      in
      ( try
          let r = List.map f (Binary_tree.to_list r) in
          let rec mk_record acc = function
            | [] -> res (Expr.record ~line_no acc)
            | (lbl, e) :: r ->
                with_cmd ds e (fun x -> mk_record ((lbl, x) :: acc) r)
          in
          mk_record [] r
        with
      | Missing_label -> assert false )
  | Prim0 p -> res (smartML_of_prim0 p)
  | Prim1 (Eq, {texpr = Prim2 (Compare, e1, e2)}) ->
      with_cmd2 ds e1 e2 (Expr.bin_op ~op:BEq ~line_no)
  | Prim1 (Lt, {texpr = Prim2 (Compare, e1, e2)}) ->
      with_cmd2 ds e1 e2 (Expr.bin_op ~op:BLt ~line_no)
  | Prim1 (Le, {texpr = Prim2 (Compare, e1, e2)}) ->
      with_cmd2 ds e1 e2 (Expr.bin_op ~op:BLe ~line_no)
  | Prim1 (Gt, {texpr = Prim2 (Compare, e1, e2)}) ->
      with_cmd2 ds e1 e2 (Expr.bin_op ~op:BGt ~line_no)
  | Prim1 (Ge, {texpr = Prim2 (Compare, e1, e2)}) ->
      with_cmd2 ds e1 e2 (Expr.bin_op ~op:BGe ~line_no)
  | Prim1_fail (Failwith, e) -> with_cmd ds e (Command.sp_failwith ~line_no)
  | Prim1 (p, e) -> with_cmd ds e (fun x -> res (smartML_of_prim1 (x, e.tys) p))
  | Prim2 (p, e1, e2) ->
      with_cmd2 ds e1 e2 (fun x1 x2 -> smartML_of_prim2 x1 x2 p)
  | Prim3 (p, e1, e2, e3) ->
      with_cmd3 ds e1 e2 e3 (fun x1 x2 x3 ->
          smartML_of_prim3 x1 x2 (x3, e3.tys) p)
  | Proj_field (fld, e) ->
      with_cmd ds e (fun x -> res (Expr.attr ~line_no x ~name:fld))
  | Variant (_, ctxt, x) ->
      with_cmd ds x (fun x ->
          let open Binary_tree in
          let rec of_ctxt = function
            | Node_left (c, _t) -> Expr.left ~line_no (of_ctxt c)
            | Node_right (_t, c) -> Expr.right ~line_no (of_ctxt c)
            | Hole -> x
          in
          res (of_ctxt ctxt))
  | Vector [] -> res unit
  | Vector [x] -> to_cmd ds x
  | Vector [x; y] -> with_cmd2 ds x y (fun x y -> Expr.tuple ~line_no [x; y])
  | Vector [x; y; z] ->
      with_cmd3 ds x y z (fun x y z -> Expr.tuple ~line_no [x; y; z])
  | Nth (i, x) -> with_cmd ds x (fun x -> res (Expr.proj ~line_no i x))
  | _ ->
      Command.sp_failwith
        ~line_no
        (err_expr (Format.asprintf "to_cmd: %a" print_expr (erase_types e)))

and assign_to ds lhs rhs =
  match (lhs, rhs.texpr) with
  | _, Prim1_fail (Failwith, _) -> to_cmd ds rhs
  | [x; y], Michel.Expr.Prim2 (Pair _, x', y') ->
      seq [assign_to ds [x] x'; assign_to ds [y] y']
  | xs, Michel.Expr.Let_in (P_var y, t, u) ->
      let c =
        match y with
        | None -> to_cmd ds t
        | Some y ->
            with_cmd ds t (fun e -> Command.define_local ~line_no y e true)
      in
      seq [c; assign_to ds xs u]
  | xs, If (c, l, r) -> decompile_if ds (flip assign_to xs) c l r
  | xs, Match_variant (scrutinee, clauses) ->
      decompile_match_variant ds (flip assign_to xs) scrutinee clauses
  | [Some x], Vector [x'] -> with_cmd ds x' (fun x' -> seq [set x x'])
  | [Some x], _ -> with_cmd ds rhs (set x)
  | [Some x; Some y], Vector [x'; y'] ->
      with_cmd ds x' (fun x' ->
          seq [set x x'; with_cmd ds y' (fun y' -> set y y')])
  | [Some x; Some y], _ ->
      with_cmd ds rhs (fun rhs -> seq [set x (sp_fst rhs); set y (sp_snd rhs)])
  | [Some x; Some y; Some z], Vector [x'; y'; z'] ->
      with_cmd ds x' (fun x' ->
          seq
            [ set x x'
            ; with_cmd ds y' (fun y' ->
                  seq [set y y'; with_cmd ds z' (fun z' -> set z z')]) ])
  | [Some x; Some y; Some z], _ ->
      with_cmd ds rhs (fun rhs ->
          seq
            [ set x (Expr.proj ~line_no 0 rhs)
            ; set y (Expr.proj ~line_no 1 rhs)
            ; set z (Expr.proj ~line_no 2 rhs) ])
  | _, _ ->
      failwith
        (Format.asprintf
           "assign_to: lhs %a rhs %a"
           (List.pp (Option.pp Expr.pp))
           lhs
           Michel.Expr.print_expr
           (Michel.Expr.erase_types rhs))

and decompile_if ds f s l r =
  match (f ds l, f ds r) with
  | {c = CResult l}, {c = CResult r} ->
      with_cmd ds s (fun s -> res (Expr.eif ~line_no s l r))
  | l, r -> with_cmd ds s (fun s -> Command.ifte ~line_no s l r)

and decompile_match_variant ds f s =
  let open Binary_tree in
  function
  | Node
      (Leaf {cons = cl; var = vl; rhs = l}, Leaf {cons = cr; var = vr; rhs = r})
    ->
      let vl = Option.default (Michel.Transformer.fresh ds.st "_l") vl in
      let vr = Option.default (Michel.Transformer.fresh ds.st "_r") vr in
      let env =
        (vl, Expr.match_cons ~line_no vl)
        :: (vr, Expr.match_cons ~line_no vr)
        :: ds.env
      in
      let ds = {ds with env} in
      let cl, cr =
        match (cl, cr) with
        | Some "None", Some "Some" -> ("None", "Some")
        | _ -> ("Left", "Right")
      in
      with_cmd ds s (fun s ->
          Command.mk_match ~line_no s [(cl, vl, f ds l); (cr, vr, f ds r)])
  | b ->
      let b = Binary_tree.to_list b in
      let g {cons; var; rhs} =
        let var = Option.default (Michel.Transformer.fresh ds.st "_x") var in
        let env = (var, Expr.match_cons ~line_no var) :: ds.env in
        let cons =
          match cons with
          | None -> assert false
          | Some cons -> cons
        in
        (cons, var, f {ds with env} rhs)
      in
      with_cmd ds s (fun s -> Command.mk_match ~line_no s (List.map g b))

let row_to_variant = function
  | Binary_tree.Leaf (_, t) -> t
  | t -> t_variant t

(** Recognizes the stylized output of Transformer.smartMLify. *)
let match_smartMLified body =
  match body.texpr with
  | Let_in
      ( P_var (Some "__parameter")
      , _
      , {texpr = Let_in (P_var (Some "__storage"), _, body)} ) ->
      body
  | _ -> assert false

(** Overlays any top-level match clauses with tparameter to infer each
   entry point's parameter type. *)
let detect_entry_points ~tparameter body =
  let body = match_smartMLified body in
  let tparameter, _annot = tparameter in
  (* TODO single entry point annotation *)
  match (body.texpr, tparameter) with
  | Match_variant ({texpr = Var "__parameter"}, clauses), T_variant row ->
    (* TODO Limit to the case where __parameter has no other occurrences. *)
    ( match Binary_tree.matches clauses row with
    | None -> assert false (* Match structure must respect tparameter. *)
    | Some xs -> List.map (map_snd row_to_variant) (Binary_tree.to_list xs) )
  | _ -> [({cons = None; var = Some "__parameter"; rhs = body}, tparameter)]

let smartML_of_michel config {checked_precontract = {tparameter; tstorage; body}}
    =
  let open Basics in
  let st = Michel.Transformer.{var_counter = ref 0} in
  let clauses_with_params = detect_entry_points ~tparameter body in
  let tstorage_explicit = Some (smartML_of_type ~with_records:true tstorage) in
  let storage = Expr.storage ~line_no in
  let mk_entry_point i ({cons; var; rhs}, tparameter_ep) =
    let tparameter_ep = smartML_of_type ~with_records:false tparameter_ep in
    let params = Expr.params ~line_no in
    let env =
      Option.cata [] (fun v -> [(v, params)]) var @ [("__storage", storage)]
    in
    let lhs = [Some (Expr.operations ~line_no); Some storage] in
    let body = assign_to {st; env} lhs rhs in
    let body = seq [Command.set_type ~line_no params tparameter_ep; body] in
    let body = embellish ~config body in
    let module Printer = (val Printer.get config : Printer.Printer) in
    if false
    then
      Format.printf
        "entry_point: %a\n%s\n"
        (Option.pp String.pp)
        cons
        (Printer.command_to_string body);
    let channel = Option.default (Printf.sprintf "ep%d" i) cons in
    { channel
    ; tparameter_ep = `Present
    ; originate = true
    ; lazify = None
    ; lazy_no_code = None
    ; line_no = []
    ; body
    ; tparameter_ep_derived = U }
  in
  let entry_points = List.mapi mk_entry_point clauses_with_params in
  let contract =
    { template_id = None
    ; balance = None
    ; storage = None
    ; baker = None
    ; tstorage_explicit
    ; entry_points
    ; entry_points_layout = None
    ; unknown_parts = None
    ; flags = []
    ; private_variables = []
    ; metadata = []
    ; views = []
    ; derived = U }
  in
  let {tcontract} = Checker.check_contract config {contract} in
  let state =
    { balance = Bigint.of_int 0
    ; storage = None
    ; baker = None
    ; lazy_entry_points = []
    ; metadata = [] }
  in
  {template = {tcontract}; state}
