(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Control
open Basics
open Typed
open Utils.Misc
open Primitives

type in_contract =
  | No_contract of smart_except list
  | In_contract of contract_id * value

type context_ =
  { sender : string option
  ; source : string option
  ; chain_id : string
  ; time : Bigint.t
  ; amount : Bigint.t
  ; level : Bigint.t
  ; voting_powers : (string * Bigint.t) list
  ; line_no : line_no
  ; debug : bool
  ; contract_id : in_contract }

type context = context_

let build_context
    ?(*    ~contract_id*)
    sender
    ?source
    ?(chain_id = "")
    ~time
    ~amount
    ~level
    ~voting_powers
    ~line_no
    ~debug
    () =
  { sender
  ; source
  ; chain_id
  ; time
  ; amount
  ; level
  ; voting_powers
  ; line_no
  ; debug
  ; contract_id = No_contract [] }

let context_sender c = c.sender

let context_time c = c.time

let context_amount c = c.amount

let context_contract_id c =
  match c.contract_id with
  | No_contract _ -> None
  | In_contract (id, _) -> Some id

let context_line_no c = c.line_no

let context_debug c = c.debug

(** Internal exception: raised by {!interpret_command} and caught by {!interpret_contract}. *)

type root =
  | R_storage
  | R_local   of string

type step =
  | S_attr      of string
  | S_item_map  of value
  | S_item_list of int

type path =
  { root : root
  ; steps : step list }

let string_of_step config =
  let module Printer = (val Printer.get config : Printer.Printer) in
  function
  | S_attr attr -> "." ^ attr
  | S_item_list i -> Printf.sprintf "[%d]" i
  | S_item_map key -> Printf.sprintf "[%s]" (Printer.value_to_string key)

let _string_of_steps config steps =
  String.concat "" (List.map (string_of_step config) steps)

let extend_path {root; steps} steps' = {root; steps = steps @ steps'}

let ( @. ) = Lens.( @. )

type variable_value =
  | Iter     of (value * path option)
  | Heap_ref of value ref
  | Constant of value

type env =
  { context : context_
  ; primitives : (module Primitives)
  ; variables : (string * variable_value) list ref
  ; steps : Execution.step list ref
  ; private_variables : (string * texpr) list
  ; current_locals : string list ref
  ; scenario_state : scenario_state
  ; config : Config.t }

let get_current_contract_id env message : contract_id =
  match env.context.contract_id with
  | No_contract l ->
      raise
        (SmartExcept
           [`Text "Missing environment to compute"; `Text message; `Br; `Rec l])
  | In_contract (id, _) -> id

let get_current_parameters env message =
  match env.context.contract_id with
  | No_contract l ->
      raise
        (SmartExcept
           [`Text "Missing environment to compute"; `Text message; `Br; `Rec l])
  | In_contract (_, params) -> params

let get_current_instance env message : instance =
  let id = get_current_contract_id env message in
  match Hashtbl.find_opt env.scenario_state.contracts id with
  | None ->
      let module Printer = (val Printer.get env.config : Printer.Printer) in
      raise
        (SmartExcept
           [ `Text
               (Printf.sprintf
                  "Missing contract in scenario %s"
                  (Printer.string_of_contract_id id))
           ; `Line env.context.line_no ])
  | Some contract -> contract

let lens_current_instance env message : (unit, instance) Lens.t =
  let module Printer = (val Printer.get env.config : Printer.Printer) in
  let id = get_current_contract_id env message in
  Lens.hashtbl_at id env.scenario_state.contracts
  @. Lens.some
       ~err:
         (Printf.sprintf
            "Missing contract in scenario %s"
            (Printer.string_of_contract_id id))

let lens_heap_ref =
  Lens.bi
    (function
      | Heap_ref x -> x
      | _ -> failwith "not a heap reference")
    (fun x -> Heap_ref x)

let lens_of_root env = function
  | R_storage ->
      let l_storage (x : contract_state) =
        Lens.{focus = x.storage; zip = (fun storage -> {x with storage})}
      in
      let l_state (x : instance) =
        Lens.{focus = x.state; zip = (fun state -> {x with state})}
      in
      lens_current_instance env "variable"
      @. Lens.make l_state
      @. Lens.make l_storage
      @. Lens.some ~err:"no storage"
  | R_local name ->
      Lens.ref env.variables
      @. Lens.assoc_exn ~equal:( = ) ~key:name ~err:("local not found: " ^ name)
      @. lens_heap_ref
      @. Lens.unref

(** Convert a step into a lens that points to an optional value,
   accompanied by an error message for when the value is missing. *)
let lens_of_step ~config ~line_no =
  let module Printer = (val Printer.get config : Printer.Printer) in
  let line_no = string_of_line_no line_no in
  function
  | S_item_list i ->
      ( Value.lens_list_nth i
      , Printf.sprintf "Line %s: Index '%d' out of range." line_no i )
  | S_item_map key ->
      ( Value.lens_map_at ~key
      , Printf.sprintf
          "Line %s: Key '%s' not found."
          line_no
          (Printer.value_to_string key) )
  | S_attr attr ->
      ( Value.lens_record_at ~attr
      , Printf.sprintf "Line %s: Impossible: attribute not found" line_no )

let rec lens_of_steps ~config ~line_no acc = function
  | [s] ->
      let l, err = lens_of_step ~config ~line_no s in
      (acc @. l, err)
  | s :: steps ->
      let l, err = lens_of_step ~config ~line_no s in
      lens_of_steps ~config ~line_no (acc @. l @. Lens.some ~err) steps
  | [] -> (Lens.option ~err:"lens_of_steps", "lens_of_steps")

let lens_of_path ~config ~line_no env {root; steps} =
  let l, err = lens_of_steps ~config ~line_no Lens.id steps in
  (lens_of_root env root @. l, err)

let addStep ?(sub_steps = ref []) upperSteps env c elements =
  let buildStep () =
    let instance = get_current_instance env "not in contract" in
    let storage = Option.get ~msg:"no storage" instance.state.storage in
    let balance = instance.state.balance in
    { Execution.iters =
        Base.List.filter_map
          ~f:(function
            | n, Iter (v, _) -> Some (n, (v, None))
            | _ -> None)
          !(env.variables)
    ; locals =
        Base.List.filter_map
          ~f:(function
            | n, Heap_ref x -> Some (n, !x)
            | _ -> None)
          !(env.variables)
    ; storage
    ; balance
    ; operations = [] (*env.operations*)
    ; command = c
    ; substeps = sub_steps
    ; elements }
  in
  if env.context.debug then upperSteps := buildStep () :: !upperSteps

let add_variable name value vars =
  match List.assoc_opt name vars with
  | Some _ -> failwith ("Variable '" ^ name ^ "' already defined")
  | None -> (name, value) :: vars

let assert_unit ~config c =
  let module Printer = (val Printer.get config : Printer.Printer) in
  function
  | {v = Literal Unit} -> ()
  | _ ->
      raise
        (SmartExcept
           [ `Text "Command doesn't return unit"
           ; `Br
           ; `Text (Printer.tcommand_to_string c)
           ; `Br
           ; `Line c.line_no ])

let assert_unknown pp x v =
  if not (Unknown.set x v) then raise (SmartExcept (pp ()))

let get_constant_value env hash =
  match Hashtbl.find_opt env.scenario_state.constants hash with
  | Some value -> value
  | None ->
      raise
        (SmartExcept [`Text "Could not find any constant with hash:"; `Text hash])

let update_contract_address ?address scenario_state id =
  let address =
    match address with
    | None -> Aux.address_of_contract_id ~html:false id None
    | Some address -> address
  in
  Hashtbl.replace scenario_state.addresses id address;
  Hashtbl.replace scenario_state.rev_addresses address id;
  address

let get_contract_address scenario_state id =
  match Hashtbl.find_opt scenario_state.addresses id with
  | Some addr -> addr
  | None -> update_contract_address scenario_state id

let update_contract_address ?address scenario_state id =
  let (_ : string) = update_contract_address ?address scenario_state id in
  ()

let interpret_mprim0 env e0 : _ Michelson_base.Primitive.prim0 -> _ = function
  | Sender ->
    ( match env.context.sender with
    | Some address -> Value.literal (Literal.address address)
    | None -> raise (SmartExcept [`Text "Sender is undefined"; `Expr e0]) )
  | Source ->
    ( match env.context.source with
    | Some address -> Value.literal (Literal.address address)
    | None -> raise (SmartExcept [`Text "Source is undefined"; `Expr e0]) )
  | Amount -> Value.mutez env.context.amount
  | Balance -> Value.mutez (get_current_instance env "balance").state.balance
  | Level -> Value.nat env.context.level
  | Now -> Value.timestamp env.context.time
  | Self None ->
      let instance = get_current_instance env "Self" in
      let id = get_current_contract_id env "Self" in
      let tparameter =
        (get_extra instance.template.tcontract.derived).tparameter
      in
      Value.contract (get_contract_address env.scenario_state id) tparameter
  | Self (Some entry_point) ->
      let msg = "interpreter: self_entry_point" in
      let id = get_current_contract_id env "Self_entry_point" in
      let instance = get_current_instance env "Self_entry_point" in
      let tparameter =
        (get_extra instance.template.tcontract.derived).tparameter
      in
      let row =
        match tparameter with
        | F (TVariant {row}) -> row
        | _ -> failwith "self_entry_point: tparameter not a variant"
      in
      let t = Option.of_some ~msg (List.assoc_opt entry_point row) in
      Value.contract ~entry_point (get_contract_address env.scenario_state id) t
  | Self_address ->
      let id = get_current_contract_id env "Self_address" in
      Value.literal
        (Literal.address (get_contract_address env.scenario_state id))
  | Chain_id -> Value.chain_id env.context.chain_id
  | Total_voting_power ->
      Value.nat
        (List.fold_left
           (fun acc (_, v) -> Big_int.add_big_int acc v)
           Big_int.zero_big_int
           env.context.voting_powers)
  | Sapling_empty_state {memo} ->
      Value.literal (Literal.sapling_test_state memo [])
  | Unit_ -> Value.literal Literal.unit
  | None_ t -> Value.none (Type.of_mtype t)
  | Nil t -> Value.build_list [] (Type.of_mtype t)
  | Empty_set telement ->
      let telement = Type.of_mtype telement in
      Value.set ~telement []
  | Empty_map (tkey, tvalue) ->
      let tkey = Type.of_mtype tkey in
      let tvalue = Type.of_mtype tvalue in
      Value.map ~big:false ~tkey ~tvalue []
  | Empty_bigmap (tkey, tvalue) ->
      let tkey = Type.of_mtype tkey in
      let tvalue = Type.of_mtype tvalue in
      Value.map ~big:true ~tkey ~tvalue []

let interpret_mprim1 ~config env e0 p =
  let module Printer = (val Printer.get config : Printer.Printer) in
  let primitives = env.primitives in
  let module P = (val primitives : Primitives) in
  let pp () = [] in
  let hash_algo h x =
    match x.v with
    | Literal (Bytes b) -> Value.bytes (h b)
    | _ -> assert false
  in
  match (p : _ Michelson_base.Primitive.prim1) with
  | Abs -> fun x -> Value.nat (Big_int.abs_big_int (Value.unInt ~pp x))
  | IsNat ->
      fun x ->
        let x = Value.unInt ~pp x in
        if 0 <= Big_int.compare_big_int x Big_int.zero_big_int
        then Value.some (Value.nat x)
        else Value.none (Type.nat ())
  | Not -> fun x -> Value.bool (not (Value.unBool ~pp x))
  | Hash_key ->
      fun x ->
        let public_key =
          x.v
          |> function
          | Literal (Key k) -> k
          | _ ->
              Format.kasprintf
                failwith
                "Type-error: hash_key expects a key, not: %s"
                (Printer.texpr_to_string e0)
        in
        Value.key_hash (P.Crypto.hash_key public_key)
  | Blake2b -> hash_algo P.Crypto.blake2b
  | Sha256 -> hash_algo P.Crypto.sha256
  | Sha512 -> hash_algo P.Crypto.sha512
  | Keccak -> hash_algo P.Crypto.keccak
  | Sha3 -> hash_algo P.Crypto.sha3
  | _ -> assert false

let interpret_mprim2 p =
  match (p : _ Michelson_base.Primitive.prim2) with
  | Lsl -> Value.shift_left
  | Lsr -> Value.shift_right
  | _ -> assert false

let interpret_mprim3 ~config env e0 p x1 x2 x3 =
  let module Printer = (val Printer.get config : Printer.Printer) in
  let primitives = env.primitives in
  let module P = (val primitives : Primitives) in
  match (p : Michelson_base.Primitive.prim3) with
  | Check_signature ->
      let pk_expr, sig_expr, msg_expr = (x1, x2, x3) in
      Dbg.on := false;
      let public_key =
        pk_expr.v
        |> function
        | Literal (Key k) -> k
        | _ ->
            Format.kasprintf
              failwith
              "Type-error: check_signature expects a key, not: %s"
              (Printer.texpr_to_string e0)
      in
      let signature =
        sig_expr.v
        |> function
        | Literal (Signature s) -> s
        | _ ->
            Format.kasprintf
              failwith
              "Type-error: check_signature expects a signature, not: %s"
              (Printer.texpr_to_string e0)
      in
      let msg =
        msg_expr.v
        |> function
        | Literal (Bytes b) -> b
        | _ ->
            Format.kasprintf
              failwith
              "Type-error: check_signature expects bytes (3rd argument), not: \
               %s"
              (Printer.texpr_to_string e0)
      in
      Value.bool (P.Crypto.check_signature ~public_key ~signature msg)
  | Open_chest ->
      let chest_key, chest, time = (x1, x2, x3) in
      let chest_key =
        chest_key.v
        |> function
        | Literal (Chest_key b) -> b
        | _ -> assert false
      in
      let chest =
        chest.v
        |> function
        | Literal (Chest b) -> b
        | _ -> assert false
      in
      let time =
        time.v
        |> function
        | Literal (Int i) -> Bigint.int_of_big_int i.i
        | _ -> assert false
      in
      let result =
        P.Timelock.open_chest
          (Misc.Hex.hexcape chest)
          (Misc.Hex.hexcape chest_key)
          time
      in
      let result_type =
        Type.variant_default_layout
          Config.Comb
          [("Left", Type.bytes); ("Right", Type.bool)]
      in
      ( match result with
      | Correct bytes ->
          Value.variant_with_type
            ~line_no:[]
            "Left"
            (Value.bytes (Misc.Hex.unhex ("0x" ^ bytes)))
            result_type
      | Bogus_cipher ->
          Value.variant_with_type
            ~line_no:[]
            "Right"
            (Value.bool false)
            result_type
      | Bogus_opening ->
          Value.variant_with_type
            ~line_no:[]
            "Right"
            (Value.bool true)
            result_type )
  | _ -> assert false

let interpret_prim0 ~config env e =
  let module Printer = (val Printer.get config : Printer.Printer) in
  let primitives = env.primitives in
  let module P = (val primitives : Primitives) in
  function
  | ECst x -> Value.literal x
  | EBounded x ->
      let cases =
        match Type.getRepr e.et with
        | TBounded {cases} ->
          ( match Unknown.get cases with
          | None -> assert false
          | Some x -> x )
        | _ -> assert false
      in
      Value.bounded cases x
  | ECstContract {address; entry_point; type_} ->
      Value.contract ?entry_point address type_
  | EConstantVar x ->
    begin
      match Hashtbl.find_opt env.scenario_state.variables x with
      | None -> assert false
      | Some value ->
          get_constant_value
            env
            (Value.unString ~pp:(fun () -> assert false) value)
    end
  | EConstant (hash, _) -> get_constant_value env hash
  | EMatchCons name ->
    ( match List.assoc_opt name !(env.variables) with
    | Some (Constant v) -> v
    | _ ->
        failwith
          (Printf.sprintf
             "Missing var %s in env [%s]"
             name
             (String.concat "; " (List.map (fun (x, _) -> x) !(env.variables))))
    )
  | EMetaLocal name ->
    ( match List.assoc_opt name !(env.variables) with
    | Some (Heap_ref {contents = x}) -> x
    | Some _ -> failwith ("Strange meta local: " ^ name)
    | None -> failwith ("Not a meta local: " ^ name) )
  | EContract_address (id, entry_point) ->
      Value.address ?entry_point (get_contract_address env.scenario_state id)
  | EContract_balance id ->
    ( match Hashtbl.find_opt env.scenario_state.contracts id with
    | Some t -> Value.mutez t.state.balance
    | None ->
        raise
          (SmartExcept
             [ `Text "Missing contract balance for id: "
             ; `Text (Printer.string_of_contract_id id)
             ; `Br
             ; `Line e.line_no ]) )
  | EContract_baker id ->
    ( match Hashtbl.find_opt env.scenario_state.contracts id with
    | Some {state = {baker = Some baker}} ->
        Value.some (Value.baker_value_of_protocol config.protocol baker)
    | Some {state = {baker = None}} ->
        Value.none (Type.baker_type_of_protocol config.protocol)
    | None ->
        raise
          (SmartExcept
             [ `Text "Missing contract for id: "
             ; `Text (Printer.string_of_contract_id id)
             ; `Br
             ; `Line e.line_no ]) )
  | EContract_data id ->
    ( match Hashtbl.find_opt env.scenario_state.contracts id with
    | Some t ->
      ( match t.state.storage with
      | Some storage -> storage
      | None ->
          raise
            (SmartExcept
               [ `Text "Missing contract storage for id: "
               ; `Text (Printer.string_of_contract_id id)
               ; `Br
               ; `Line e.line_no ]) )
    | None ->
        raise
          (SmartExcept
             [ `Text "Missing contract for id: "
             ; `Text (Printer.string_of_contract_id id)
             ; `Br
             ; `Line e.line_no ]) )
  | EContract_typed (id, entry_point) ->
    ( match Hashtbl.find_opt env.scenario_state.contracts id with
    | Some t ->
        let t_ep =
          match entry_point with
          | None -> (get_extra t.template.tcontract.derived).tparameter
          | Some entry_point ->
              let t_ep =
                match
                  Type.getRepr
                    (get_extra t.template.tcontract.derived).tparameter
                with
                | TVariant {row} ->
                    Option.of_some ~msg:"" (List.assoc_opt entry_point row)
                | _ -> assert false
              in
              t_ep
        in
        Value.contract
          ?entry_point
          (get_contract_address env.scenario_state id)
          t_ep
    | None ->
        raise
          (SmartExcept
             [ `Text "Missing contract for id: "
             ; `Text (Printer.string_of_contract_id id)
             ; `Br
             ; `Line e.line_no ]) )
  | EAccount_of_seed {seed} ->
      let account = P.Crypto.account_of_seed seed in
      let seed = Value.string seed in
      let pkh = Value.key_hash account.pkh in
      let address = Value.address account.pkh in
      let pk = Value.key account.pk in
      let sk = Value.secret_key account.sk in
      Value.record_comb
        [ ("seed", seed)
        ; ("address", address)
        ; ("public_key", pk)
        ; ("public_key_hash", pkh)
        ; ("secret_key", sk) ]

let interpret_prim1 ~config env e p =
  let scenario_vars = String.Map.of_hashtbl env.scenario_state.variables in
  let module Printer = (val Printer.get config : Printer.Printer) in
  let primitives = env.primitives in
  let module P = (val primitives : Primitives) in
  let pp () = [] in
  match p with
  | EToInt ->
      fun (x, _tx) ->
        ( match x.v with
        | Literal (Bls12_381_fr hex) ->
            Value.int
              (Big_int.big_int_of_string
                 ( try P.Bls12.convertFrToInt (Misc.Hex.hexcape hex) with
                 | e ->
                     Printf.ksprintf
                       failwith
                       "convertFrToInt: %s"
                       (Base.Exn.to_string e) ))
        | Literal (Int {i}) -> Value.int i
        | _ -> raise (SmartExcept [`Text "Cannot cast to Int"; `Expr e]) )
  | ENeg ->
      fun (x, _tx) ->
        begin
          match x.v with
          | Literal (Bls12_381_g1 hex) ->
              Value.bls12_381_g1
                (Misc.Hex.unhex
                   ( try P.Bls12.negateG1 (Misc.Hex.hexcape hex) with
                   | e ->
                       Printf.ksprintf
                         failwith
                         "negateG1: %s"
                         (Base.Exn.to_string e) ))
          | Literal (Bls12_381_g2 hex) ->
              Value.bls12_381_g2
                (Misc.Hex.unhex
                   ( try P.Bls12.negateG2 (Misc.Hex.hexcape hex) with
                   | e ->
                       Printf.ksprintf
                         failwith
                         "negateG2: %s"
                         (Base.Exn.to_string e) ))
          | Literal (Bls12_381_fr hex) ->
              Value.bls12_381_fr
                (Misc.Hex.unhex
                   ( try P.Bls12.negateFr (Misc.Hex.hexcape hex) with
                   | e ->
                       Printf.ksprintf
                         failwith
                         "negateFr: %s"
                         (Base.Exn.to_string e) ))
          | Literal (Int {i}) -> Value.int (Big_int.minus_big_int i)
          | _ -> assert false
        end
  | ESign ->
      fun (x, _tx) ->
        let x = Value.unInt ~pp x in
        let result = Big_int.compare_big_int x Big_int.zero_big_int in
        Value.int (Bigint.of_int result)
  | ESum ->
      fun (x, _tx) ->
        ( match x.v with
        | List (_, l) | Set (_, l) ->
            List.fold_left
              (fun x y -> Value.plus_inner ~primitives x y)
              (Value.zero_of_type e.et)
              l
        | Map (_, _, _, l) ->
            List.fold_left
              (fun x (_, y) -> Value.plus_inner ~primitives x y)
              (Value.zero_of_type e.et)
              l
        | _ -> failwith ("bad sum " ^ Printer.texpr_to_string e) )
  | EListRev ->
      fun (l, tl) ->
        ( match (l.v, Type.getRepr tl) with
        | Basics.List (_, l), T1 (T_list, t) -> Value.build_list (List.rev l) t
        | _ -> assert false )
  | EListItems rev ->
      fun (l, tl) ->
        ( match (l.v, Type.getRepr tl) with
        | Basics.Map (_, _, _, l), T2 ((T_map | T_big_map), tkey, tvalue) ->
            Value.build_list
              ((if rev then List.rev_map else List.map)
                 (fun (k, v) -> Value.record_comb [("key", k); ("value", v)])
                 l)
              (Type.key_value tkey tvalue)
        | _ -> assert false )
  | EListKeys rev ->
      fun (l, tl) ->
        ( match (l.v, Type.getRepr tl) with
        | Basics.Map (_, _, _, l), T2 ((T_map | T_big_map), tkey, _) ->
            Value.build_list
              ((if rev then List.rev_map else List.map) fst l)
              tkey
        | _ -> assert false )
  | EListValues rev ->
      fun (l, tl) ->
        ( match (l.v, Type.getRepr tl) with
        | Basics.Map (_, _, _, l), T2 ((T_map | T_big_map), _, tvalue) ->
            Value.build_list
              ((if rev then List.rev_map else List.map) snd l)
              tvalue
        | _ -> assert false )
  | EListElements rev ->
      fun (l, tl) ->
        ( match (l.v, Type.getRepr tl) with
        | Basics.Set (_, l), T1 (T_set, telement) ->
            Value.build_list (if rev then List.rev l else l) telement
        | _ -> assert false )
  | EPack ->
      fun (r, _tx) ->
        begin
          try
            r |> Compiler.pack_value ~config ~scenario_vars |> Value.bytes
          with
          | Data_encoding.Binary.Write_error write_error ->
              raise
                (ExecFailure
                   ( Value.string "Pack error"
                   , [ `Text "Could not pack bytes due to invalid content"
                     ; `Br
                     ; `Expr e
                     ; `Br
                     ; `Value r
                     ; `Text
                         (Format.asprintf
                            "%a"
                            Data_encoding.Binary.pp_write_error
                            write_error)
                     ; `Line e.line_no ] ))
        end
  | EUnpack t ->
      fun (r, _tx) ->
        ( match r.v with
        | Literal (Bytes b) ->
          begin
            try Compiler.unpack_value ~config t b |> Value.some with
            | Data_encoding.Binary.Read_error read_error ->
                raise
                  (ExecFailure
                     ( Value.string "Unpack error"
                     , [ `Text
                           (Format.asprintf
                              "Could not unpack bytes (%s) due to invalid \
                               content: %a."
                              (Misc.Hex.hexcape b)
                              Data_encoding.Binary.pp_read_error
                              read_error)
                       ; `Br
                       ; `Expr e
                       ; `Br
                       ; `Value r
                       ; `Line e.line_no ] ))
          end
        | _ ->
            failwith
              ( "Interpreter. sp.unpack expects bytes as parameter: "
              ^ Printer.texpr_to_string e ) )
  | EConcat_list ->
      fun (x, _tx) ->
        let vals =
          match x.v with
          | List (_, sl) ->
              Base.List.map sl ~f:(fun sb ->
                  match sb.v with
                  | Literal (String s) | Literal (Bytes s) -> s
                  | _ ->
                      Format.kasprintf
                        failwith
                        "Error while concatenating string/bytes:@ not a list \
                         of strings/bytes:@ %s"
                        (Printer.texpr_to_string e))
          | _ ->
              Format.kasprintf
                failwith
                "Error while concatenating string/bytes:@ not a list:@ %s"
                (Printer.texpr_to_string e)
        in
        ( match Type.getRepr e.et with
        | T0 T_string -> Value.string (String.concat "" vals)
        | T0 T_bytes -> Value.bytes (String.concat "" vals)
        | _ ->
            raise
              (SmartExcept
                 [ `Text "Bad type in concat (should apply to lists of "
                 ; `Type Type.string
                 ; `Text "or"
                 ; `Type Type.bytes
                 ; `Text ")"
                 ; `Expr e
                 ; `Br
                 ; `Line e.line_no ]) )
  | ESize ->
      fun (x, _tx) ->
        let result length = Value.nat (Bigint.of_int length) in
        ( match x.v with
        | Literal (String s) | Literal (Bytes s) -> result (String.length s)
        | List (_, l) | Set (_, l) -> result (List.length l)
        | Map (_, _, _, l) -> result (List.length l)
        | _ -> assert false )
  | EResolve -> fun (x, _tx) -> x
  | EProject i ->
      fun (x, _tx) ->
        let vs = Value.untuple ~pp x in
        if i < List.length vs
        then List.nth vs i
        else failwith "interpreter: short tuple"
  | EAddress ->
      fun (x, _tx) ->
        ( match x.v with
        | Contract {address; entry_point} ->
            Value.literal (Literal.address ?entry_point address)
        | _ -> assert false )
  | EImplicit_account ->
      fun (x, _tx) ->
        ( match x.v with
        | Literal (Key_hash key_hash) -> Value.contract key_hash Type.unit
        | _ ->
            failwith
              ( "Interpreter. sp.implicit_account is not available in this \
                 context: "
              ^ Printer.texpr_to_string e ) )
  | ESetDelegate ->
      fun (x, _tx) ->
        let baker = Value.un_baker x in
        (* The delegate must be registered *)
        ( match baker with
        | None -> ()
        | Some key_hash ->
            let registered =
              List.exists (fun (k, _) -> k = key_hash) env.context.voting_powers
            in
            if not registered
            then failwith (Printf.sprintf "Unregistered delegate (%s)" key_hash)
        );
        Value.operation (SetDelegate baker)
  | EType_annotation _ -> fun (x, _tx) -> x
  | EAttr name ->
      fun (x, _tx) ->
        ( match x.v with
        | Record (_, l) ->
          ( match List.assoc_opt name l with
          | Some v -> v
          | None ->
              failwith
                (Printf.sprintf
                   "Missing field in record [%s] [%s] [%s]."
                   name
                   (Printer.texpr_to_string e)
                   (String.concat ", " (List.map fst l))) )
        | _ -> failwith ("Not a record " ^ Printer.texpr_to_string e) )
  | EIsVariant name ->
      fun (x, _tx) ->
        ( match x.v with
        | Variant (_layout, _row, cons, _) -> Value.bool (cons = name)
        | _ ->
            failwith
              (Printf.sprintf
                 "Not a variant %s: %s"
                 (Printer.texpr_to_string e)
                 (Printer.value_to_string x)) )
  | EVariant name ->
      fun (x, _tx) ->
        Value.variant_with_type ~no_check:() ~line_no:[] name x e.et
  | EReadTicket ->
      fun (t, _tx) ->
        ( match t.v with
        | Ticket (address, content, amount) ->
            let address = Value.literal (Literal.address address) in
            let amount = Value.nat amount in
            Value.tuple [Value.tuple [address; content; amount]; t]
        | _ -> assert false )
  | EJoinTickets ->
      fun (x, _tx) ->
        ( match x.v with
        | Tuple
            [ {v = Ticket (ticketer1, content1, amount1)}
            ; {v = Ticket (ticketer2, content2, amount2)} ] ->
            if ticketer1 = ticketer2 && Value.equal content1 content2
            then
              Value.some
                (Value.ticket
                   ticketer1
                   content1
                   (Bigint.add_big_int amount1 amount2))
            else assert false
        | _ -> assert false )
  | EPairingCheck ->
      fun (x, _tx) ->
        let vals =
          match x.v with
          | List (_, sl) ->
              Base.List.map sl ~f:(fun sb ->
                  match sb.v with
                  | Tuple [i1; i2] ->
                      ( Misc.Hex.hexcape (Value.unBls12_381 i1)
                      , Misc.Hex.hexcape (Value.unBls12_381 i2) )
                  | _ ->
                      Format.kasprintf
                        failwith
                        "Expected a list of pairs (g1, g2) @ %s"
                        (Printer.texpr_to_string e))
          | _ ->
              Format.kasprintf
                failwith
                "Expected a list @ %s"
                (Printer.texpr_to_string e)
        in
        Value.bool
          ( try P.Bls12.pairingCheck vals with
          | e ->
              Printf.ksprintf failwith "pairingCheck: %s" (Base.Exn.to_string e)
          )
  | EVotingPower ->
      fun (x, _tx) ->
        let key_hash =
          match x.v with
          | Literal (Key_hash key_hash) -> key_hash
          | _ ->
              failwith
                ( "Interpreter. sp.voting_power is not available in this \
                   context: "
                ^ Printer.texpr_to_string e )
        in
        let res =
          List.find_opt
            (fun (k, _) ->
              Value.equal (Value.key_hash k) (Value.key_hash key_hash))
            env.context.voting_powers
        in
        Value.nat
          ( match res with
          | Some (_, v) -> v
          | None -> Big_int.zero_big_int )
  | EUnbounded ->
      fun (x, _tx) ->
        ( match x.v with
        | Bounded (_, l) -> Value.literal l
        | _ -> assert false )
  | EConvert ->
      fun (x, t_from) ->
        let open Michelson_interpreter in
        let t_to = e.et in
        let x = mvalue_of_value t_from x in
        value_of_mvalue config t_to x
  | EStaticView _ -> assert false

let interpret_prim2 ~config _upper_steps env e p =
  let module Printer = (val Printer.get config : Printer.Printer) in
  let primitives = env.primitives in
  let module P = (val primitives : Primitives) in
  match p with
  | EGetOpt ->
      fun (k, _tk) (m, tm) ->
        let tm = Type.unF tm in
        ( match (tm, m.v) with
        | T2 ((T_map | T_big_map), _, tvalue), Map (_, _, _, m) ->
          ( match List.assoc_opt ~equal:Value.equal k m with
          | Some v -> Value.some v
          | None -> Value.none tvalue )
        | _ -> assert false )
  | EBinOp BEq -> fun (x, _tx) (y, _ty) -> Value.bool (Value.equal x y)
  | EBinOp BNeq -> fun (x, _tx) (y, _ty) -> Value.bool (not (Value.equal x y))
  | EBinOp BAdd -> fun (x, _tx) (y, _ty) -> Value.plus ~primitives x y
  | EBinOp (BMul _) -> fun (x, _tx) (y, _ty) -> Value.mul ~primitives x y
  | EBinOp BMod -> fun (x, _tx) (y, _ty) -> Value.e_mod x y
  | EBinOp BDiv -> fun (x, _tx) (y, _ty) -> Value.div x y
  | EBinOp BSub -> fun (x, _tx) (y, _ty) -> Value.minus x y
  | EBinOp BLt -> fun (x, _tx) (y, _ty) -> Value.bool (Value.lt x y)
  | EBinOp BLe -> fun (x, _tx) (y, _ty) -> Value.bool (Value.le x y)
  | EBinOp BGt -> fun (x, _tx) (y, _ty) -> Value.bool (Value.lt y x)
  | EBinOp BGe -> fun (x, _tx) (y, _ty) -> Value.bool (Value.le y x)
  | EBinOp (BAnd | BOr) -> assert false
  | EBinOp BXor -> fun (x, _tx) (y, _ty) -> Value.xor x y
  | EBinOp BEDiv -> fun (x, _tx) (y, _ty) -> Value.ediv x y
  | ECons -> fun (x, _tx) (y, _ty) -> Value.cons x y
  | EBinOp BMax ->
      fun (x, tx) (y, _ty) ->
        let pp () = [] in
        let isNat =
          match Type.getRepr tx with
          | TInt {isNat} -> isNat
          | _ -> assert false
        in
        Value.intOrNat
          isNat
          (Big_int.max_big_int (Value.unInt ~pp x) (Value.unInt ~pp y))
  | EBinOp BMin ->
      fun (x, tx) (y, _ty) ->
        let pp () = [] in
        let isNat =
          match Type.getRepr tx with
          | TInt {isNat} -> isNat
          | _ -> assert false
        in
        Value.intOrNat
          isNat
          (Big_int.min_big_int (Value.unInt ~pp x) (Value.unInt ~pp y))
  | EAdd_seconds ->
      fun (t, _tt) (s, _ts) ->
        ( match (t.v, s.v) with
        | Literal (Timestamp t), Literal (Int {i = s}) ->
            Value.timestamp (Big_int.add_big_int t s)
        | _ -> failwith ("Cannot add timestamp " ^ Printer.texpr_to_string e) )
  | EContains ->
      fun (member, _) (items, _) ->
        ( match items.v with
        | Map (_, _, _, l) ->
            Value.bool (List.exists (fun (x, _) -> Value.equal x member) l)
        | Set (_, l) ->
            Value.bool (List.exists (fun x -> Value.equal x member) l)
        | _ -> failwith ("Cannot compute " ^ Printer.texpr_to_string e) )
  | EApplyLambda -> fun (x, _tx) (f, _tf) -> Value.closure_apply f x
  | ETicket ->
      fun (content, _tcontent) (y, _ty) ->
        let ticketer =
          let id = get_current_contract_id env "ETicket" in
          get_contract_address env.scenario_state id
        in
        ( match y.v with
        | Literal (Int {i}) -> Value.ticket ticketer content i
        | _ -> assert false )
  | ESplitTicket ->
      fun (ticket, _tticket) (y, _ty) ->
        ( match (ticket.v, y.v) with
        | ( Ticket (ticketer, content, amount)
          , Tuple [{v = Literal (Int {i = a1})}; {v = Literal (Int {i = a2})}] )
          ->
            if Bigint.equal (Bigint.add_big_int a1 a2) amount
            then
              let t1 = Value.ticket ticketer content a1 in
              let t2 = Value.ticket ticketer content a2 in
              Value.some (Value.tuple [t1; t2])
            else
              Value.none
                (Type.pair (type_of_value ticket) (type_of_value ticket))
        | _ -> assert false )
  | ECallLambda -> assert false
  | EView _ -> assert false

let interpret_prim3 ~config env e p =
  let module Printer = (val Printer.get config : Printer.Printer) in
  let primitives = env.primitives in
  let module P = (val primitives : Primitives) in
  match p with
  | ESplit_tokens ->
      fun (mutez, _tmutez) quantity (total, _ttotal) ->
        let pp () = [] in
        Value.mutez
          (Big_int.div_big_int
             (Big_int.mult_big_int
                (Value.unMutez ~pp mutez)
                (Value.unInt ~pp quantity))
             (Value.unInt ~pp total))
  | ERange ->
      fun (a, ta) b (step, _tstep) ->
        ( match (a.v, b.v, step.v) with
        | Literal (Int {i = a}), Literal (Int {i = b}), Literal (Int {i = step})
          ->
            let a = Big_int.int_of_big_int a in
            let b = Big_int.int_of_big_int b in
            let step = Big_int.int_of_big_int step in
            if step = 0
            then failwith (Printf.sprintf "Range with 0 step")
            else if step * (b - a) < 0
            then Value.build_list [] ta
            else
              let isNat =
                match Type.getRepr ta with
                | TInt {isNat} -> isNat
                | _ -> assert false
              in
              let rec aux a acc =
                if (b - a) * step <= 0
                then List.rev acc
                else
                  aux (a + step) (Value.intOrNat isNat (Bigint.of_int a) :: acc)
              in
              Value.build_list (aux a []) ta
        | _ -> failwith ("bad range" ^ Printer.texpr_to_string e) )
  | EUpdate_map ->
      fun (key, _tkey) value (map, tz) ->
        ( match (Type.unF (Type.normalize tz), map.v) with
        | T2 (((T_map | T_big_map) as tm), tkey, tvalue), Map (_, _, _, xs) ->
            let value = Value.unOption value in
            let m' = Lens.set (Lens.assoc ~equal:Value.equal ~key) value xs in
            Value.map ~big:(tm = T_big_map) ~tkey ~tvalue m'
        | _ -> assert false )
  | EGet_and_update ->
      fun (key, _tkey) value (map, tz) ->
        ( match (Type.unF (Type.normalize tz), map.v) with
        | T2 (((T_map | T_big_map) as tm), tkey, tvalue), Map (_, _, _, xs) ->
            let value' = Value.unOption value in
            let old, m' =
              Lens.get_and_set (Lens.assoc ~equal:Value.equal ~key) value' xs
            in
            let map = Value.map ~big:(tm = T_big_map) ~tkey ~tvalue m' in
            Value.tuple [Value.option (type_of_value value) old; map]
        | _ -> assert false )
  | ETest_ticket ->
      fun (ticketer, _tticketer) content (amount, _tamount) ->
        ( match amount.v with
        | Literal (Int {i}) ->
            Value.ticket
              (Value.unAddress
                 ~pp:(fun () ->
                   [`Text "Bad ticketer in test_ticket"; `Line e.line_no])
                 ticketer)
              content
              i
        | _ -> assert false )

let rec interpret_expr ~config upper_steps env : texpr -> value =
  let module Printer = (val Printer.get config : Printer.Printer) in
  let rec interp e =
    let primitives = env.primitives in
    let module P = (val primitives : Primitives) in
    match e.e with
    | EVar ("__parameter__", _) -> get_current_parameters env "__parameter__"
    | EVar ("__storage__", _) ->
        let instance = get_current_instance env "__storage__" in
        Option.get ~msg:"no storage" instance.state.storage
    | EVar (id, Scenario) ->
      ( match Hashtbl.find_opt env.scenario_state.variables id with
      | Some x -> x
      | None ->
          raise
            (SmartExcept
               [ `Text "Missing scenario variable for id: "
               ; `Text id
               ; `Br
               ; `Line e.line_no ]) )
    | EVar (name, _) ->
      ( match List.assoc_opt name !(env.variables) with
      | Some (Heap_ref {contents = x}) -> x
      | Some (Iter (v, _)) -> v
      | Some (Constant v) -> v
      | None -> failwith name )
    | EPrivate id ->
      ( match List.assoc_opt id env.private_variables with
      | Some e -> interp e
      | None -> Printf.ksprintf failwith "Missing private variable %s" id )
    | EMPrim0 p -> interpret_mprim0 env e p
    | EMPrim1 (p, x) ->
        let x = interp x in
        interpret_mprim1 ~config env e p x
    | EMPrim1_fail _ -> assert false
    | EMPrim2 (p, x1, x2) ->
        let x2 = interp x2 in
        let x1 = interp x1 in
        interpret_mprim2 p x1 x2
    | EMPrim3 (p, x1, x2, x3) ->
        let x3 = interp x3 in
        let x2 = interp x2 in
        let x1 = interp x1 in
        interpret_mprim3 ~config env e p x1 x2 x3
    | EPrim0 p -> interpret_prim0 ~config env e p
    | EIsFailing x ->
      ( try
          let (_ : value) = interp x in
          Value.bool false
        with
      | _ -> Value.bool true )
    | ECatchException (t, x) ->
      ( try
          let (_ : value) = interp x in
          Value.none t
        with
      | Failure f -> Value.some (Value.string f)
      | ExecFailure (value, _) -> Value.some value
      | SmartExcept _ -> Value.some (Value.string "Error")
      | _ -> assert false )
    | EPrim1 (EStaticView (static_id, name), param) ->
        let param = interp param in
        let id = C_static static_id in
        let return_type =
          match Type.getRepr e.et with
          | T1 (T_option, t) -> t
          | _ -> assert false
        in
        eval_view
          ~name
          ~id
          ~config
          ~line_no:e.line_no
          ~e
          ~return_type
          ~types_ok:true
          env
          param
    | EPrim1 (p, x) ->
        let tx = x.et in
        let x = interp x in
        interpret_prim1 ~config env e p (x, tx)
    | EPrim2 (EBinOp BOr, x, y) ->
        let pp () = [] in
        ( match Type.getRepr x.et with
        | TInt _ ->
            Value.nat
              (Big_int.or_big_int
                 (Value.unInt ~pp (interp x))
                 (Value.unInt ~pp (interp y)))
        | T0 T_bool ->
            Value.bool
              (Value.unBool ~pp (interp x) || Value.unBool ~pp (interp y))
        | _ ->
            (* This should never happen (ensured by checker.ml) *) assert false
        )
    | EPrim2 (EBinOp BAnd, x, y) ->
        let pp () = [] in
        ( match Type.getRepr x.et with
        | TInt _ ->
            Value.nat
              (Big_int.and_big_int
                 (Value.unInt ~pp (interp x))
                 (Value.unInt ~pp (interp y)))
        | T0 T_bool ->
            Value.bool
              (Value.unBool ~pp (interp x) && Value.unBool ~pp (interp y))
        | _ ->
            (* This should never happen (ensured by checker.ml) *) assert false
        )
    | EPrim2 (ECallLambda, parameter, lambda) ->
        let lambda = interp lambda in
        let parameter = interp parameter in
        call_lambda ~config upper_steps env lambda parameter
    | EPrim2 (EView (name, return_type), x, param) ->
        let param = interp param in
        let x = interp x in
        let address =
          match x with
          | {v = Literal (Address {address})} -> address
          | _ -> raise (SmartExcept [`Text "View address is invalid"])
        in
        ( match Hashtbl.find_opt env.scenario_state.rev_addresses address with
        | None ->
            if config.view_check_exception
            then
              raise
                (ExecFailure
                   ( Value.string "Missing contract for view"
                   , [`Expr e; `Line e.line_no] ))
            else Value.none return_type
        | Some id ->
            eval_view
              ~name
              ~id
              ~config
              ~line_no:e.line_no
              ~e
              ~return_type
              ~types_ok:false
              env
              param )
    | EPrim2 (p, x1, x2) ->
        let tx1 = x1.et in
        let tx2 = x2.et in
        let x2 = interp x2 in
        let x1 = interp x1 in
        interpret_prim2 ~config upper_steps env e p (x1, tx1) (x2, tx2)
    | EIf (c, t, e) ->
        let pp () = [] in
        if Value.unBool ~pp (interp c) then interp t else interp e
    | EPrim3 (p, x1, x2, x3) ->
        let tx1 = x1.et in
        let tx3 = x3.et in
        let x3 = interp x3 in
        let x2 = interp x2 in
        let x1 = interp x1 in
        interpret_prim3 ~config env e p (x1, tx1) x2 (x3, tx3)
    | EOpenVariant (name, (x as x0), missing_message) ->
        let x = interp x in
        ( match x.v with
        | Variant (_row, _layout, cons, v) ->
            if cons <> name
            then
              let default_message =
                Printf.sprintf
                  "Not the proper variant constructor [%s] != [%s]"
                  name
                  cons
              in
              let missing_message =
                Base.Option.map
                  ~f:(interpret_expr ~config upper_steps env)
                  missing_message
              in
              let missing_message, message =
                match missing_message with
                | Some except -> (except, [`Br; `Text "Message:"; `Value except])
                | None -> (Value.string default_message, [])
              in
              raise
                (ExecFailure
                   ( missing_message
                   , [ `Text default_message
                     ; `Br
                     ; `Expr e
                     ; `Rec message
                     ; `Line e.line_no ] ))
            else v
        | _ ->
            failwith
              (Printf.sprintf
                 "Not a variant %s (from %s)"
                 (Printer.value_to_string x)
                 (Printer.texpr_to_string x0)) )
    | EItem {items; key; default_value; missing_message} ->
        let pp () =
          Printf.sprintf
            "%s[%s]"
            (Printer.texpr_to_string items)
            (Printer.texpr_to_string key)
        in
        let items = interp items in
        let key = interp key in
        let def =
          match default_value with
          | None -> None
          | Some def -> Some (lazy (interp def))
        in
        let missing_message =
          match missing_message with
          | None -> None
          | Some x -> Some (lazy (interp x))
        in
        ( match items.v with
        | Map (_, _, _, map) ->
          ( match (List.assoc_opt ~equal:Value.equal key map, def) with
          | Some v, _ -> v
          | None, Some v -> Lazy.force v
          | _ ->
              let default_message = "Missing item in map" in
              let missing_message =
                match missing_message with
                | None -> Value.string default_message
                | Some missing_message -> Lazy.force missing_message
              in
              raise
                (ExecFailure
                   ( missing_message
                   , [ `Text (default_message ^ ":")
                     ; `Value key
                     ; `Text "is not in"
                     ; `Value items
                     ; `Text "while evaluating"
                     ; `Text (pp ()) ] )) )
        | _ ->
            raise
              (SmartExcept
                 [ `Text "Bad getItem"
                 ; `Value items
                 ; `Text "["
                 ; `Value key
                 ; `Text "]" ]) )
    | ERecord [] -> Value.unit
    | ERecord l ->
        let layout =
          match Type.getRepr e.et with
          | TRecord {layout} -> layout
          | _ ->
              raise
                (SmartExcept
                   [`Text "Bad type while evaluating"; `Expr e; `Line e.line_no])
        in
        let layout =
          Option.get ~msg:"interpreter: layout" (Unknown.get layout)
        in
        Value.record ~layout (List.map (fun (s, e) -> (s, interp e)) l)
    | EMapFunction {l; f} ->
        let f = interp f in
        ( match ((interp l).v, Type.getRepr (type_of_value f)) with
        | List (_, l), TLambda (effects, _, t) ->
            let pp () = [`Text "effects not allowed here"; `Line e.line_no] in
            assert_unknown pp effects.with_storage None;
            assert_unknown pp effects.with_operations false;
            l
            |> List.map (call_lambda ~config upper_steps env f)
            |> fun l -> Value.build_list l t
        | ( Map (_, _, _, l)
          , TLambda
              ( effects
              , F (TRecord {layout; row = [("key", tkey); ("value", _)]})
              , t ) ) ->
            let pp () = [`Text "effects not allowed here"; `Line e.line_no] in
            assert_unknown pp effects.with_storage None;
            assert_unknown pp effects.with_operations false;
            let layout =
              Option.get ~msg:"interpreter: layout" (Unknown.get layout)
            in
            l
            |> List.map (fun (k, v) ->
                   ( k
                   , call_lambda
                       ~config
                       upper_steps
                       env
                       f
                       (Value.record ~layout [("key", k); ("value", v)]) ))
            |> fun l -> Value.map ~big:false ~tkey ~tvalue:t l
        | _ ->
            Printf.ksprintf
              failwith
              "Not a list or a map %s"
              (Printer.type_to_string (type_of_value f)) )
    | ESlice {offset; length; buffer} ->
        let offset, length, buffer =
          (interp offset, interp length, interp buffer)
        in
        Basics.(
          ( match (offset.v, length.v, buffer.v) with
          | ( Literal (Int {i = ofs_bi})
            , Literal (Int {i = len_bi})
            , Literal (String s) ) ->
            ( try
                Value.some
                  (Value.string
                     (String.sub
                        s
                        (Big_int.int_of_big_int ofs_bi)
                        (Big_int.int_of_big_int len_bi)))
              with
            | _ -> Value.none Type.string )
          | ( Literal (Int {i = ofs_bi})
            , Literal (Int {i = len_bi})
            , Literal (Bytes s) ) ->
            ( try
                Value.some
                  (Value.bytes
                     (String.sub
                        s
                        (Big_int.int_of_big_int ofs_bi)
                        (Big_int.int_of_big_int len_bi)))
              with
            | _ -> Value.none Type.bytes )
          | _ ->
              Format.kasprintf
                failwith
                "Error while slicing string/bytes: %s"
                (Printer.texpr_to_string e) ))
    | EMap (_, entries) ->
      ( match Type.getRepr e.et with
      | T2 (((T_map | T_big_map) as tm), tkey, tvalue) ->
          Value.map
            ~big:(tm = T_big_map)
            ~tkey
            ~tvalue
            (List.fold_right
               (fun (key, value) entries ->
                 let value = interp value in
                 let key = interp key in
                 (key, value)
                 :: Base.List.Assoc.remove ~equal:Value.equal entries key)
               entries
               [])
      | _ ->
          Format.kasprintf
            failwith
            "%s is not a map: %s"
            (Printer.type_to_string e.et)
            (Printer.texpr_to_string e) )
    | EList items ->
      ( match Type.unF (Type.normalize e.et) with
      | T1 (T_list, t) ->
          Value.build_list (List.rev_map interp (List.rev items)) t
      | _ -> assert false )
    | ESet l ->
      begin
        match Type.getRepr e.et with
        | T1 (T_set, telement) ->
            Value.set ~telement (List.rev_map interp (List.rev l))
        | _ -> assert false
      end
    | EContract {arg_type; address; entry_point} ->
      ( match (entry_point, interp address) with
      | entry_point, {v = Literal (Address {address = addr; entry_point = None})}
       |None, {v = Literal (Address {address = addr; entry_point})} ->
          let ok () = Value.some (Value.contract ?entry_point addr arg_type) in
          let contract =
            match Hashtbl.find_opt env.scenario_state.rev_addresses addr with
            | None -> None
            | Some id ->
                Option.map
                  (fun c -> (id, c))
                  (Hashtbl.find_opt env.scenario_state.contracts id)
          in
          ( match contract with
          | None -> ok ()
          | Some (id, contract) ->
              let extra = get_extra contract.template.tcontract.derived in
              let raise_or_none exn =
                if extra.config.contract_check_exception
                then raise exn
                else Value.none (Type.contract arg_type)
              in
              let check_type t =
                if Type.equal arg_type t
                then ok ()
                else if extra.config.contract_check_exception
                then
                  raise_or_none
                    (ExecFailure
                       ( Value.string "Wrong type for contract"
                       , [ `Text "Wrong type for contract("
                         ; `Text (Printer.string_of_contract_id id)
                         ; `Text ")."
                         ; `Br
                         ; `Expr e
                         ; `Br
                         ; `Type t
                         ; `Br
                         ; `Text "instead of"
                         ; `Br
                         ; `Type arg_type
                         ; `Br
                         ; `Text "Please set a stable type."
                         ; `Br
                         ; `Line e.line_no ] ))
                else Value.none (Type.contract arg_type)
              in
              begin
                match entry_point with
                | None ->
                  begin
                    match Type.getRepr extra.tparameter with
                    | TVariant {row = [(_, t)]} -> check_type t
                    | TVariant {row = []} | T0 T_unit -> check_type Type.unit
                    | TVariant {row} ->
                      begin
                        match List.assoc_opt "default" row with
                        | None ->
                            raise_or_none
                              (ExecFailure
                                 ( Value.string
                                     "Missing entry point target in contract"
                                 , [ `Text
                                       "Missing entry point target in contract \
                                        ("
                                   ; `Text (Printer.string_of_contract_id id)
                                   ; `Text ")."
                                   ; `Br
                                   ; `Line e.line_no ] ))
                        | Some t -> check_type t
                      end
                    | _ -> assert false
                  end
                | Some entry_point ->
                  begin
                    match Type.getRepr extra.tparameter with
                    | TVariant {row = [(ep_name, t)]} ->
                        if ep_name = entry_point
                           && extra.config.single_entry_point_annotation
                        then check_type t
                        else
                          raise_or_none
                            (ExecFailure
                               ( Value.string "No annotation in contract"
                               , [ `Text "Entry point annotation ("
                                 ; `Text entry_point
                                 ; `Text ") for contract ("
                                 ; `Text (Printer.string_of_contract_id id)
                                 ; `Text
                                     ") with a single entry point and no \
                                      annotation."
                                 ; `Br
                                 ; `Line e.line_no ] ))
                    | TVariant {row} ->
                      begin
                        match List.assoc_opt entry_point row with
                        | None ->
                            raise_or_none
                              (ExecFailure
                                 ( Value.string
                                     "Missing entry point target in contract"
                                 , [ `Text "Missing entry point target ("
                                   ; `Text entry_point
                                   ; `Text ") for contract ("
                                   ; `Text (Printer.string_of_contract_id id)
                                   ; `Text ")."
                                   ; `Br
                                   ; `Line e.line_no ] ))
                        | Some t -> check_type t
                      end
                    | _ -> assert false
                  end
              end )
      | _, res ->
          raise (SmartExcept [`Text "wrong result for EContract:"; `Value res])
      )
    | ETuple es -> Value.tuple (List.rev_map interp (List.rev es))
    | ELambda l -> Value.closure_init l
    | EMake_signature {secret_key; message; message_format} ->
        let secret_key, message = (interp secret_key, interp message) in
        let secret_key =
          secret_key.v
          |> function
          | Literal (Secret_key k) -> k
          | _ -> assert false
        in
        let message =
          message.v
          |> function
          | Literal (Bytes k) ->
            ( match message_format with
            | `Raw -> k
            | `Hex ->
                Misc.Hex.unhex
                  Base.(
                    String.chop_prefix k ~prefix:"0x" |> Option.value ~default:k)
            )
          | _ ->
              Format.kasprintf
                failwith
                "Type-error: make_signature expects bytes, not: %s"
                (Printer.texpr_to_string e)
        in
        P.Crypto.sign ~secret_key message |> Value.signature
    | EMichelson ({parsed; typesIn; typesOut}, es) ->
        let open Michelson_interpreter in
        let code = Michelson.Of_micheline.instruction parsed in
        let ts =
          List.map (fun e -> Typing.mtype_of_type ~with_annots:true e.et) es
        in
        let code =
          Michelson.typecheck_instr
            ~strict_dup:false
            ~tparameter:(Michelson.mt_unit, None)
            (* TODO: rely on the Checker and treat tparameter *)
            (Stack_ok ts)
            code
        in
        let stack =
          List.map2 (fun t x -> mvalue_of_value t (interp x)) typesIn es
        in
        let module MI = M (P) in
        let context =
          let amount = env.context.amount in
          let balance = (get_current_instance env "balance").state.balance in
          ({config; balance; amount} : context)
        in
        let stack = MI.interpret context code stack in
        ( match (stack, typesOut) with
        | Ok [x], [t] ->
            let mt = Typing.mtype_of_type ~with_annots:false t in
            let x = typecheck_mvalue mt x in
            let x = value_of_tmvalue config x in
            let x = Value.smartMLify t x in
            let _ = Value.typecheck t x in
            x
        | Failed v, _ ->
            let v = value_of_tmvalue config v in
            raise
              (ExecFailure
                 ( v
                 , [ `Text "Failure:"
                   ; `Text (Printer.value_to_string v)
                   ; `Br
                   ; `Line e.line_no ] ))
        | Error msg, _ -> failwith msg
        | _ -> assert false )
    | ETransfer {destination; arg = params; amount} ->
        let params, amount, destination =
          (interp params, interp amount, interp destination)
        in
        let destination =
          match destination with
          | {v = Contract d} -> d
          | _ -> assert false
        in
        let amount =
          match amount with
          | {v = Literal (Mutez x)} -> x
          | _ -> assert false
        in
        Value.operation (Transfer {params; destination; amount})
    | ECreate_contract {contract_template; baker; balance; storage} ->
        let baker, balance, storage =
          (interp baker, interp balance, interp storage)
        in
        let baker = Value.un_baker baker in
        let balance =
          match balance with
          | {v = Literal (Mutez x)} -> x
          | _ -> assert false
        in
        let storage = storage in
        let {template; state} =
          interpret_contract
            ~config
            ~primitives
            ~scenario_state:env.scenario_state
            {tcontract = {contract_template with storage = None}}
        in
        let state = {state with baker; balance; storage = Some storage} in
        let dynamic_id = !(env.scenario_state.next_dynamic_address_id) in
        incr env.scenario_state.next_dynamic_address_id;
        let id = C_dynamic {dynamic_id} in
        let address = get_contract_address env.scenario_state id in
        Value.record_comb
          [ ( "operation"
            , Value.operation
                (CreateContract {id; instance = {template; state}}) )
          ; ("address", Value.literal (Literal.address address)) ]
    | EMatch _ -> failwith "Interpreter TODO: ematch"
    | ESaplingVerifyUpdate {state; transaction} ->
        let pp () = [] in
        let state = interp state in
        let transaction = interp transaction in
        let source, target, amount =
          Value.unSaplingTransaction ~pp transaction
        in
        let output =
          match (source, target) with
          | None, None -> assert false
          | None, Some _ -> Bigint.minus_big_int amount
          | Some _, None -> amount
          | Some _, Some _ -> Bigint.zero_big_int
        in
        let memo, state = Value.unSaplingState ~pp state in
        let state =
          match source with
          | None -> Some state
          | Some source ->
            ( match List.assoc_opt source state with
            | Some x when Bigint.ge_big_int x amount ->
                Some
                  ( (source, Bigint.sub_big_int x amount)
                  :: List.remove_assoc source state )
            | _ -> None )
        in
        let state =
          match (state, target) with
          | None, _ -> None
          | Some state, None -> Some state
          | Some state, Some target ->
            ( match List.assoc_opt target state with
            | Some x ->
                Some
                  ( (target, Bigint.add_big_int x amount)
                  :: List.remove_assoc target state )
            | _ -> Some ((target, amount) :: state) )
        in
        ( match state with
        | None ->
            Value.none
              (Type.pair (Type.int ()) (Type.sapling_state (Some memo)))
        | Some state ->
            Value.some
              (Value.tuple
                 [ Value.int output
                 ; Value.literal (Literal.sapling_test_state memo state) ]) )
    | EHasEntryPoint ep_name ->
        let instance = get_current_instance env "sp.has_entry_point" in
        let leps = instance.state.lazy_entry_points in
        let has =
          match List.assoc_opt ep_name leps with
          | None -> assert false
          | Some `Absent -> false
          | Some (`Closure _ | `Initial) -> true
        in
        Value.bool has
  in
  interp

and path_of_expr ~config upper_steps env =
  let rec of_expr acc e =
    match e.e with
    | EVar ("__storage__", _) -> Some {root = R_storage; steps = acc}
    | EPrim0 (EMetaLocal name) | EVar (name, Local) ->
        Some {root = R_local name; steps = acc}
    | EItem {items = cont; key; default_value = None; missing_message = None} ->
        let key = interpret_expr ~config upper_steps env key in
        ( match Type.getRepr cont.et with
        | T2 ((T_map | T_big_map), _, _) -> of_expr (S_item_map key :: acc) cont
        | _ ->
            raise
              (SmartExcept
                 [ `Text "Interpreter Error"
                 ; `Br
                 ; `Text "GetItem"
                 ; `Expr e
                 ; `Expr cont
                 ; `Text "is not a map"
                 ; `Line e.line_no ]) )
    | EPrim1 (EAttr name, expr) -> of_expr (S_attr name :: acc) expr
    | EVar (name, Simple) ->
      ( match List.assoc name !(env.variables) with
      | Iter (_, Some p) -> Some (extend_path p acc)
      | Iter (_, None) -> None (* Iteratee not an l-expression. *)
      | _ -> None )
    | _ -> None
  in
  of_expr []

and interpret_command
    ~config upper_steps env ({line_no} as initialCommand : tcommand) : value =
  let module Printer = (val Printer.get config : Printer.Printer) in
  let interpret_command = interpret_command ~config in
  let interpret_expr = interpret_expr ~config in
  let path_of_expr = path_of_expr ~config in
  let pp () = [] in
  match initialCommand.c with
  | CNever message ->
      ignore (interpret_expr upper_steps env message : Value.t);
      failwith "Evaluation of sp.never. It should not happen."
  | CFailwith message ->
      let value = interpret_expr upper_steps env message in
      raise
        (ExecFailure
           ( value
           , [`Text "Failure:"; `Text (Printer.value_to_string value)]
             @ ( match message.e with
               | EPrim0 (ECst _) -> []
               | _ -> [`Br; `Text "("; `Expr message; `Text ")"] )
             @ [`Br; `Line line_no] ))
  | CIf (c, t, e) ->
      let sub_steps = ref [] in
      let condition = interpret_expr upper_steps env c in
      let r =
        if Value.unBool ~pp condition
        then interpret_command sub_steps env t
        else interpret_command sub_steps env e
      in
      addStep
        ~sub_steps
        upper_steps
        env
        initialCommand
        [("condition", condition)];
      r
  | CMatchCons {expr; id; ok_match; ko_match} ->
    ( match Value.unList ~pp (interpret_expr upper_steps env expr) with
    | [] ->
        let sub_steps = ref [] in
        interpret_command sub_steps env ko_match
    | head :: tail ->
        let sub_steps = ref [] in
        let env =
          { env with
            variables =
              ref
                (add_variable
                   id
                   (Constant
                      (Value.record_comb
                         [ ("head", head)
                         ; ("tail", Value.build_list tail (type_of_value head))
                         ]))
                   !(env.variables)) }
        in
        interpret_command sub_steps env ok_match )
  | CMatchProduct (s, p, c) ->
      let sub_steps = ref [] in
      let variables = !(env.variables) in
      let vs =
        match p with
        | Pattern_single x ->
            let v = interpret_expr upper_steps env s in
            [(x, Constant v)]
        | Pattern_tuple ns ->
            let vs = Value.untuple ~pp (interpret_expr upper_steps env s) in
            List.map2 ~err:"match" (fun n v -> (n, Constant v)) ns vs
        | Pattern_record (_, bs) ->
            let r = Value.un_record (interpret_expr upper_steps env s) in
            let f {var; field} =
              (var, Constant (List.assoc_exn ~msg:"match" field r))
            in
            List.map f bs
      in
      let variables = List.fold_right (uncurry add_variable) vs variables in
      let env = {env with variables = ref variables} in
      interpret_command sub_steps env c
  | CModifyProduct (lhs, p, c) ->
    ( match path_of_expr upper_steps env lhs with
    | None ->
        failwith
          (Printf.sprintf
             "Line %s: Cannot assign to '%s'"
             (string_of_line_no initialCommand.line_no)
             (Printer.texpr_to_string lhs))
    | Some lhs_path ->
        let lhs_lens, _err = lens_of_path ~config ~line_no env lhs_path in
        let sub_steps = ref [] in
        let variables = !(env.variables) in
        let v = interpret_expr upper_steps env lhs in
        let vs =
          match p with
          | Pattern_single x -> [(x, Heap_ref (ref v))]
          | Pattern_tuple ns ->
              let vs = Value.untuple ~pp v in
              List.map2 ~err:"match" (fun n v -> (n, Heap_ref (ref v))) ns vs
          | Pattern_record (name, _bs) -> [(name, Heap_ref (ref v))]
          (* let r = Value.un_record v in
           * let f {var; field} =
           *   (var, Heap_ref (ref (List.assoc_exn ~msg:"match" field r)))
           * in
           * List.map f bs *)
        in
        let variables = List.fold_right (uncurry add_variable) vs variables in
        let env = {env with variables = ref variables} in
        let inner = interpret_command sub_steps env c in
        ( match p with
        | Pattern_single _ | Pattern_tuple _ ->
            let rhs = Some inner in
            Lens.set lhs_lens rhs ();
            Value.unit
        | Pattern_record (name, _bs) ->
            assert_unit ~config initialCommand inner;
            let value =
              match List.assoc_opt name !(env.variables) with
              | Some (Heap_ref {contents = x}) -> x
              | _ -> failwith ("Not a local: " ^ name)
            in
            Lens.set lhs_lens (Some value) ();
            Value.unit ) )
  | CMatch (scrutinee, cases) ->
      let sub_steps = ref [] in
      let scrutinee = interpret_expr upper_steps env scrutinee in
      ( match scrutinee.v with
      | Variant (_row, _layout, cons, arg) ->
          let res =
            match
              List.find_opt
                (fun (constructor, _, _) -> constructor = cons)
                cases
            with
            | None -> Value.unit
            | Some (_constructor, arg_name, body) ->
                interpret_command
                  sub_steps
                  { env with
                    variables =
                      ref
                        (add_variable arg_name (Constant arg) !(env.variables))
                  }
                  body
          in
          addStep
            ~sub_steps
            upper_steps
            env
            initialCommand
            [("match", scrutinee)];
          res
      | _ -> assert false )
  | CBind (x, c1, c2) ->
      let outer_locals = !(env.current_locals) in
      env.current_locals := [];
      let y = interpret_command upper_steps env c1 in
      ( match x with
      | None -> ()
      | Some x ->
          env.variables := add_variable x (Heap_ref (ref y)) !(env.variables) );
      let r = interpret_command upper_steps env c2 in
      env.variables :=
        List.filter
          (fun (n, _) ->
            (not (List.mem n !(env.current_locals)))
            && Option.cata true (( <> ) n) x)
          !(env.variables);
      env.current_locals := outer_locals;
      r
  | CSetVar (lhs, rhs) ->
      ( match path_of_expr upper_steps env lhs with
      | None ->
          failwith
            (Printf.sprintf
               "Line %s: Cannot assign to '%s'"
               (string_of_line_no initialCommand.line_no)
               (Printer.texpr_to_string lhs))
      | Some lhs ->
          let lhs, _err = lens_of_path ~config ~line_no env lhs in
          let rhs = Some (interpret_expr upper_steps env rhs) in
          Lens.set lhs rhs () );
      addStep upper_steps env initialCommand [];
      Value.unit
  | CDelItem (map, key) ->
      ( match path_of_expr upper_steps env map with
      | None ->
          failwith
            (Printf.sprintf
               "Line %s: Cannot assign to '%s'"
               (string_of_line_no line_no)
               (Printer.texpr_to_string map))
      | Some path ->
          let key = interpret_expr upper_steps env key in
          let l, _err =
            lens_of_path
              ~config
              ~line_no
              env
              (extend_path path [S_item_map key])
          in
          Lens.set l None () );
      addStep upper_steps env initialCommand [];
      Value.unit
  | CUpdateSet (set, elem, add) ->
      ( match path_of_expr upper_steps env set with
      | None ->
          failwith
            (Printf.sprintf
               "Line %s: Cannot remove from '%s'"
               (string_of_line_no line_no)
               (Printer.texpr_to_string set))
      | Some path ->
          let elem = interpret_expr upper_steps env elem in
          let l, err = lens_of_path ~config ~line_no env path in
          Lens.set (l @. Lens.some ~err @. Value.lens_set_at ~elem) add () );
      addStep upper_steps env initialCommand [];
      Value.unit
  | CDefineLocal {var; rhs} ->
      env.current_locals := var :: !(env.current_locals);
      env.variables :=
        add_variable
          var
          (Heap_ref (ref (interpret_expr upper_steps env rhs)))
          !(env.variables);
      addStep upper_steps env initialCommand [];
      Value.unit
  | CFor (name, iteratee, body) ->
      let sub_steps = ref [] in
      let iteratee_v = interpret_expr upper_steps env iteratee in
      let iteratee_l = path_of_expr upper_steps env iteratee in
      ( match iteratee_v.v with
      | List (_, elems) ->
          let step i v =
            let path =
              Base.Option.map iteratee_l ~f:(fun p ->
                  extend_path p [S_item_list i])
            in
            assert_unit
              ~config
              initialCommand
              (interpret_command
                 sub_steps
                 { env with
                   variables =
                     ref (add_variable name (Iter (v, path)) !(env.variables))
                 }
                 body)
          in
          List.iteri step elems
      | _ -> assert false );
      addStep ~sub_steps upper_steps env initialCommand [];
      Value.unit
  | CWhile (e, cmd) ->
      let sub_steps = ref [] in
      while Value.unBool ~pp (interpret_expr upper_steps env e) do
        assert_unit ~config initialCommand (interpret_command sub_steps env cmd)
      done;
      addStep ~sub_steps upper_steps env initialCommand [];
      Value.unit
  | CVerify (x, message) ->
      let v = interpret_expr upper_steps env x in
      addStep upper_steps env initialCommand [];
      ( match v.v with
      | Literal (Bool true) -> Value.unit
      | Literal (Bool false) ->
          let except =
            Base.Option.map ~f:(interpret_expr upper_steps env) message
          in
          let except, message =
            match except with
            | Some except -> (except, [`Br; `Text "Message:"; `Value except])
            | None -> (Value.string (Printer.wrong_condition_string x), [])
          in
          raise
            (ExecFailure
               ( except
               , [`Text "Wrong condition:"; `Expr x; `Rec message; `Line line_no]
               ))
      | _ ->
          failwith
            (Printf.sprintf
               "Failed condition [%s] in line %s"
               (Printer.value_to_string v)
               (string_of_line_no line_no)) )
  | CResult e -> interpret_expr upper_steps env e
  | CComment _ -> Value.unit
  | CSetType _ -> Value.unit
  | CSetResultType (c, _) -> interpret_command upper_steps env c
  | CSetEntryPoint (ep_name, l) ->
      let l = interpret_expr upper_steps env l in
      let l, args = Value.unclosure ~pp:(fun () -> assert false) l in
      let instance = get_current_instance env "set_entry_point" in
      let leps = instance.state.lazy_entry_points in
      let leps = List.remove_assoc ep_name leps in
      let leps = (ep_name, `Closure (l, args)) :: leps in
      let id = get_current_contract_id env "set_entry_point" in
      Hashtbl.add
        env.scenario_state.contracts
        id
        {instance with state = {instance.state with lazy_entry_points = leps}};
      Value.unit
  | CTrace e ->
      let e = interpret_expr upper_steps env e in
      print_endline (Printer.value_to_string e);
      Value.unit

and call_closure ~config upper_steps env (l, args) parameter =
  let parameter =
    List.fold_left (fun acc v -> Value.tuple [v; acc]) parameter args
  in
  let steps = ref [] in
  let variables =
    if l.with_operations
    then List.filter (fun (n, _) -> n = "__operations__") !(env.variables)
    else []
  in
  let variables = ref ((l.name, Constant parameter) :: variables) in
  let env = {env with variables} in
  let r = interpret_command ~config steps env l.body in
  addStep
    ~sub_steps:steps
    upper_steps
    env
    l.body
    [("lambda input", parameter); ("lambda output", r)];
  r

and call_lambda ~config upper_steps env lambda parameter =
  let l, args = Value.unclosure ~pp:(fun () -> assert false) lambda in
  call_closure ~config upper_steps env (l, args) parameter

and eval_view
    ~name ~id ~config ~line_no ~e ~return_type ~types_ok env parameters =
  let optional_return_type = Type.option return_type in
  match Hashtbl.find_opt env.scenario_state.contracts id with
  | None ->
      Printf.ksprintf
        failwith
        "Missing contract %s for view %s"
        (address_of_contract_id id)
        name
  | Some contract ->
      let view =
        List.find_opt
          (fun (view : _ view) -> view.name = name)
          contract.template.tcontract.views
      in
      begin
        match view with
        | None ->
            if config.view_check_exception
            then
              Printf.ksprintf
                failwith
                "Missing view %s in contract %s"
                name
                (address_of_contract_id id)
            else Value.none return_type
        | Some view ->
            let tparameter =
              match get_extra view.tparameter_derived with
              | None -> Type.unit
              | Some parameter -> parameter
            in
            let input_ok, output_ok =
              ( types_ok || Type.equal tparameter (type_of_value parameters)
              , types_ok
                || Type.equal (Type.option view.body.ct) optional_return_type )
            in
            if input_ok && output_ok
            then
              let env =
                let context =
                  let sender =
                    match env.context.contract_id with
                    | No_contract _ -> env.context.sender
                    | In_contract (id, _) -> Some (address_of_contract_id id)
                  in
                  { env.context with
                    contract_id = In_contract (id, parameters)
                  ; line_no
                  ; amount = Bigint.zero_big_int
                  ; sender }
                in
                {env with context}
              in
              Value.some (interpret_command ~config env.steps env view.body)
            else if config.view_check_exception
            then
              let message =
                match (input_ok, output_ok) with
                | true, true -> assert false
                | true, false -> "Wrong output type"
                | false, true -> "Wrong input type"
                | false, false -> "Wrong input and output type"
              in
              raise
                (ExecFailure
                   ( Value.string "Type error in view"
                   , [ `Text message
                     ; `Expr e
                     ; `Br
                     ; `Text "Expected input type (in the view):"
                     ; `Br
                     ; `Type tparameter
                     ; `Br
                     ; `Text "Actual input type:"
                     ; `Br
                     ; `Type (type_of_value parameters)
                     ; `Br
                     ; `Text "Actual output type (in the view):"
                     ; `Br
                     ; `Type view.body.ct
                     ; `Br
                     ; `Text "Expected output type:"
                     ; `Br
                     ; `Type optional_return_type
                     ; `Line line_no ] ))
            else Value.none return_type
      end

and interpret_message
    ~config ~primitives ~scenario_state context id {channel; params} =
  let context = {context with contract_id = In_contract (id, params)} in
  let env =
    { context
    ; variables =
        ref
          [ ( "__operations__"
            , Heap_ref (ref (Value.build_list [] Type.operation)) ) ]
    ; current_locals = ref []
    ; primitives
    ; steps = ref []
    ; private_variables = []
    ; scenario_state
    ; config }
  in
  let { template =
          {tcontract = {entry_points; private_variables; derived}} as template
      ; state } =
    get_current_instance env "message"
  in
  let env = {env with private_variables} in
  let interpret_command = interpret_command ~config in
  let tparameter = (get_extra derived).tparameter in
  let ep =
    List.find_opt
      (fun ({channel = x} : _ entry_point) -> channel = x)
      entry_points
  in
  let ep =
    match (ep, channel, entry_points) with
    | None, "default", [ep] -> Some ep
    | _ -> ep
  in
  match ep with
  | None ->
      ( None
      , []
      , Some
          (Execution.Exec_failure
             (Printf.ksprintf Value.string "Channel not found: %s" channel, []))
      , [] )
  | Some {channel; lazify; body} ->
      let lazify = Option.default config.lazy_entry_points lazify in
      let ep =
        if lazify
        then
          match List.assoc_opt channel state.lazy_entry_points with
          | None -> assert false
          | Some ep -> ep
        else `Initial
      in
      ( try
          match ep with
          | `Absent ->
              let err = [`Text "lazy entry point not found"] in
              ( None
              , []
              , Some
                  (Execution.Exec_failure
                     (Value.string "Missing entry point", err))
              , !(env.steps) )
          | `Initial ->
              let (_ : value) = interpret_command env.steps env body in
              let operations =
                match List.assoc_opt "__operations__" !(env.variables) with
                | Some (Heap_ref x) ->
                    let pp () =
                      [`Text "Impossible: not a list of operations"]
                    in
                    List.map (Value.unoperation ~pp) (Value.unList ~pp !x)
                | _ -> assert false
              in
              let state = (get_current_instance env "result").state in
              (Some {template; state}, List.rev operations, None, !(env.steps))
          | `Closure (l, args) ->
              let storage =
                match state.storage with
                | Some storage -> storage
                | None -> failwith "no storage"
              in
              let params =
                Value.variant_with_type ~line_no:[] channel params tparameter
              in
              let r =
                call_closure
                  ~config
                  env.steps
                  env
                  (l, args)
                  (Value.tuple [params; storage])
              in
              ( match r.v with
              | Tuple [ops; storage] ->
                  let ops =
                    let pp () = assert false in
                    List.map (Value.unoperation ~pp) (Value.unList ~pp ops)
                  in
                  let state = (get_current_instance env "result").state in
                  let state = {state with storage = Some storage} in
                  (Some {template; state}, List.rev ops, None, !(env.steps))
              | _ -> assert false )
        with
      | Failure f ->
          ( None
          , []
          , Some
              (Execution.Exec_failure
                 (Value.string f, [`Text "Failure:"; `Text f]))
          , !(env.steps) )
      | ExecFailure (value, message) ->
          ( None
          , []
          , Some (Execution.Exec_failure (value, message))
          , !(env.steps) )
      | SmartExcept l ->
          ( None
          , []
          , Some (Execution.Exec_failure (Value.string "Error", l))
          , !(env.steps) ) )

and interpret_expr_external
    ~config ~primitives ~no_env ~(scenario_state : scenario_state) =
  let context =
    build_context
      ~time:scenario_state.time
      ~amount:Big_int.zero_big_int
      ~level:scenario_state.level
      ~voting_powers:[]
      ~line_no:[]
      ~debug:false
      ()
  in
  let context = {context with contract_id = No_contract no_env} in
  let env =
    { context
    ; variables = ref []
    ; current_locals = ref []
    ; primitives
    ; steps = ref []
    ; private_variables = []
    ; scenario_state
    ; config }
  in
  interpret_expr ~config (ref []) env

and interpret_contract
    ~config
    ~primitives
    ~scenario_state
    ({tcontract = {balance; storage; baker; metadata}} as c : tcontract) =
  let conv name =
    interpret_expr_external
      ~config
      ~primitives
      ~no_env:[`Text ("Compute " ^ name)]
      ~scenario_state
  in
  let balance =
    match Option.map (conv "balance") balance with
    | None -> Big_int.zero_big_int
    | Some {v = Literal (Mutez x)} -> x
    | _ -> assert false
  in
  let storage = Option.map (conv "storage") storage in
  let baker = Option.bind baker (fun x -> Value.un_baker (conv "baker" x)) in
  let metadata = List.map (map_snd (Meta.map (conv "storage"))) metadata in
  let lazy_entry_points =
    let f ep =
      let lazify = Option.default config.lazy_entry_points ep.lazify in
      let lazy_no_code = Option.default false ep.lazy_no_code in
      if lazify
      then Some (ep.channel, if lazy_no_code then `Absent else `Initial)
      else None
    in
    List.map_some f c.tcontract.entry_points
  in
  {template = c; state = {balance; storage; baker; metadata; lazy_entry_points}}
