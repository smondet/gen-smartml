(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils_pure
open Basics

type sexpr =
  | Atom of string
  | List of sexpr list

let rec display_sexpr = function
  | Atom s -> s
  | List es -> "(" ^ String.concat " " (List.map display_sexpr es) ^ ")"

let export_bool = function
  | true -> Atom "True"
  | false -> Atom "False"

let export_option f = function
  | None -> Atom "None"
  | Some x -> f x

let export_layout layout =
  match Unknown.get layout with
  | None -> "None"
  | Some layout ->
      let leaf Layout.{source; target} =
        if source = target
        then Printf.sprintf "(\"%s\")" source
        else Printf.sprintf "(\"%s as %s\")" source target
      in
      let node l1 l2 = Printf.sprintf "(%s %s)" l1 l2 in
      Printf.sprintf "(Some %s)" (Binary_tree.cata leaf node layout)

let export_type =
  let rec export_type t =
    match Type.getRepr t with
    | T0 t ->
        let t, memo = Michelson_base.Type.string_of_type0 t in
        assert (memo = None);
        t
    | TInt {isNat} ->
      ( match Unknown.get isNat with
      | None -> "intOrNat"
      | Some true -> "nat"
      | Some false -> "int" )
    | TBounded {t; cases} ->
        let final, cases =
          match Unknown.get cases with
          | None -> (false, [])
          | Some {final; cases} -> (final, cases)
        in
        Printf.sprintf
          "(bounded %s %s %s)"
          (export_type t)
          (if final then "True" else "False")
          (String.concat " " (List.map (fun _ -> "bounded not handled") cases))
    | TRecord {row; layout} ->
        Printf.sprintf
          "(record (%s) %s (\"\" -1))"
          (String.concat
             " "
             (List.map
                (fun (s, t) -> Printf.sprintf "(%s %s)" s (export_type t))
                row))
          (export_layout layout)
    | T1 (T_option, t) -> Printf.sprintf "(option %s)" (export_type t)
    | TVariant {layout; row} ->
        Printf.sprintf
          "(variant (%s) %s (\"\" -1))"
          (String.concat
             " "
             (List.map
                (fun (s, t) -> Printf.sprintf "(%s %s)" s (export_type t))
                row))
          (export_layout layout)
    | T1 (T_set, telement) -> Printf.sprintf "(set %s)" (export_type telement)
    | T2 ((T_map | T_big_map), tkey, tvalue) ->
        Printf.sprintf "(map %s %s)" (export_type tkey) (export_type tvalue)
        (* TODO big ? *)
    | T1 (T_contract, t) -> Printf.sprintf "(contract %s)" (export_type t)
    | TSecretKey -> "secret_key"
    | TUnknown {contents = UExact t} -> export_type t
    | TVar x -> Printf.sprintf "(tvar %S)" x
    | TUnknown {contents = UUnknown id} -> Printf.sprintf "(unknown %s)" id
    | TUnknown {contents = URecord _ | UVariant _ | UTuple _} -> assert false
    | TTuple ts ->
        let ts = List.map export_type ts in
        Printf.sprintf "(tuple %s)" (String.concat " " ts)
    | T1 (T_list, t) -> Printf.sprintf "(list %s)" (export_type t)
    | TLambda ({with_storage; with_operations}, t1, t2) ->
        let f x =
          match Unknown.get x with
          | None -> "None"
          | Some b -> if b then "True" else "False"
        in
        let with_storage =
          match Unknown.get with_storage with
          | None | Some None -> "None"
          | Some (Some Read_only) -> "read-only"
          | Some (Some Read_write) -> "read-write"
        in
        Printf.sprintf
          "(lambda %s %s %s %s)"
          with_storage
          (f with_operations)
          (export_type t1)
          (export_type t2)
    | T2 (T_lambda, _, _) -> assert false
    | TSaplingState {memo} ->
        let memo =
          match Unknown.get memo with
          | None -> "unknown"
          | Some i -> string_of_int i
        in
        Printf.sprintf "(sapling_state %s)" memo
    | TSaplingTransaction {memo} ->
        let memo =
          match Unknown.get memo with
          | None -> "unknown"
          | Some i -> string_of_int i
        in
        Printf.sprintf "(sapling_transaction %s)" memo
    | T1 (T_ticket, t) -> Printf.sprintf "(ticket %s)" (export_type t)
    | T2 ((T_pair _ | T_or _), _, _) -> assert false
  in
  export_type

let no_loc = Atom "None"

let loc = function
  | [] -> no_loc
  | (file, no) :: _ ->
      List [Atom (Printf.sprintf "%S" file); Atom (string_of_int no)]

let rec unbind r (c : Untyped.command) =
  match c.c with
  | CBind (None, c1, c2) -> unbind (c1 :: r) c2
  | _ -> List.rev (c :: r)

let export_contract_id = function
  | C_static {static_id} ->
      List [Atom "static_id"; Atom (string_of_int static_id); no_loc]
  | C_dynamic {dynamic_id} ->
      List [Atom "dynamic_id"; Atom (string_of_int dynamic_id); no_loc]

let export_literal : Literal.t -> _ =
  let stringy kind x = List [Atom kind; Atom (Printf.sprintf "%S" x)] in
  let integery kind x = List [Atom kind; Atom (Bigint.string_of_big_int x)] in
  function
  | Unit -> List [Atom "unit"]
  | Bool true -> List [Atom "bool"; Atom "True"]
  | Bool false -> List [Atom "bool"; Atom "False"]
  | Int {i; is_nat} ->
      let c =
        match Unknown.get is_nat with
        | Some true -> "nat"
        | Some false -> "int"
        | None -> "intOrNat"
      in
      List [Atom c; Atom (Bigint.string_of_big_int i)]
  | Mutez x -> integery "mutez" x
  | String x -> stringy "string" x
  | Bytes x -> stringy "bytes" x
  | Chain_id x -> stringy "chain_id_cst" x
  | Timestamp x -> integery "timestamp" x
  | Address {address; entry_point} when entry_point = None ->
      stringy "address" address
  | Address _ -> assert false
  | Key x -> stringy "key" x
  | Secret_key x -> stringy "secret_key" x
  | Key_hash x -> stringy "key_hash" x
  | Signature x -> stringy "signature" x
  | Sapling_test_transaction {source; target; amount; memo} ->
      List
        [ Atom "sapling_test_transaction"
        ; Atom (string_of_int memo)
        ; Atom (Option.default "" source)
        ; Atom (Option.default "" target)
        ; Atom (Big_int.string_of_big_int amount)
        ; Atom "None" ]
  | Bls12_381_g1 x -> stringy "bls12_381_g1" x
  | Bls12_381_g2 x -> stringy "bls12_381_g2" x
  | Bls12_381_fr x -> stringy "bls12_381_fr" x
  | Chest_key x -> stringy "chest_key" x
  | Chest x -> stringy "chest" x
  | Sapling_test_state _ -> assert false

let rec export_expr (e : Untyped.expr) =
  let l = loc e.line_no in
  match e.e with
  | EVar ("__storage__", _) -> List [Atom "data"]
  | EVar (name, Local) -> List [Atom "getLocal"; Atom name; l]
  | EVar (name, Simple) -> List [Atom "iter"; Atom name; l]
  | EVar (_, Scenario) -> assert false
  | EPrivate x -> List [Atom "private"; Atom x; l]
  | ERecord entries ->
      let entries =
        List.map (fun (n, e) -> List [Atom n; export_expr e]) entries
      in
      List (Atom "record" :: l :: entries)
  | EMPrim0 p -> export_mprim0 l p
  | EMPrim1 (p, e) -> export_mprim1 l p e
  | EMPrim2 (p, e1, e2) -> export_mprim2 l p e1 e2
  | EMPrim3 (p, e1, e2, e3) -> export_mprim3 l p e1 e2 e3
  | EPrim0 p -> export_prim0 l p
  | EPrim1 (p, e) -> export_prim1 l p e
  | EPrim2 (p, e1, e2) -> export_prim2 l p e1 e2
  | EPrim3 (p, e1, e2, e3) -> export_prim3 l p e1 e2 e3
  | EItem {items; key; default_value; missing_message} ->
      assert (default_value = None);
      assert (missing_message = None);
      List [Atom "getItem"; export_expr items; export_expr key; l]
  | EMap (big, entries) ->
      let entries =
        List.map (fun (k, v) -> List [export_expr k; export_expr v]) entries
      in
      List (Atom (if big then "big_map" else "map") :: l :: entries)
  | ELambda {name; body} ->
      List [Atom "lambda"; Atom "-1"; Atom name; l; List [export_command body]]
  | ETuple xs -> List (Atom "tuple" :: l :: List.map export_expr xs)
  | EList xs -> List (Atom "list" :: l :: List.map export_expr xs)
  | ESet xs -> List (Atom "set" :: l :: List.map export_expr xs)
  | ECreate_contract {contract_template; baker; balance; storage} ->
      List
        [ Atom "create_contract"
        ; List [Atom "contract"; export_contract contract_template]
        ; List [Atom "storage"; export_expr storage]
        ; List [Atom "baker"; export_expr baker]
        ; List [Atom "amount"; export_expr balance]
        ; l ]
  | ETransfer {arg; amount; destination} ->
      List
        [ Atom "transfer"
        ; export_expr arg
        ; export_expr amount
        ; export_expr destination
        ; l ]
  | EOpenVariant (constructor, arg, missing_message) ->
      let missing_message =
        Option.cata (Atom "None") export_expr missing_message
      in
      List
        [ Atom "openVariant"
        ; export_expr arg
        ; Atom constructor
        ; missing_message
        ; l ]
  | ESaplingVerifyUpdate {state; transaction} ->
      List
        [ Atom "sapling_verify_update"
        ; export_expr state
        ; export_expr transaction
        ; l ]
  | EMichelson ({name = _; parsed = _; typesIn = _; typesOut = _}, args) ->
      let instr = assert false in
      List ([Atom "call_michelson"; instr; l] @ List.map export_expr args)
  | EMapFunction {f; l = l'} ->
      List [Atom "map_function"; export_expr l'; export_expr f; l]
  | EContract {entry_point; arg_type; address} ->
      List
        [ Atom "contract"
        ; Atom (Option.default "\"\"" entry_point)
        ; Atom (export_type arg_type)
        ; export_expr address
        ; l ]
  | ESlice {offset; length; buffer} ->
      List
        [ Atom "slice"
        ; export_expr offset
        ; export_expr length
        ; export_expr buffer
        ; l ]
  | EMake_signature {secret_key; message; message_format} ->
      let message_format =
        match message_format with
        | `Raw -> "raw"
        | `Hex -> "hex"
      in
      List
        [ Atom "make_signature"
        ; export_expr secret_key
        ; export_expr message
        ; Atom message_format
        ; l ]
  | EMatch (scrutinee, clauses) ->
      let export_clause (constructor, e) =
        List [Atom constructor; export_expr e]
      in
      List
        ( Atom "ematch"
        :: l
        :: export_expr scrutinee
        :: List.map export_clause clauses )
  | EHasEntryPoint ep_name -> List [Atom "has_entry_point"; Atom ep_name; l]
  | EMPrim1_fail _ -> assert false
  | EIf (e1, e2, e3) ->
      List [Atom "eif"; export_expr e1; export_expr e2; export_expr e3; l]
  | EIsFailing e -> List [Atom "is_failing"; export_expr e; l]
  | ECatchException (t, e) ->
      List [Atom "catch_exception"; export_expr e; Atom (export_type t); l]

and export_mprim0 l p =
  let open Michelson_base.Primitive in
  let open Michelson_base.Type in
  let _f name = List [Atom name; l] in
  match (p : mtype prim0) with
  | Amount -> List [Atom "amount"]
  | Balance -> List [Atom "balance"]
  | Chain_id -> List [Atom "chain_id"]
  | Level -> List [Atom "level"]
  | Now -> List [Atom "now"]
  | Self None -> List [Atom "self"]
  | Self (Some s) -> List [Atom "self"; Atom s; l]
  | Self_address -> List [Atom "self_address"]
  | Sender -> List [Atom "sender"]
  | Source -> List [Atom "source"]
  | Total_voting_power -> List [Atom "total_voting_power"]
  | Empty_bigmap _ | Empty_map _ | Empty_set _ | Nil _ | None_ _
   |Sapling_empty_state _ | Unit_ ->
      assert false

and export_mprim1 l p e =
  let open Michelson_base.Primitive in
  let open Michelson_base.Type in
  let f name = List [Atom name; export_expr e; l] in
  match (p : mtype prim1) with
  | Abs -> f "abs"
  | Not -> f "not"
  | IsNat -> f "isNat"
  | Blake2b -> f "blake2b"
  | Sha256 -> f "sha256"
  | Sha512 -> f "sha512"
  | Keccak -> f "keccak"
  | Sha3 -> f "sha3"
  | Hash_key -> f "hash_key"
  | Car | Cdr | Some_ | Eq | Neg | Int | Neq | Le | Lt | Ge | Gt | Concat1
   |Size | Address | Implicit_account | Pack | Set_delegate | Read_ticket
   |Join_tickets | Pairing_check | Voting_power | Left _ | Right _
   |Contract _ | Unpack _ | Getn _ | Cast _ | Rename _ ->
      assert false

and export_mprim2 l p e1 e2 =
  let open Michelson_base.Primitive in
  let f name = List [Atom name; export_expr e1; export_expr e2; l] in
  match (p : _ prim2) with
  | Lsl -> f "lsl"
  | Lsr -> f "lsr"
  | Add | Mul | Sub | Xor | Ediv | And | Or | Cons | Compare | Concat2 | Get
   |Mem | Exec | Apply | Sapling_verify_update | Ticket | Split_ticket
   |Pair _ | Updaten _ | View _ ->
      assert false

and export_mprim3 l p e1 e2 e3 =
  let open Michelson_base.Primitive in
  let f name =
    List [Atom name; export_expr e1; export_expr e2; export_expr e3; l]
  in
  match (p : prim3) with
  | Check_signature -> f "check_signature"
  | Slice | Update | Get_and_update | Transfer_tokens | Open_chest ->
      assert false

and export_prim0 l (p : _ prim0) =
  match p with
  | ECst x -> List [Atom "literal"; export_literal x; l]
  | EContract_data id -> List [Atom "contract_data"; export_contract_id id; l]
  | ECstContract _ -> assert false
  | EBounded x -> List [Atom "bounded"; export_literal x; l]
  | EMetaLocal x -> List [Atom "getMetaLocal"; Atom x; l]
  | EMatchCons x -> List [Atom "match_cons"; Atom x; l]
  | EAccount_of_seed {seed} -> List [Atom "account_of_seed"; Atom seed; l]
  | EContract_address (x, ep) ->
      let ep = Option.default "\"\"" ep in
      List [Atom "contract_address"; export_contract_id x; Atom ep; l]
  | EContract_balance x ->
      List [Atom "contract_balance"; export_contract_id x; l]
  | EContract_baker x -> List [Atom "contract_baker"; export_contract_id x; l]
  | EContract_typed (x, ep) ->
      let ep = Option.default "" ep in
      List [Atom "contract_typed"; export_contract_id x; Atom ep; l]
  | EConstant (e, t) -> List [Atom "constant"; Atom e; Atom (export_type t); l]
  | EConstantVar e -> List [Atom "constant_scenario_var"; Atom e; l]

and export_prim1 l (p : _ prim1) e =
  let f name = List [Atom name; export_expr e; l] in
  match p with
  | EAttr name -> List [Atom "attr"; export_expr e; Atom name; l]
  | EType_annotation t ->
      List [Atom "type_annotation"; export_expr e; Atom (export_type t); l]
  | EProject 0 -> f "first"
  | EProject 1 -> f "second"
  | ESum -> f "sum"
  | ENeg -> f "neg"
  | ESign -> f "sign"
  | ESize -> f "size"
  | EToInt -> f "toInt"
  | EPack -> f "pack"
  | EVariant cons -> List [Atom "variant"; Atom cons; export_expr e; l]
  | EConcat_list -> f "concat"
  | EResolve -> f "resolve"
  | EListRev -> f "rev"
  | EListItems rev -> f (if rev then "rev_items" else "items")
  | EListKeys rev -> f (if rev then "rev_keys" else "keys")
  | EListValues rev -> f (if rev then "rev_values" else "values")
  | EListElements rev -> f (if rev then "rev_elements" else "elements")
  | EReadTicket -> f "read_ticket"
  | EJoinTickets -> f "join_tickets"
  | ESetDelegate -> f "set_delegate"
  | EProject _ -> assert false
  | EAddress -> f "to_address"
  | EImplicit_account -> f "implicit_account"
  | EPairingCheck -> f "pairing_check"
  | EVotingPower -> f "voting_power"
  | EUnbounded -> f "unbounded"
  | EUnpack t -> List [Atom "unpack"; export_expr e; Atom (export_type t); l]
  | EIsVariant c -> List [Atom "isVariant"; export_expr e; Atom c; l]
  | EConvert -> f "convert"
  | EStaticView (static_id, view) ->
      List
        [ Atom "static_view"
        ; export_contract_id (C_static static_id)
        ; Atom view
        ; l ]

and export_prim2 l (p : _ prim2) e1 e2 =
  let f name = List [Atom name; export_expr e1; export_expr e2; l] in
  let f_swapped name = List [Atom name; export_expr e2; export_expr e1; l] in
  match p with
  | EApplyLambda -> f_swapped "apply_lambda"
  | ECallLambda -> f_swapped "call_lambda"
  | EBinOp op ->
    ( match op with
    | BNeq -> f "neq"
    | BEq -> f "eq"
    | BAnd -> f "and"
    | BOr -> f "or"
    | BAdd -> f "add"
    | BSub -> f "sub"
    | BDiv -> f "truediv"
    | BEDiv -> f "ediv"
    | BMul {overloaded = false} -> f "mul"
    | BMul {overloaded = true} -> f "mul_overloaded"
    | BMod -> f "mod"
    | BLt -> f "lt"
    | BLe -> f "le"
    | BGt -> f "gt"
    | BGe -> f "ge"
    | BXor -> f "xor"
    | BMin -> f "min"
    | BMax -> f "max" )
  | EContains -> f_swapped "contains"
  | EGetOpt -> List [Atom "getOpt"; export_expr e2; l; export_expr e1]
  | EAdd_seconds -> f "add_seconds"
  | ECons -> f "cons"
  | ETicket -> f "ticket"
  | ESplitTicket -> f "split_ticket"
  | EView (name, return_type) ->
      List
        [ Atom "view"
        ; Atom name
        ; export_expr e1
        ; export_expr e2
        ; Atom (export_type return_type)
        ; l ]

and export_prim3 l (p : _ prim3) e1 e2 e3 =
  let f name =
    List [Atom name; export_expr e1; export_expr e2; export_expr e3; l]
  in
  match p with
  | ERange -> f "range"
  | ESplit_tokens -> f "split_tokens"
  | EUpdate_map ->
      List [Atom "update_map"; export_expr e3; export_expr e1; export_expr e2; l]
  | EGet_and_update ->
      List
        [ Atom "get_and_update"
        ; export_expr e3
        ; export_expr e1
        ; export_expr e2
        ; l ]
  | ETest_ticket -> f "test_ticket"

and export_command (c : Untyped.command) =
  let l = loc c.line_no in
  match c.c with
  | CDefineLocal {var; rhs; is_mutable} ->
      if is_mutable
      then List [Atom "defineLocal"; Atom var; export_expr rhs; l]
      else List [Atom "defineLocal"; Atom var; export_expr rhs; Atom "False"; l]
  | CVerify (e, None) -> List [Atom "verify"; export_expr e; l]
  | CVerify (e, Some msg) ->
      List [Atom "verify"; export_expr e; export_expr msg; l]
  | CSetVar (lhs, rhs) -> List [Atom "set"; export_expr lhs; export_expr rhs; l]
  | CBind (None, _, _) -> List (List.map export_command (unbind [] c))
  | CBind (Some x, c1, c2) ->
      List [List [Atom "bind"; Atom x; export_command c1]; export_command c2]
  | CSetType (e, t) ->
      List [Atom "set_type"; export_expr e; Atom (export_type t); l]
  | CResult e -> List [Atom "result"; export_expr e; l]
  | CFailwith e -> List [Atom "failwith"; export_expr e; l]
  | CNever e -> List [Atom "never"; export_expr e; l]
  | CIf (cond, t, f) ->
      List
        [ List [Atom "ifBlock"; export_expr cond; List [export_command t]; l]
        ; List [Atom "elseBlock"; List [export_command f]] ]
  | CWhile (cond, body) ->
      List [Atom "whileBlock"; export_expr cond; List [export_command body]; l]
  | CFor (name, e, body) ->
      List
        [ Atom "forGroup"
        ; Atom name
        ; export_expr e
        ; List [export_command body]
        ; l ]
  | CMatch (scrutinee, cases) ->
      let f (constructor, arg_name, body) =
        List
          [ Atom "match"
          ; Atom "None"
          ; Atom constructor
          ; Atom arg_name
          ; List [export_command body]
          ; l ]
      in
      List
        [ Atom "match_cases"
        ; export_expr scrutinee
        ; Atom "None"
        ; List (List.map f cases)
        ; l ]
  | CTrace e -> List [Atom "trace"; export_expr e; l]
  | CMatchProduct _ | CModifyProduct _ | CMatchCons _ -> assert false
  | CDelItem (e, k) -> List [Atom "delItem"; export_expr e; export_expr k; l]
  | CUpdateSet (e, k, b) ->
      List [Atom "updateSet"; export_expr e; export_expr k; export_bool b; l]
  | CComment _ -> assert false
  | CSetResultType (c, t) ->
      List
        [ Atom "set_result_type"
        ; List [export_command c]
        ; Atom (export_type t)
        ; l ]
  | CSetEntryPoint (ep_name, new_ep) ->
      List [Atom "set_entry_point"; Atom ep_name; export_expr new_ep; l]

and export_entry_point
    {channel; tparameter_ep; originate; lazify; lazy_no_code; line_no; body} =
  let has_param =
    match tparameter_ep with
    | `Present | `Annotated _ -> true
    | `Absent -> false
  in
  List
    [ Atom channel
    ; export_bool originate
    ; export_option export_bool lazify
    ; export_option export_bool lazy_no_code
    ; export_bool has_param
    ; loc line_no
    ; List [export_command body] ]

and export_contract
    ({ template_id
     ; balance
     ; storage
     ; baker = _ (* TODO handle baker in import.ml? *)
     ; tstorage_explicit
     ; entry_points
     ; entry_points_layout = _
     ; unknown_parts = _ (* TODO move this into "derived"? *)
     ; flags
     ; private_variables
     ; metadata = _
     ; views = _ } :
      _ contract_f) =
  let id =
    match template_id with
    | None -> Atom "(static_id -1 None)" (* TODO *)
    | Some id -> export_contract_id (C_static id)
  in
  let balance =
    match balance with
    | None -> List []
    | Some x -> export_expr x
  in
  let tstorage_explicit =
    match tstorage_explicit with
    | None -> List []
    | Some t -> Atom (export_type t)
  in
  List
    [ Atom "template_id"
    ; id
    ; Atom "storage"
    ; Option.cata (List []) export_expr storage
    ; Atom "storage_type"
    ; List [tstorage_explicit]
    ; Atom "messages"
    ; List (List.map export_entry_point entry_points)
    ; Atom "flags"
    ; List
        (List.map
           (fun x ->
             List (List.map (fun x -> Atom x) (Config.string_of_flag x)))
           flags)
    ; Atom "privates"
    ; List
        (List.map
           (fun (x, e) -> List [Atom x; export_expr e])
           private_variables)
    ; Atom "views"
    ; List []
    ; Atom "entry_points_layout"
    ; List []
    ; Atom "initial_metadata"
    ; List []
    ; Atom "balance"
    ; balance ]

let export_action = function
  | New_contract {id; contract; line_no; accept_unknown_types; show; address = _}
    ->
      `Assoc
        [ ("action", `String "newContract")
        ; ("accept_unknown_types", `Bool accept_unknown_types)
        ; ("export", `String (display_sexpr (export_contract contract)))
        ; ("id", `String (display_sexpr (export_contract_id id)))
        ; ("line_no", `String (display_sexpr (loc line_no)))
        ; ("show", `Bool show) ]
  | Message
      { id
      ; valid
      ; exception_ = _
      ; params
      ; line_no
      ; title = _
      ; messageClass = _
      ; source = _
      ; sender = _
      ; chain_id = _
      ; time = _
      ; amount = _
      ; level = _
      ; voting_powers = _
      ; message
      ; show = _
      ; export = _ } ->
      `Assoc
        [ ("action", `String "message")
        ; ("id", `String (display_sexpr (export_contract_id id)))
        ; ("line_no", `String (display_sexpr (loc line_no)))
        ; ("message", `String message)
        ; ("params", `String (display_sexpr (export_expr params)))
        ; ("valid", `String (display_sexpr (export_expr valid))) ]
  | Verify {condition; line_no} ->
      `Assoc
        [ ("action", `String "verify")
        ; ("condition", `String (display_sexpr (export_expr condition)))
        ; ("line_no", `String (display_sexpr (loc line_no))) ]
  | Html {tag; inner; line_no} ->
      `Assoc
        [ ("action", `String "html")
        ; ("inner", `String inner)
        ; ("tag", `String tag)
        ; ("line_no", `String (display_sexpr (loc line_no))) ]
  | Show {expression; html; stripStrings; compile; line_no} ->
      `Assoc
        [ ("action", `String "show")
        ; ("expression", `String (display_sexpr (export_expr expression)))
        ; ("html", `Bool html)
        ; ("stripStrings", `Bool stripStrings)
        ; ("compile", `Bool compile)
        ; ("line_no", `String (display_sexpr (loc line_no))) ]
  | Compute {var; expression; line_no} ->
      `Assoc
        [ ("action", `String "compute")
        ; ("id", `String var)
        ; ("expression", `String (display_sexpr (export_expr expression)))
        ; ("line_no", `String (display_sexpr (loc line_no))) ]
  | Simulation {id; line_no} ->
      `Assoc
        [ ("action", `String "simulation")
        ; ("id", `String (display_sexpr (export_contract_id id)))
        ; ("line_no", `String (display_sexpr (loc line_no))) ]
  | ScenarioError _ -> `Assoc [("action", `String "error")]
  | Exception _ -> assert false
  | Set_delegate _ -> assert false
  | DynamicContract {id; model_id; line_no} ->
      `Assoc
        [ ("action", `String "dynamic_contract")
        ; ("id", `String (display_sexpr (export_contract_id (C_dynamic id))))
        ; ("model_id", `String (display_sexpr (export_contract_id model_id)))
        ; ("line_no", `String (display_sexpr (loc line_no))) ]
  | Add_flag {flag; line_no} ->
      `Assoc
        [ ("action", `String "flag")
        ; ( "flag"
          , `List (List.map (fun x -> `String x) (Config.string_of_flag flag))
          )
        ; ("line_no", `String (display_sexpr (loc line_no))) ]
  | Prepare_constant_value {var; hash; expression; line_no} ->
      let hash =
        match hash with
        | None -> "None"
        | Some x -> x
      in
      `Assoc
        [ ("action", `String "value")
        ; ("var", `String var)
        ; ("hash", `String hash)
        ; ("expression", `String (display_sexpr (export_expr expression)))
        ; ("line_no", `String (display_sexpr (loc line_no))) ]

let export_scenario ({shortname; kind; actions} : Basics.scenario) =
  `Assoc
    [ ("shortname", `String shortname)
    ; ("longname", `String shortname)
    ; ("scenario", `List (List.map export_action (List.map snd actions)))
    ; ("kind", `String kind.kind) ]

let export_scenarios scenarios =
  let scenarios = List.map export_scenario scenarios in
  let scenarios = `List scenarios in
  Yojson.Basic.pretty_to_string scenarios
