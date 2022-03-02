(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Basics
open Untyped

type t = expr [@@deriving show {with_path = false}]

type nullary_expr = line_no:line_no -> t

type unary_expr = line_no:line_no -> t -> t

type binary_expr = line_no:line_no -> t -> t -> t

type ternary_expr = line_no:line_no -> t -> t -> t -> t

let build ~line_no e = {e; line_no}

let lsl_ ~line_no x y = build ~line_no (EMPrim2 (Lsl, x, y))

let lsr_ ~line_no x y = build ~line_no (EMPrim2 (Lsr, x, y))

let bin_op ~op ~line_no x y = build ~line_no (EPrim2 (EBinOp op, x, y))

let storage ~line_no = build ~line_no (EVar ("__storage__", Local))

let attr ~name ~line_no x = build ~line_no (EPrim1 (EAttr name, x))

let variant ~name ~line_no x = build ~line_no (EPrim1 (EVariant name, x))

let isVariant ~name ~line_no x = build ~line_no (EPrim1 (EIsVariant name, x))

let variable ~line_no arg_name = build ~line_no (EVar (arg_name, Simple))

let openVariant ~line_no name x missing_message =
  build ~line_no (EOpenVariant (name, x, missing_message))

let updateMap ~line_no key value map =
  build ~line_no (EPrim3 (EUpdate_map, key, value, map))

let params ~line_no = build ~line_no (EVar ("__parameter__", Local))

let local ~line_no n = build ~line_no (EVar (n, Local))

let meta_local ~line_no n = build ~line_no (EPrim0 (EMetaLocal n))

let operations ~line_no = local ~line_no "__operations__"

let private_ ~line_no n = build ~line_no (EPrivate n)

let item ~line_no items key default_value missing_message =
  build ~line_no (EItem {items; key; default_value; missing_message})

let contains ~line_no member items =
  build ~line_no (EPrim2 (EContains, member, items))

let sum ~line_no l = build ~line_no (EPrim1 (ESum, l))

let range ~line_no a b step = build ~line_no (EPrim3 (ERange, a, b, step))

let cons ~line_no x l = build ~line_no (EPrim2 (ECons, x, l))

let cst ~line_no x = build ~line_no (EPrim0 (ECst x))

let bounded ~line_no x = build ~line_no (EPrim0 (EBounded x))

let unbounded ~line_no x = build ~line_no (EPrim1 (EUnbounded, x))

let is_failing ~line_no x = build ~line_no (EIsFailing x)

let catch_exception ~t ~line_no x = build ~line_no (ECatchException (t, x))

let unit = cst ~line_no:[] Literal.unit

let type_annotation ~t ~line_no e =
  build ~line_no (EPrim1 (EType_annotation t, e))

let has_entry_point ep_name ~line_no = build ~line_no (EHasEntryPoint ep_name)

let convert ~line_no x = build ~line_no (EPrim1 (EConvert, x))

let record ~line_no entries = build ~line_no (ERecord entries)

let build_list ~line_no ~elems = build ~line_no (EList elems)

let build_map ~line_no ~big ~entries = build ~line_no (EMap (big, entries))

let build_set ~line_no ~entries = build ~line_no (ESet entries)

let hash_key ~line_no e = build ~line_no (EMPrim1 (Hash_key, e))

let blake2b ~line_no e = {e = EMPrim1 (Blake2b, e); line_no}

let sha256 ~line_no e = {e = EMPrim1 (Sha256, e); line_no}

let sha512 ~line_no e = {e = EMPrim1 (Sha512, e); line_no}

let keccak ~line_no e = {e = EMPrim1 (Keccak, e); line_no}

let sha3 ~line_no e = {e = EMPrim1 (Sha3, e); line_no}

let pack ~line_no e = build ~line_no (EPrim1 (EPack, e))

let unpack ~line_no e t = build ~line_no (EPrim1 (EUnpack t, e))

let check_signature ~line_no message signature pk =
  build ~line_no (EMPrim3 (Check_signature, message, signature, pk))

let account_of_seed ~seed ~line_no =
  build ~line_no (EPrim0 (EAccount_of_seed {seed}))

let make_signature ~line_no ~secret_key ~message ~message_format =
  build ~line_no (EMake_signature {secret_key; message; message_format})

let scenario_var ~line_no id = build ~line_no (EVar (id, Scenario))

let resolve ~line_no e = build ~line_no (EPrim1 (EResolve, e))

let constant ~line_no e t = build ~line_no (EPrim0 (EConstant (e, t)))

let constant_scenario_var ~line_no e = build ~line_no (EPrim0 (EConstantVar e))

let split_tokens ~line_no mutez quantity total =
  build ~line_no (EPrim3 (ESplit_tokens, mutez, quantity, total))

let now = build (EMPrim0 Now)

let chain_id = build (EMPrim0 Chain_id)

let balance = build (EMPrim0 Balance)

let sender = build (EMPrim0 Sender)

let source = build (EMPrim0 Source)

let amount = build (EMPrim0 Amount)

let level = build (EMPrim0 Level)

let total_voting_power = build (EMPrim0 Total_voting_power)

let self = build (EMPrim0 (Self None))

let self_address = build (EMPrim0 Self_address)

let add_seconds ~line_no t s = build ~line_no (EPrim2 (EAdd_seconds, t, s))

let notE ~line_no x = build ~line_no (EMPrim1 (Not, x))

let absE ~line_no x = build ~line_no (EMPrim1 (Abs, x))

let to_int ~line_no x = build ~line_no (EPrim1 (EToInt, x))

let is_nat ~line_no x = build ~line_no (EMPrim1 (IsNat, x))

let negE ~line_no x = build ~line_no (EPrim1 (ENeg, x))

let signE ~line_no x = build ~line_no (EPrim1 (ESign, x))

let slice ~line_no ~offset ~length ~buffer =
  build ~line_no (ESlice {offset; length; buffer})

let concat_list ~line_no l = build ~line_no (EPrim1 (EConcat_list, l))

let size ~line_no s = build ~line_no (EPrim1 (ESize, s))

let match_cons ~line_no name = build ~line_no (EPrim0 (EMatchCons name))

let self_entry_point ~line_no name = build ~line_no (EMPrim0 (Self (Some name)))

let to_address ~line_no e =
  match e.e with
  | EMPrim0 (Self None) -> build ~line_no (EMPrim0 Self_address)
  | _ -> build ~line_no (EPrim1 (EAddress, e))

let implicit_account ~line_no e = build ~line_no (EPrim1 (EImplicit_account, e))

let voting_power ~line_no e = build ~line_no (EPrim1 (EVotingPower, e))

let listRev ~line_no e = build ~line_no (EPrim1 (EListRev, e))

let listItems ~line_no e rev = build ~line_no (EPrim1 (EListItems rev, e))

let listKeys ~line_no e rev = build ~line_no (EPrim1 (EListKeys rev, e))

let listValues ~line_no e rev = build ~line_no (EPrim1 (EListValues rev, e))

let listElements ~line_no e rev = build ~line_no (EPrim1 (EListElements rev, e))

let contract ~line_no entry_point arg_type address =
  build ~line_no (EContract {entry_point; arg_type; address})

let view ~line_no name address param return_type =
  build ~line_no (EPrim2 (EView (name, return_type), address, param))

let static_view ~line_no static_id name param =
  build ~line_no (EPrim1 (EStaticView (static_id, name), param))

let tuple ~line_no es = build ~line_no (ETuple es)

let proj ~line_no i e = build ~line_no (EPrim1 (EProject i, e))

let first ~line_no = proj ~line_no 0

let second ~line_no = proj ~line_no 1

let none ~line_no = variant ~line_no ~name:"None" unit

let some ~line_no e = variant ~line_no ~name:"Some" e

let left ~line_no l = variant ~line_no ~name:"Left" l

let right ~line_no r = variant ~line_no ~name:"Right" r

let inline_michelson ~line_no michelson exprs =
  build ~line_no (EMichelson (michelson, exprs))

let map_function ~line_no l f = build ~line_no (EMapFunction {l; f})

let call_lambda ~line_no parameter lambda =
  build ~line_no (EPrim2 (ECallLambda, parameter, lambda))

let apply_lambda ~line_no parameter lambda =
  build ~line_no (EPrim2 (EApplyLambda, parameter, lambda))

let lambda ~line_no name body ~clean_stack ~with_storage ~with_operations =
  build
    ~line_no
    (ELambda
       { name
       ; body
       ; clean_stack
       ; with_storage
       ; with_operations
       ; tParams = U
       ; tResult = U })

let lambdaParams ~line_no name = build ~line_no (EVar (name, Simple))

let create_contract ~line_no ~baker ~balance ~storage ({contract} : contract) =
  build
    ~line_no
    (ECreate_contract {contract_template = contract; baker; balance; storage})

let sapling_empty_state memo =
  build ~line_no:[] (EMPrim0 (Sapling_empty_state {memo}))

let sapling_verify_update ~line_no state transaction =
  build ~line_no (ESaplingVerifyUpdate {state; transaction})

let set_delegate ~line_no e = build ~line_no (EPrim1 (ESetDelegate, e))

let transfer ~line_no ~arg ~amount ~destination =
  build ~line_no (ETransfer {arg; amount; destination})

let contract_address ~line_no entry_point e =
  build ~line_no (EPrim0 (EContract_address (e, entry_point)))

let contract_typed ~line_no entry_point e =
  build ~line_no (EPrim0 (EContract_typed (e, entry_point)))

let contract_data ~line_no e = build ~line_no (EPrim0 (EContract_data e))

let contract_balance ~line_no e = build ~line_no (EPrim0 (EContract_balance e))

let contract_baker ~line_no e = build ~line_no (EPrim0 (EContract_baker e))

let ematch ~line_no scrutinee clauses =
  build ~line_no (EMatch (scrutinee, clauses))

let eif ~line_no cond a b = build ~line_no (EIf (cond, a, b))

let allow_lambda_full_stack ({e; line_no} as x) =
  match e with
  | ELambda params -> build ~line_no (ELambda {params with clean_stack = false})
  | _ -> x

let test_ticket ~line_no ticketer content amount =
  build ~line_no (EPrim3 (ETest_ticket, ticketer, content, amount))

let build_ticket ~line_no content amount =
  build ~line_no (EPrim2 (ETicket, content, amount))

let read_ticket ~line_no ticket = build ~line_no (EPrim1 (EReadTicket, ticket))

let split_ticket ~line_no ticket decomposition =
  build ~line_no (EPrim2 (ESplitTicket, ticket, decomposition))

let join_tickets ~line_no tickets =
  build ~line_no (EPrim1 (EJoinTickets, tickets))

let pairing_check ~line_no pairs =
  build ~line_no (EPrim1 (EPairingCheck, pairs))

let get_and_update ~line_no key value map =
  build ~line_no (EPrim3 (EGet_and_update, key, value, map))

let open_chest ~line_no chest_key chest time =
  build ~line_no (EMPrim3 (Open_chest, chest_key, chest, time))

let getOpt ~line_no k m = build ~line_no (EPrim2 (EGetOpt, k, m))

let rec of_value v : expr =
  let line_no = [] in
  match v.v with
  | Literal l -> cst ~line_no l
  | Bounded (_, l) -> bounded ~line_no l
  | Contract x -> {e = EPrim0 (ECstContract x); line_no}
  | Record (_, entries) ->
      build
        ~line_no
        (ERecord (entries |> List.map (fun (fld, l) -> (fld, of_value l))))
  | Variant (_row, _layout, lbl, arg) ->
      build ~line_no (EPrim1 (EVariant lbl, of_value arg))
  | List (_, elems) -> build ~line_no (EList (List.map of_value elems))
  | Set (_, elems) -> build_set ~line_no ~entries:(List.map of_value elems)
  | Map (_, _, big, entries) ->
      build
        ~line_no
        (EMap (big, entries |> List.map (fun (k, v) -> (of_value k, of_value v))))
  | Tuple vs -> tuple ~line_no (List.map of_value vs)
  | Closure ({name; body}, args) ->
      List.fold_left
        (fun f arg -> apply_lambda ~line_no f (of_value arg))
        (lambda
           ~line_no
           name
           (erase_types_command body)
           ~clean_stack:true
           ~with_storage:None
           ~with_operations:false)
        args
  | Operation _ -> failwith "TODO expr.of_value Operation"
  | Ticket (ticketer, content, amount) ->
      let ticketer = cst ~line_no (Literal.address ticketer) in
      let content = of_value content in
      let amount = cst ~line_no (Literal.int amount) in
      test_ticket ~line_no ticketer content amount
