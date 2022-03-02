(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Control
open Basics
open Utils.Misc

let json_of_address address entry_point =
  let address =
    match entry_point with
    | None -> address
    | Some ep -> Printf.sprintf "%s%%%s" address ep
  in
  J_string address

let json_of_literal config =
  let module Printer = (val Printer.get config : Printer.Printer) in
  let err v =
    failwith
      (Printf.sprintf
         "json_of_literal: %s"
         (Printer.literal_to_sp_string ~html:false v))
  in
  function
  | Literal.Int {i} -> J_int i
  | Bool b -> J_bool b
  | String x -> J_string x
  | Bytes x
   |Bls12_381_g1 x
   |Bls12_381_g2 x
   |Bls12_381_fr x
   |Chest_key x
   |Chest x ->
      J_string ("0x" ^ Misc.Hex.hexcape x)
  | Unit -> J_string ""
  | Mutez amount -> J_string (Printer.ppAmount false amount)
  | Address {address; entry_point} -> json_of_address address entry_point
  | Chain_id s -> J_string ("0x" ^ Misc.Hex.hexcape s)
  | Key_hash s | Key s | Signature s -> J_string s
  | Timestamp s -> J_string (Bigint.string_of_big_int s)
  | (Secret_key _ | Sapling_test_state _ | Sapling_test_transaction _) as v ->
      err v

let rec json_of_value ~config v =
  let module Printer = (val Printer.get config : Printer.Printer) in
  let json_of_value = json_of_value ~config in
  let err () =
    failwith (Printf.sprintf "json_of_value: %s" (Printer.value_to_string v))
  in
  match v.v with
  | Literal l | Bounded (_, l) -> json_of_literal config l
  | Contract _ -> err ()
  | List (_, xs) | Set (_, xs) | Tuple xs -> J_list (List.map json_of_value xs)
  | Record (_, entries) -> J_record (List.map (map_snd json_of_value) entries)
  | Map (_, _, _, entries) ->
      let f (k, v) =
        match k.v with
        | Literal (String x) -> (x, json_of_value v)
        | _ -> err ()
      in
      J_record (List.map f entries)
  | Closure (lambda, []) ->
      let v = Value.closure_init lambda in
      let l =
        Compiler.compile_value ~config ~scenario_vars:String.Map.empty v
      in
      J_record
        [ ( "parameter"
          , Micheline.to_json
              (Michelson.To_micheline.mtype
                 (Typing.mtype_of_type
                    ~with_annots:true
                    (get_extra lambda.tParams))) )
        ; ( "returnType"
          , Micheline.to_json
              (Michelson.To_micheline.mtype
                 (Typing.mtype_of_type
                    ~with_annots:true
                    (get_extra lambda.tResult))) )
        ; ("code", Micheline.to_json (Michelson.To_micheline.literal l)) ]
  | Variant (_row, _layout, name, v) -> J_record [(name, json_of_value v)]
  | Closure (_, _ :: _) | Operation _ -> err ()
  | Ticket (ticketer, content, amount) ->
      J_record
        [ ("address", json_of_address ticketer None)
        ; ("value", json_of_value content)
        ; ("amount", J_int amount) ]

let for_contract ~config vc (compiled_contract : Michelson.tcontract) =
  let rec json_of_meta_expr = function
    | Meta.List l -> J_list (List.map json_of_meta_expr l)
    | Map l ->
        let field (k, v) =
          let k =
            match k.v with
            | Literal (String x) -> x
            | _ -> assert false
          in
          (k, json_of_meta_expr v)
        in
        J_record (List.map field l)
    | Other x -> json_of_value ~config x
    | View name ->
      ( match
          List.find_opt
            (fun (v : Michelson.tinstr Michelson.view) -> v.name = name)
            compiled_contract.views
        with
      | None -> failwith ("view not found: " ^ name)
      | Some {name; pure; doc; tparameter; treturn; offchain_code} ->
          J_record
            ( [("name", J_string name); ("pure", J_bool pure)]
            @ (if doc <> "" then [("description", J_string doc)] else [])
            @ [ ( "implementations"
                , J_list
                    [ J_record
                        [ ( "michelsonStorageView"
                          , J_record
                              ( ( if Option.is_none tparameter
                                then []
                                else
                                  [ ( "parameter"
                                    , Micheline.to_json
                                        (Michelson.To_micheline.mtype
                                           (Option.default
                                              Michelson.mt_unit
                                              tparameter)) ) ] )
                              @ [ ( "returnType"
                                  , Micheline.to_json
                                      (Michelson.To_micheline.mtype
                                         (Michelson.strip_annot_variable
                                            treturn)) )
                                ; ( "code"
                                  , Micheline.to_json
                                      (Sequence
                                         (Michelson.To_micheline.instruction
                                            (Michelson.erase_types_instr
                                               offchain_code))) ) ] ) ) ] ] ) ]
            ) )
  in
  List.map (map_snd json_of_meta_expr) vc.state.metadata
