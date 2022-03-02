(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils.Misc
open Basics

type loaded_scenario =
  { scenario : tscenario
  ; scenario_state : Basics.scenario_state
  ; warnings : smart_except list list
  ; language : Config.language }

let action_of_json ~primitives ~env x =
  let module M = (val json_getter x : JsonGetter) in
  let open M in
  let default f s v =
    match get s with
    | `Null -> v
    | _ -> f s
  in
  let import_expr_string s =
    Import.import_expr env (Parsexp.Single.parse_string_exn (string s))
  in
  let _import_type_string s =
    Import.import_type env (Parsexp.Single.parse_string_exn (string s))
  in
  let import_contract_id_string s =
    Import.import_contract_id (Parsexp.Single.parse_string_exn (string s))
  in
  let parse_line_no s =
    Import.import_line_no (Parsexp.Single.parse_string_exn (string s))
  in
  let read_address s =
    match Parsexp.Single.parse_string_exn (string s) with
    | Atom _ -> assert false
    | List l ->
      ( match Literal.unAddress (Import.import_literal l) with
      | Some {address} -> address
      | _ -> assert false )
  in
  match string "action" with
  | "newContract" ->
      let contract =
        Import.import_contract
          env
          (Parsexp.Single.parse_string_exn (string "export"))
      in
      let id = import_contract_id_string "id" in
      let line_no = parse_line_no "line_no" in
      New_contract
        { id
        ; contract = contract.contract
        ; line_no
        ; accept_unknown_types = bool "accept_unknown_types"
        ; show = bool "show"
        ; address =
            default
              read_address
              "address"
              (Aux.address_of_contract_id ~html:false id None) }
  | "compute" ->
      let expression = import_expr_string "expression" in
      Compute
        { var = string_of_int (int "id")
        ; expression
        ; line_no = parse_line_no "line_no" }
  | "simulation" ->
      Simulation
        {id = import_contract_id_string "id"; line_no = parse_line_no "line_no"}
  | "message" ->
      let of_seed id =
        match string id with
        | "none" -> None
        | seed ->
            if Base.String.is_prefix seed ~prefix:"seed:"
            then
              let module P = (val primitives : Primitives.Primitives) in
              Some
                (Account
                   (P.Crypto.account_of_seed
                      (String.sub seed 5 (String.length seed - 5))))
            else Some (Address (import_expr_string id))
      in
      let chain_id s =
        if string s = "" then None else Some (import_expr_string "chain_id")
      in
      let params = import_expr_string "params" in
      let line_no = parse_line_no "line_no" in
      let message = string "message" in
      let id = import_contract_id_string "id" in
      Message
        { id
        ; valid =
            default
              import_expr_string
              "valid"
              (Expr.cst ~line_no:[] (Literal.bool true))
        ; exception_ =
            default (fun s -> Some (import_expr_string s)) "exception" None
        ; params
        ; line_no
        ; title = default string "title" ""
        ; messageClass = default string "messageClass" ""
        ; sender = default of_seed "sender" None
        ; source = default of_seed "source" None
        ; chain_id = default chain_id "chain_id" None
        ; time = default (fun s -> Some (import_expr_string s)) "time" None
        ; amount =
            default
              import_expr_string
              "amount"
              (Expr.cst ~line_no:[] (Literal.mutez Big_int.zero_big_int))
        ; level = default (fun s -> Some (import_expr_string s)) "level" None
        ; voting_powers =
            default
              import_expr_string
              "voting_powers"
              (Expr.build_map ~line_no:[] ~big:false ~entries:[])
        ; message
        ; show = default bool "show" true
        ; export = default bool "export" true }
  | "error" -> ScenarioError {message = string "message"}
  | "html" ->
      Html
        { tag = string "tag"
        ; inner = string "inner"
        ; line_no = parse_line_no "line_no" }
  | "verify" ->
      Verify
        { condition = import_expr_string "condition"
        ; line_no = parse_line_no "line_no" }
  | "show" ->
      Show
        { expression = import_expr_string "expression"
        ; html = bool "html"
        ; stripStrings = bool "stripStrings"
        ; compile = bool "compile"
        ; line_no = parse_line_no "line_no" }
  | "dynamic_contract" ->
      DynamicContract
        { id =
            ( match import_contract_id_string "dynamic_id" with
            | C_dynamic dyn -> dyn
            | _ -> assert false )
        ; model_id = import_contract_id_string "model_id"
        ; line_no = parse_line_no "line_no" }
  | "constant" ->
      let primitives = primitives in
      let module P = (val primitives : Primitives.Primitives) in
      (* Get the constant hash if any was provided *)
      let hash =
        match
          Parsexp.Single.parse_string_exn (string "hash") |> Base.Sexp.to_string
        with
        | "None" -> None
        | hash -> Some hash
      in
      ( match
          Parsexp.Single.parse_string_exn (string "kind") |> Base.Sexp.to_string
        with
      | "value" ->
          Prepare_constant_value
            { var = string_of_int (int "id")
            ; hash
            ; expression = import_expr_string "expression"
            ; line_no = parse_line_no "line_no" }
      | s -> failwith ("Unexpected constant kind: " ^ s) )
  | "flag" ->
      let flags = string_list "flag" in
      ( match Config.parse_flag flags with
      | None ->
          raise
            (SmartExcept
               [ `Text "Flag parse errors"
               ; `Text
                   (String.concat
                      "; "
                      (List.map (fun s -> Printf.sprintf "%S" s) flags))
               ; `Line (parse_line_no "line_no") ])
      | Some flag -> Add_flag {flag; line_no = parse_line_no "line_no"} )
  | action -> failwith ("Unknown action: '" ^ action ^ "'")

let check_close_scenario config s =
  let s, warnings = Checker.check_scenario config s in
  let scenario = Closer.close_scenario ~config s in
  let scenario_state = Basics.scenario_state () in
  let language =
    match config.languages with
    | [] -> assert false
    | language :: _ -> language
  in
  {scenario; scenario_state; warnings; language}

let acc_config config actions =
  let initial_config, _, actions =
    List.fold_left
      (fun (initial_config, config, actions) action ->
        let initial_config, config =
          match action with
          | Add_flag {flag; line_no} ->
              let config = Config.apply_flag config flag in
              let initial_config =
                match actions with
                | [] -> config
                | _ ->
                    check_initial_flag ~line_no flag;
                    initial_config
              in
              (initial_config, config)
          | _ -> (initial_config, config)
        in
        (initial_config, config, (config, action) :: actions))
      (config, config, [])
      actions
  in
  (initial_config, List.rev actions)

let load_from_string ~primitives config j =
  let open Yojson.Basic.Util in
  let module Printer = (val Printer.get config : Printer.Printer) in
  let j_actions = member "scenario" j in
  let shortname = Yojson.Basic.Util.(to_string (member "shortname" j)) in
  let env = Import.init_env () in
  let kind = {kind = Yojson.Basic.Util.(to_string (member "kind" j))} in
  let actions =
    List.map
      (action_of_json ~primitives ~env)
      (Yojson.Basic.Util.to_list j_actions)
  in
  let config, actions = acc_config config actions in
  let s = {shortname; actions; kind} in
  check_close_scenario config s
