(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Basics

type pre_scenario =
  { shortname : string
  ; actions :
      (Untyped.expr, Untyped.command, Type.t, untyped) action_f list Lazy.t
  ; flags : Config.flag list
  ; is_default : bool
  ; kind : scenario_kind }

let scenarios : pre_scenario list ref = ref []

let clear () = scenarios := []

let register ?(flags = []) ?(is_default = true) ~name ~kind actions =
  let scenario =
    {shortname = name; actions; is_default; flags; kind = {kind}}
  in
  scenarios := scenario :: !scenarios

let register_test ?(is_default = true) ~name actions =
  register ~is_default ~name ~kind:"test" actions

let register_compilation ?(flags = []) ?(is_default = false) ~name {contract} =
  let id = Ids.C_static {static_id = 0} in
  (* let add_flag flag = Add_flag {flag; line_no = []} in
   * let flags = List.map add_flag flags in *)
  let action =
    Basics.New_contract
      { id
      ; contract
      ; line_no = []
      ; accept_unknown_types = false
      ; show = true
      ; address = Basics.address_of_contract_id id }
  in
  register ~is_default ~name ~flags ~kind:"compilation" (lazy [action])

let to_json ~all name =
  let scenarios = List.rev !scenarios in
  let scenarios =
    match name with
    | None -> List.filter (fun x -> x.is_default || all) scenarios
    | Some name -> List.filter (fun x -> x.shortname = name) scenarios
  in
  let scenario_of_pre_scenario {shortname; actions; kind} =
    ( { shortname
      ; actions =
          List.map (fun action -> (Config.default, action)) (Lazy.force actions)
      ; kind }
      : scenario )
  in
  Export.export_scenarios (List.map scenario_of_pre_scenario scenarios)

let names_kinds_json () =
  let name_kind ({shortname; kind} : pre_scenario) =
    `List [`String shortname; `String kind.kind]
  in
  Yojson.to_string (`List (List.map name_kind (List.rev !scenarios)))

let dump target_file =
  Utils_pure.Io.write_file target_file (to_json ~all:true None)

let to_json = to_json ~all:false
