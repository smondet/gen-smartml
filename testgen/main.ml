open! Base
open! Stdio

let line_no = []

let config =
  (* SmartML.Config.{ default with simplify = false; simplify_via_michel = false } *)
  SmartML.Config.default

let type_check = Tools.Checker.check_contract config

let compile tc =
  let instance =
    SmartML.Basics.
      {
        template = tc;
        state =
          {
            balance = Utils_pure.Bigint.of_int 100;
            storage = None;
            baker = None;
            lazy_entry_points = [];
            metadata = [];
          };
      }
  in
  Tools.Compiler.compile_instance ~scenario_vars:Utils.String.Map.empty instance

module Shell = struct
  let command_to_string_list cmd =
    let open Caml in
    let i = Unix.open_process_in cmd in
    let rec loop acc =
      try loop (input_line i :: acc) with _ -> List.rev acc
    in
    let res = loop [] in
    let status = Unix.close_process_in i in
    match status with
    | Unix.WEXITED 0 -> res
    | _ -> Fmt.failwith "Command %S returned non-zero" cmd

  exception Command_failed of { cmd : string; out : string; err : string }

  let command_to_out_err cmd =
    let open Caml in
    let out = Filename.temp_file "cmd" ".out" in
    let err = Filename.temp_file "cmd" ".err" in
    match
      Fmt.kstr Sys.command "%s > %s 2> %s" cmd (Filename.quote out)
        (Filename.quote err)
    with
    | 0 -> (out, err)
    | other -> raise (Command_failed { out; err; cmd })

  let exec_to_string_list l =
    command_to_string_list
      (String.concat ~sep:" " (List.map l ~f:Caml.Filename.quote))

  let exec_to_out_err l =
    command_to_out_err
      (String.concat ~sep:" " (List.map l ~f:Caml.Filename.quote))
end

module Mockup = struct
  let base_dir = "_build/mockup-client"

  let exec = "../octez/tezos-client"

  let call args =
    Shell.exec_to_out_err
      ([ exec; "--base-dir"; base_dir; "--mode"; "mockup" ] @ args)

  let call_unit args = call args |> ignore

  let init () =
    Fmt.kstr Shell.command_to_string_list "rm -fr %s" base_dir |> ignore;
    call_unit
      [
        "--protocol";
        "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK";
        "create";
        "mockup";
      ];
    ()
end

let count = ref 0

let try_contract c =
  let id = !count in
  Int.incr count;
  let pptmp_file n pp v =
    let tmp = Caml.Filename.concat "_build" (Fmt.str "testgen-%04d-%s" id n) in
    let data = Fmt.str "%a" pp v in
    Out_channel.write_all tmp ~data;
    Fmt.epr "    - %s -> %s\n%!" n tmp;
    tmp
  in
  Fmt.epr "Contract %04d\n%!" id;
  try
    let pptmp n pp v = pptmp_file n pp v |> ignore in
    let checked = type_check c#contract in
    pptmp "Type-checked" SmartML.Basics.pp_tcontract checked;
    let compiled = compile checked in
    pptmp "Compiled" Tools.Michelson.pp_tcontract compiled;
    let concrete = Tools.Michelson.display_tcontract compiled in
    let source = pptmp_file "Concrete" Fmt.string concrete in
    let _storage (* Always Unit because we don't set it in `compile` *) =
      match compiled.Tools.Michelson.storage with
      | Some tcom ->
          let v = Tools.Michelson.string_of_tliteral tcom in
          Fmt.epr "    - Storage -> %s\n%!" v;
          v
      | None -> "Unit"
    in
    List.iter c#inits ~f:(fun mich ->
        let client_id = Fmt.str "c%04d" id in
        let origination =
          try
            Mockup.call_unit
              [
                "originate";
                "contract";
                client_id;
                "transferring";
                "10";
                "from";
                "bootstrap1";
                "running";
                source;
                "--init";
                mich;
                "--burn-cap";
                "10";
              ];
            let address =
              Mockup.call [ "show"; "known"; "contract"; client_id ]
              |> fst |> In_channel.read_lines |> String.concat ~sep:""
            in
            Fmt.str "Success: %s" address
          with Shell.Command_failed { cmd; out; err } ->
            Fmt.str "Failed (cf. %s & %s)" out err
        in
        Fmt.epr "    - Origination with %s:\n      %s\n" mich origination;
        ())
  with e -> Fmt.epr "    - Exception: %a\n%!" Exn.pp e

let contractize ?(inits = [ "Unit" ]) ?storage eps =
  let open SmartML in
  let open Basics in
  let entry_points_layout =
    let open Utils_pure.Binary_tree in
    right_comb
      (List.map eps ~f:(fun (ep : _ entry_point) ->
           Layout.{ source = ep.channel; target = ep.channel }))
  in
  object
    method contract = build_contract ?storage ~entry_points_layout eps
    (* ~flags:[ Config.(Bool_Flag (Simplify, false)) ] *)

    method inits = inits
  end

let default_receive =
  let open SmartML in
  let open Basics in
  build_entry_point ~name:"default" ~tparameter:Type.unit
    Command.(
      seq ~line_no
        [ (* comment ~line_no "This is the default entry point, it does nothing" *) ])

let easydelegation using =
  let open SmartML in
  let open Basics in
  let tparameter =
    Type.(
      record_default_layout SmartML.Config.Comb
        [ ("delegate", key_hash); ("some_other", nat ()) ])
  in
  build_entry_point ~name:"easy_delegation" ~tparameter
    Command.(
      seq ~line_no
        [
          (* comment ~line_no "This just allows anyone to delegate";
             comment ~line_no "more text";
             comment ~line_no "more text";
             comment ~line_no "more text"; *)
          (* define_local ~line_no "param"
             Expr.(type_annotation ~line_no ~t:tparameter (params ~line_no))
             true; *)
          Library.set_delegate ~line_no
            (match using with
            | `None -> Expr.none ~line_no (*  WORKS! *)
            | `Param ->
                Expr.(
                  some ~line_no
                    (attr ~line_no ~name:"delegate"
                       (type_annotation ~line_no ~t:tparameter (params ~line_no)))));
        ])

let send_back =
  let open SmartML in
  let open Basics in
  build_entry_point ~name:"sendback" ~tparameter:Type.unit
    Command.(
      seq ~line_no
        [
          (* comment ~line_no "This is the default entry point, it does nothing"; *)
          Library.send ~line_no (Expr.sender ~line_no) (Expr.amount ~line_no);
        ])

let checkadmin =
  let open SmartML in
  let open Basics in
  build_entry_point ~name:"admincheck" ~tparameter:Type.unit
    Command.(
      seq ~line_no
        [
          (* comment ~line_no "This is the default entry point, it does nothing"; *)
          Command.verify ~line_no
            Expr.(
              bin_op ~line_no ~op:BEq (sender ~line_no)
                (attr ~line_no ~name:"admin"
                   (storage ~line_no)
                   (* (contract_data
                      (Ids.C_static { static_id = 0 })
                      ~line_no) *)))
            None;
        ])

let all =
  [
    contractize [ default_receive; easydelegation `None ];
    contractize [ default_receive; easydelegation `Param ];
    contractize [ default_receive; send_back ];
    contractize
      [ default_receive; checkadmin ]
      ~inits:[ "\"tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb\"" ]
      ~storage:
        SmartML.(
          let t =
            Type.(
              record_default_layout SmartML.Config.Comb
                [ ("admin", address) (* ("extra", address) *) ])
          in
          Expr.(
            record ~line_no
              [
                ( "admin",
                  cst ~line_no
                    (Literal.address "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb") );
                (* ( "extra",
                   cst ~line_no
                     (Literal.address "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb") ) *)
              ]
            |> type_annotation ~line_no ~t));
  ]

let () =
  Stdlib.Printexc.register_printer (function
    | SmartML.Basics.SmartExcept errs ->
        Some
          (Fmt.str "smartml-error: %a"
             (Fmt.list SmartML.Basics.pp_smart_except)
             errs)
    | _ -> None);
  Mockup.init ();
  List.iter all ~f:try_contract;
  Fmt.epr "Done\n%!"
