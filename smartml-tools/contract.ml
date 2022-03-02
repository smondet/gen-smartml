(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Basics
open Interpreter

type t = instance [@@deriving show {with_path = false}]

let execMessageInner
    ~config
    ~primitives
    ~scenario_state
    ~title
    ~execMessageClass
    ~context
    ~channel
    ~params
    ~id
    ~line_no =
  let module Printer = (val Printer.get config : Printer.Printer) in
  let init_contract =
    match Hashtbl.find_opt scenario_state.contracts id with
    | None ->
        raise
          (SmartExcept
             [ `Text
                 (Printf.sprintf
                    "Missing contract in scenario %s"
                    (Printer.string_of_contract_id id))
             ; `Line line_no ])
    | Some contract -> contract
  in
  let initial_balance = init_contract.state.balance in
  let init_contract =
    let state = init_contract.state in
    let state =
      { state with
        balance =
          Big_int.add_big_int (Interpreter.context_amount context) state.balance
      }
    in
    {init_contract with state}
  in
  Hashtbl.add scenario_state.contracts id init_contract;
  let contract, operations, error, steps =
    interpret_message
      ~config
      ~primitives
      ~scenario_state
      context
      id
      {channel; params}
  in
  let balance, contract =
    match contract with
    | None -> (initial_balance, None)
    | Some contract -> (contract.state.balance, Some contract)
  in
  let rec pp_step (step : Execution.step) =
    let steps =
      match !(step.substeps) with
      | [] -> ""
      | steps ->
          Printf.sprintf
            "<h4>SubSteps</h4><div class='steps'>%s</div>"
            (String.concat "\n" (List.rev_map pp_step steps))
    in
    let locals =
      match step.locals with
      | [] -> ""
      | locals ->
          Printf.sprintf
            "<h4>Locals</h4> %s"
            (Printer.value_to_string
               ~options:Printer.Options.html
               (Value.record_comb locals))
    in
    let elements =
      match step.elements with
      | [] -> ""
      | elements ->
          Printf.sprintf
            "<h4>Elements</h4> %s"
            (Printer.value_to_string
               ~options:Printer.Options.html
               (Value.record_comb elements))
    in
    let iters =
      match step.iters with
      | [] -> ""
      | iters ->
          Printf.sprintf
            "<h4>Iterators</h4> %s"
            (Printer.value_to_string
               ~options:Printer.Options.html
               (Value.record_comb (List.map (fun (n, (v, _)) -> (n, v)) iters)))
    in
    Format.asprintf
      "<div class='steps'><h4>Command %a</h4><div \
       class='white-space-pre-wrap'>%s</div>%s%s%s%s<h4>Storage</h4>%s</div>"
      Html.pp_render
      (Html.showLine step.command.line_no)
      (Printer.tcommand_to_string step.command)
      steps
      locals
      iters
      elements
      (Printer.value_to_string ~options:Printer.Options.html step.storage)
  in
  let html ok output =
    let transaction =
      let line_no = context_line_no context in
      let hd_line_no =
        match line_no with
        | x :: _ -> x
        | [] -> ("", -1)
      in
      Printf.sprintf
        "<div class='execMessage execMessage%s%s'>%s<h3>Transaction [%s] by \
         [%s] at time [timestamp(%s)] to %s</h3>%s<h3>Balance: \
         %s</h3>%s</div>%s(<button class='text-button' \
         onClick='showLine(%i)'>line %i</button>)"
        ok
        execMessageClass
        title
        ok
        ( match context_sender context with
        | None -> ""
        | Some x ->
            Printer.value_to_string
              ~options:Printer.Options.html
              (Value.literal (Literal.address x)) )
        (Big_int.string_of_big_int (context_time context))
        (Aux.address_of_contract_id ~html:true id None)
        (Printer.value_to_string
           ~options:Printer.Options.html
           (* !checkImport *)
           (let row = [(channel, type_of_value params)] in
            Value.variant_with_type
              ~line_no
              channel
              params
              (Type.variant_default_layout Config.Comb row)))
        (Printer.ppAmount true balance)
        output
        ( if context_debug context
        then
          Printf.sprintf
            "<h3>Steps</h3><div>%s</div>"
            (String.concat "\n" (List.rev_map pp_step steps))
        else "" )
        (snd hd_line_no)
        (snd hd_line_no)
    in
    let michelson =
      let initContract = init_contract.template.tcontract in
      match
        List.find_opt
          (fun (x : _ entry_point) -> x.channel = channel)
          initContract.entry_points
      with
      | None -> "Missing entry point."
      | Some {originate; line_no} ->
          let params =
            if originate
            then
              match initContract.entry_points with
              | [_] -> params
              | _ ->
                  Value.variant_with_type
                    ~line_no:(line_no @ context_line_no context)
                    channel
                    params
                    (get_extra initContract.derived).tparameter
            else params
          in
          let scenario_vars = String.Map.of_hashtbl scenario_state.variables in
          let params = Compiler.compile_value ~config ~scenario_vars params in
          let mich =
            Html.render
              (Html.copy_div
                 ~id:"transaction_params"
                 ~className:"white-space-pre-wrap"
                 "paramsCode"
                 (Raw (Michelson.string_of_literal params)))
          in
          let json =
            Html.render
              (Html.copy_div
                 ~id:"transaction_params_json"
                 "paramsCodeJson"
                 (Raw
                    (Format.asprintf
                       "<div class='white-space-pre'>%a</div>"
                       (Micheline.pp_as_json ())
                       (Michelson.To_micheline.literal params))))
          in
          let result =
            let myTabs =
              [ Html.tab "Parameter Michelson" ~active:() (Raw mich)
              ; Html.tab "Parameter JSON" (Raw json) ]
            in
            Html.render (Html.tabs "Generated Michelson" myTabs)
          in
          if originate
          then result
          else
            Printf.sprintf
              "Warning: private entry point, showing only entry point specific \
               Michelson. %s"
              result
    in
    let myTabs =
      [ Html.tab "Summary" ~active:() (Raw transaction)
      ; Html.tab "Michelson" (Raw michelson)
      ; Html.tab "X" (Raw "") ]
    in
    Html.render (Html.tabs "Transaction" myTabs)
  in
  match contract with
  | Some {state = {storage}} ->
      let storage =
        match storage with
        | None -> failwith "Cannot simulate contract without a storage"
        | Some storage -> storage
      in
      let output =
        let operations =
          List.map
            (Printer.operation_to_string ~options:Printer.Options.html)
            operations
        in
        Printf.sprintf
          "<h3>Operations:</h3>%s<h3>Storage:</h3><div class='subtype'>%s</div>"
          (String.concat " " operations)
          (Printer.html_of_data Printer.Options.html storage)
      in
      { Execution.ok = true
      ; contract
      ; operations
      ; error
      ; html = html "OK" output
      ; storage
      ; steps }
  | None ->
      let errorMessage =
        match error with
        | Some (Exec_failure (_, message)) ->
            Printer.pp_smart_except true message
        | None -> ""
      in
      let output = Printf.sprintf "<h3>Error:</h3>%s" errorMessage in
      { ok = false
      ; contract
      ; operations
      ; error
      ; html = html "KO" output
      ; storage = Value.unit
      ; steps }
