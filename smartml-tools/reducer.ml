(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics
open Typed
open Utils
open Control

let reduce_expr_f ~config ~primitives ~scenario_state line_no et e =
  let e0 = build_texpr ~line_no e et in
  match e with
  | EPrim0 (EContract_address (id, _)) ->
      let e =
        Expr.cst
          ~line_no:[]
          (Literal.address (Interpreter.get_contract_address scenario_state id))
      in
      Checker.check_expr config [] e
  | EPrim1 (EResolve, x) ->
      let v =
        Interpreter.interpret_expr_external
          ~config
          ~primitives
          ~no_env:[`Text "sp.resolve"; `Expr x; `Br; `Line line_no]
          ~scenario_state
          x
      in
      let e = Expr.of_value v in
      Checker.check_expr config [] e
  | _ -> e0

let reduce_talg ~config ~primitives ~scenario_state =
  { f_texpr = reduce_expr_f ~config ~primitives ~scenario_state
  ; f_tcommand = (fun line_no ct c -> build_tcommand ~line_no c ct)
  ; f_ttype = (fun t -> t) }

let reduce_expr ~config ~primitives ~scenario_state =
  cata_texpr (reduce_talg ~config ~primitives ~scenario_state)

let reduce_command ~config ~primitives ~scenario_state =
  cata_tcommand (reduce_talg ~config ~primitives ~scenario_state)

let reduce_contract ~primitives ~scenario_state {tcontract = c} =
  let config = (get_extra c.derived).config in
  let reduce_entry_point (ep : _ entry_point) =
    {ep with body = reduce_command ~config ~primitives ~scenario_state ep.body}
  in
  let c =
    { c with
      entry_points = (List.map reduce_entry_point) c.entry_points
    ; private_variables =
        List.map
          (map_snd (reduce_expr ~config ~primitives ~scenario_state))
          c.private_variables }
  in
  {tcontract = c}

let reduce_instance ~primitives ~scenario_state {template = {tcontract}; state}
    =
  {template = reduce_contract ~primitives ~scenario_state {tcontract}; state}
