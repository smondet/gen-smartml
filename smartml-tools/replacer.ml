(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics
open Untyped
open Utils
open Control

type substitutions =
  { contracts : (contract_id, expr) Hashtbl.t
  ; scenario_variables : (string, expr) Hashtbl.t }

let replace_expr_f subst line_no e =
  match e with
  | EPrim0 (EContract_data i) ->
      let subst = Hashtbl.find_opt subst.contracts i in
      ( match subst with
      | Some x -> x
      | None -> {e; line_no} )
  | EVar (i, Scenario) ->
      let subst = Hashtbl.find_opt subst.scenario_variables i in
      ( match subst with
      | Some x -> x
      | None -> {e; line_no} )
  | _ -> {e; line_no}

let replace_alg subst =
  { f_expr = replace_expr_f subst
  ; f_command = (fun line_no c -> {c; line_no})
  ; f_type = (fun t -> t) }

let replace_expr subst = cata_expr (replace_alg subst)

let replace_command subst = cata_command (replace_alg subst)

let replace_entry_point subst (ep : _ entry_point) =
  {ep with body = replace_command subst ep.body}

let replace_contract subst {contract = c} =
  let c =
    { c with
      entry_points = (List.map (replace_entry_point subst)) c.entry_points
    ; private_variables =
        List.map (map_snd (replace_expr subst)) c.private_variables }
  in
  {contract = c}
