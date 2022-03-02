(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics
open Untyped

type substitutions =
  { contracts : (contract_id, expr) Hashtbl.t
  ; scenario_variables : (string, expr) Hashtbl.t }

val replace_expr : substitutions -> expr -> expr

val replace_command : substitutions -> command -> command

val replace_contract : substitutions -> contract -> contract
