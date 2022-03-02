(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics
open Typed
open Untyped

val check_command : Config.t -> (string * Type.t) list -> command -> tcommand
(** Typechecks the given command in the given environment. *)

val check_expr : Config.t -> (string * Type.t) list -> expr -> texpr

val check_lambda : Config.t -> (string * Type.t) list -> lambda -> tlambda

val check_contract : Config.t -> contract -> tcontract

val check_scenario : Config.t -> scenario -> tscenario * smart_except list list
