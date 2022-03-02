(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics
open Typed

val close_type : config:Config.t -> Type.t -> Type.t
(** Closes a type: occurrences of {! Type.t} {! Unknown.t} are replaced with {! Type.t}. *)

val close_expr : config:Config.t -> texpr -> texpr
(** Closes all types occurring in the given expression. *)

val close_command : config:Config.t -> tcommand -> tcommand
(** Closes all types occurring in the given command. *)

val close_contract : tcontract -> tcontract
(** Closes all types occurring in the given contract. *)

val close_scenario : config:Config.t -> tscenario -> tscenario
(** Closes all types occurring in the given scenario. *)
