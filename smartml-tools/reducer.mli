(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics
open Typed

val reduce_expr :
     config:Config.t
  -> primitives:(module Primitives.Primitives)
  -> scenario_state:scenario_state
  -> texpr
  -> texpr

val reduce_command :
     config:Config.t
  -> primitives:(module Primitives.Primitives)
  -> scenario_state:scenario_state
  -> tcommand
  -> tcommand

val reduce_contract :
     primitives:(module Primitives.Primitives)
  -> scenario_state:scenario_state
  -> tcontract
  -> tcontract

val reduce_instance :
     primitives:(module Primitives.Primitives)
  -> scenario_state:scenario_state
  -> instance
  -> instance
