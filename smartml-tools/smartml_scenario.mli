(* Copyright 2019-2021 Smart Chain Arena LLC. *)

module Scenario_bak = Scenario
open SmartML
module Scenario = Scenario_bak
open Basics

(** {1 In-browser} *)

val run_scenario_browser :
     primitives:(module Primitives.Primitives)
  -> scenario:string
  -> Config.t
  -> unit
(** Load and execute the scenario in-browser. *)

val run_all_scenarios_browser :
     primitives:(module Primitives.Primitives)
  -> scenario:string
  -> Config.t
  -> unit
(** Load and execute the scenario in-browser. *)

(** {1 Lower-level Functions} *)

val run :
     primitives:(module Primitives.Primitives)
  -> html:bool
  -> install:string
  -> scenario:Scenario.loaded_scenario
  -> string option
  -> ([ `Warning | `Error ] * smart_except list) list
