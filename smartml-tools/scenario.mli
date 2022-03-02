(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics

type loaded_scenario =
  { scenario : tscenario
  ; scenario_state : Basics.scenario_state
  ; warnings : smart_except list list
  ; language : Config.language }

val load_from_string :
     primitives:(module Primitives.Primitives)
  -> Config.t
  -> Yojson.Basic.t
  -> loaded_scenario

val check_close_scenario : Config.t -> scenario -> loaded_scenario
