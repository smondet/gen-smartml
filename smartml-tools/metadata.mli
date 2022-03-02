(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics

val for_contract :
     config:Config.t
  -> instance
  -> Michelson.tcontract
  -> (string * Utils.Misc.json) list
