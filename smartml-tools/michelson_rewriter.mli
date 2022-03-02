(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Michelson

type group

type pipeline = group list

val run : pipeline -> instr -> instr

val run_on_tcontract : pipeline -> Michelson.tcontract -> Michelson.tcontract

val collapse_drops : pipeline

val simplify : config:Config.t -> pipeline

val pushify : pipeline

val remove_comments : pipeline
