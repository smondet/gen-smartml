(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics

val run : config:Config.t -> (line_no * typing_constraint) list -> unit
