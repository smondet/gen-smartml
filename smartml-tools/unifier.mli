(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics

type 'a unification_result =
  | Ok     of 'a
  | Failed of smart_except list

val assertEqual : config:Config.t -> Type.t -> Type.t -> unit unification_result
