(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics
open Typed

type t =
  | HO_none
  | HO_at_most_one
  | HO_many
[@@deriving show, eq, ord]

val or_ : t -> t -> t

val add : t -> t -> t

val widen : t -> t

val has_operations : tcommand -> t
