(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils_pure

type l =
  { source : string
  ; target : string }
[@@deriving eq, ord, show]

type t = l Binary_tree.t [@@deriving eq, ord, show]

val leaf : string -> string -> t
