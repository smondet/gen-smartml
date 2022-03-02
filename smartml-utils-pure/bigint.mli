(* Copyright 2019-2021 Smart Chain Arena LLC. *)
type t = Big_int.big_int [@@deriving eq, ord, show]

val of_int : int -> t

include module type of Big_int with type big_int := t
