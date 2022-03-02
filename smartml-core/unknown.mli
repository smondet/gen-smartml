(* Copyright 2019-2021 Smart Chain Arena LLC. *)

type 'a t [@@deriving eq, show, ord]

val unknown : unit -> 'a t

val value : 'a -> 'a t

val get : 'a t -> 'a option

val get_ok : string -> 'a t -> 'a

val set : 'a t -> 'a -> bool

val default : 'a t -> 'a -> unit

val normalize : 'a t -> 'a t

val equalize : 'a t -> 'a t -> bool

val equalize_merge :
  ('a -> 'a -> bool) -> ('a -> 'a -> 'a option) -> 'a t -> 'a t -> bool
