(* Copyright 2019-2021 Smart Chain Arena LLC. *)
open Control

include module type of Stdlib.Option with type 'a t := 'a option

type 'a t = 'a option [@@deriving eq, show]

val cata : 'a -> ('b -> 'a) -> 'b t -> 'a

val some : 'a -> 'a t

include MONAD with type 'a t := 'a t

val default : 'a -> 'a t -> 'a

val is_none : 'a option -> bool

val is_some : 'a option -> bool

val of_some : ?msg:string -> 'a option -> 'a

val of_some_exn : ?msg:string -> 'a t -> 'a

val get : ?msg:string -> 'a t -> 'a
