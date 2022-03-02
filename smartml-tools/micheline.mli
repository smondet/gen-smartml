(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics

type t = micheline

val unAnnot : string list -> string

val pretty : string -> t -> string

val to_json : t -> Utils.Misc.json

val pp_as_json :
  ?margin:int -> ?max_indent:int -> unit -> Format.formatter -> t -> unit

val left : t -> t

val right : t -> t

val annotName : string -> string

val extractAnnot : string -> string list -> string

val identity : t -> 'a

val unString : [> `String of 'a ] -> 'a

val parse : Yojson.Basic.t -> t

val int : string -> t

val string : string -> t

val bytes : string -> t

(* val chain_id : string -> t *)

val primitive : string -> ?annotations:string list -> t list -> t

val sequence : t list -> t
