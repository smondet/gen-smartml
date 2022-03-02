(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Basics

val clear : unit -> unit

val register :
     ?flags:Config.flag list
  -> ?is_default:bool
  -> name:string
  -> kind:string
  -> action list Lazy.t
  -> unit

val register_test :
  ?is_default:bool -> name:string -> action list Lazy.t -> unit

val register_compilation :
  ?flags:Config.flag list -> ?is_default:bool -> name:string -> contract -> unit

val to_json : string option -> string

val names_kinds_json : unit -> string

val dump : string -> unit
