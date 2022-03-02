(* Copyright 2019-2021 Smart Chain Arena LLC. *)

(** Debug printing functions. *)
module Dbg : sig
  val on : bool ref
  (** Off by default, use [Dbg.on := true] to activate. *)

  val p : (Format.formatter -> unit -> unit) -> unit

  val f : ('a, Format.formatter, unit, unit) format4 -> 'a

  val summary : ?len:int -> string -> string
end

module type JsonGetter = sig
  val null : bool

  val get : string -> Yojson.Basic.t

  val string : string -> string

  val string_option : string -> string option

  val string_list : string -> string list

  val int : string -> int

  val bool : string -> bool
end

val json_getter : Yojson.Basic.t -> (module JsonGetter)

val json_sub : (module JsonGetter) -> string -> (module JsonGetter)

(** Shared Utilities on hexadecimal encodings. *)
module Hex : sig
  val hexcape : string -> string

  val unhex : string -> string
end

val ( <|> ) : 'a option -> (unit -> 'a option) -> 'a option

val memoize : ?clear_after:int -> (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b

val pp_with_max_indent :
  Format.formatter -> int -> (unit -> unit) -> unit -> unit

val pp_with_margin : Format.formatter -> int -> (unit -> unit) -> unit -> unit

type json =
  | J_int    of Bigint.t
  | J_bool   of bool
  | J_string of string
  | J_list   of json list
  | J_record of (string * json) list
[@@deriving eq, ord, show]

val json_to_json : json -> Yojson.Basic.t

val pp_json_as_json :
  ?margin:int -> ?max_indent:int -> unit -> Format.formatter -> json -> unit

val tic : unit -> string -> unit

val tictoc : string -> ('a -> 'b) -> 'a -> 'b

val with_buffer : (Buffer.t -> unit) -> string

val buffer_protect :
  Buffer.t -> bool -> string -> string -> (unit -> unit) -> unit

val buffer_concat : Buffer.t -> string -> ('a -> unit) -> 'a list -> unit

val tick : string -> unit
