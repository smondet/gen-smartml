(* Copyright 2019-2021 Smart Chain Arena LLC. *)

module type S = sig
  val run : ?env:(string * string) list -> string -> string list -> int
end
