(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML

(** {1 Automatic User Interface Generation from Types} *)

type inputGuiResult =
  { gui : string
  ; get : bool -> Value.t }

val inputGuiR :
  ?path:string list -> nextId:(unit -> string) -> Type.t -> inputGuiResult
