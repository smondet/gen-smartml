(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML

val intType : bool Unknown.t -> [> `Int | `Nat | `Unknown ]

val mtype_of_type : with_annots:bool -> Type.t -> Michelson.mtype

val mtype_of_type_with_single :
  with_annots:bool -> Type.t -> string option * Michelson.mtype
