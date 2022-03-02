(* Copyright 2019-2021 Smart Chain Arena LLC. *)

val getText : string -> string

val getTextRef : (string -> string) ref

val isChecked : string -> bool

val isCheckedRef : (string -> bool) ref

val parseDate : string -> string

val parseDateRef : (string -> string) ref

val setText : string -> string -> unit

val setTextRef : (string -> string -> unit) ref

val setValue : string -> string -> unit

val setValueRef : (string -> string -> unit) ref

val setOutput : string -> unit

val addOutput : string -> unit

val setOutputToMethod : string -> string -> unit

val setOutputRef : (string -> unit) ref

val addOutputRef : (string -> unit) ref

val setOutputToMethodRef : (string -> string -> unit) ref
