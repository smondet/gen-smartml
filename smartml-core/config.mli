(* Copyright 2019-2021 Smart Chain Arena LLC. *)

type language =
  | SmartPy
  | SmartML
  | SmartTS
[@@deriving eq, ord, show {with_path = false}]

type protocol =
  (* Order is IMPORTANT *)
  | Delphi  (** Proto 7 *)
  | Edo  (** Proto 8 *)
  | Florence  (** Proto 9 *)
  | Granada  (** Proto 10 *)
  | Hangzhou  (** Proto 11 *)
  | Ithaca  (** Proto 12 *)
[@@deriving eq, ord, show]

type exceptions =
  | FullDebug
  | Message
  | VerifyOrLine
  | DefaultLine
  | Line
  | DefaultUnit
  | Unit
[@@deriving eq, ord, show]

type default_layout =
  | Tree
  | Comb
[@@deriving eq, ord, show {with_path = false}]

type t =
  { simplify : bool
  ; simplify_via_michel : bool
  ; decompile : bool
  ; erase_comments : bool
  ; disable_dup_check : bool
  ; contract_check_exception : bool
  ; view_check_exception : bool
  ; protocol : protocol
  ; lazy_entry_points : bool
  ; exceptions : exceptions
  ; default_variant_layout : default_layout
  ; default_record_layout : default_layout
  ; initial_cast : bool
  ; erase_var_annots : bool
  ; pairn : bool
  ; dump_michel : bool
  ; single_entry_point_annotation : bool
  ; warn_unused : bool
  ; languages : language list }
[@@deriving eq, ord, show {with_path = false}]

val default : t

type bool_flag =
  | Contract_check_exception
  | Decompile
  | Disable_dup_check
  | Dump_michel
  | Erase_comments
  | Erase_var_annots
  | Initial_cast
  | Pairn
  | Simplify
  | Simplify_via_michel
  | Single_entry_point_annotation
  | Warn_unused
  | Lazy_entry_points
  | View_check_exception
[@@deriving eq, ord, show]

type flag =
  | Bool_Flag              of bool_flag * bool
  | Default_record_layout  of default_layout
  | Default_variant_layout of default_layout
  | Exceptions             of exceptions
  | Protocol               of protocol
  | Languages              of language list
[@@deriving eq, ord, show]

val parse_flag : string list -> flag option

val apply_flag : t -> flag -> t

val protocol_of_string : string -> protocol

val is_initial_flag : flag -> bool

val string_of_bool_flag : bool_flag -> string

val string_of_flag : flag -> string list
