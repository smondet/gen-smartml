(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils_pure
open Control

type 't row = (string * 't) list [@@deriving eq, show, ord, map, fold]

val sort_row : 't row -> 't row

type 't bounded_cases =
  { final : bool
  ; cases : 't list }
[@@deriving eq, map, fold, show {with_path = false}, ord]

type with_storage =
  | Read_only
  | Read_write
[@@deriving eq, ord, show]

type effects =
  { with_storage : with_storage option Unknown.t
  ; with_operations : bool Unknown.t }
[@@deriving eq, ord, show {with_path = false}]

val no_effects : effects

val unknown_effects : unit -> effects

type 't f = private
  | TVar                of string
  | T0                  of Michelson_base.Type.type0
  | T1                  of Michelson_base.Type.type1 * 't
  | T2                  of Michelson_base.Type.type2 * 't * 't
  | TInt                of {isNat : bool Unknown.t}
  | TLambda             of effects * 't * 't
  | TBounded            of
      { t : 't
      ; cases : Literal.t bounded_cases Unknown.t }
  | TRecord             of
      { layout : Layout.t Unknown.t
      ; row : 't row }
  | TVariant            of
      { layout : Layout.t Unknown.t
      ; row : 't row }
  | TUnknown            of 't unknownType_f ref
  | TTuple              of 't list
  | TSecretKey
  | TSaplingState       of {memo : int Unknown.t}
  | TSaplingTransaction of {memo : int Unknown.t}

and 't unknownType_f =
  | UUnknown of string
  | URecord  of (string * 't) list
  | UTuple   of (int * 't) list
  | UVariant of (string * 't) list
  | UExact   of 't
[@@deriving eq, show, ord, map, fold]

include UNTIED with type 'a f := 'a f

include EQ with type t := t

include ORD with type t := t

include SHOW with type t := t

type unknownType = t unknownType_f

type tvariable = string * t [@@deriving eq, show]

val tree_layout : string list -> Layout.t

val default_layout_of_row : Config.default_layout -> string list -> Layout.t

val comb_layout : [ `Left | `Right ] -> string list -> Layout.t

val comb_layout_of_row : [ `Left | `Right ] -> string list -> Layout.t

val getRepr : t -> t f

val normalize : t -> t

val mk0 : Michelson_base.Type.type0 -> t

val mk1 : Michelson_base.Type.type1 -> t -> t

val mk2 : Michelson_base.Type.type2 -> t -> t -> t

val unit : t

val address : t

val contract : t -> t

val bool : t

val bytes : t

val key_hash : t

val baker_type_of_protocol : Config.protocol -> t

val int_raw : isNat:bool Unknown.t -> t

val int : unit -> t

val nat : unit -> t

val intOrNat : unit -> t

val key : t

val key_value : t -> t -> t

val head_tail : t -> t -> t

val chain_id : t

val secret_key : t

val operation : t

val sapling_state : int option -> t

val sapling_transaction : int option -> t

val never : t

val map : big:bool -> tkey:t -> tvalue:t -> t

val set : telement:t -> t

val record : Layout.t Unknown.t -> (string * t) list -> t

val variant : Layout.t Unknown.t -> (string * t) list -> t

val record_default_layout : Config.default_layout -> (string * t) list -> t

val variant_default_layout : Config.default_layout -> (string * t) list -> t

val record_or_unit : Layout.t Unknown.t -> (string * t) list -> t

val signature : t

val option : t -> t

val tor : t -> t -> t

val string : t

val bounded : t -> bool -> Literal.t list -> t

val timestamp : t

val token : t

val uvariant : string -> t -> t

val urecord : (string * t) list -> t

val utuple : int -> t -> t

val unknown_raw : unknownType ref -> t

val account : t

val pair : t -> t -> t

val tuple : t list -> t

val list : t -> t

val ticket : t -> t

val lambda : effects -> t -> t -> t

val has_unknowns : t -> bool

val bls12_381_g1 : t

val bls12_381_g2 : t

val bls12_381_fr : t

val chest_key : t

val chest : t

val is_hot : t -> Ternary.t

val view_variant : t -> (Layout.t Unknown.t * t row) option

val of_mtype : Michelson_base.Type.mtype -> t

val type_of_literal : Literal.t -> t

val rawify_lambda :
  with_storage:t option -> with_operations:bool -> t -> t -> t * t

val has_effects : effects -> bool
