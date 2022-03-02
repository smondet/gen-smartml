(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Control

include module type of Michelson_base.Type

include module type of Michelson_base.Primitive

val cata_mtype_stripped : ('a mtype_f -> 'a) -> mtype -> 'a

val remove_annots : mtype -> mtype

val two_field_annots : string option * string option -> string list

val mt_bls12_381_g1 : mtype

val mt_bls12_381_g2 : mtype

val mt_bls12_381_fr : mtype

type ad_step =
  | A
  | D
[@@deriving eq, show]

type tezos_int = Bigint.t [@@deriving eq, ord, show {with_path = false}]

type stack =
  | Stack_ok     of mtype list
  | Stack_failed
[@@deriving eq, ord, show]

type ('i, 'literal) instr_f =
  | MI0                 of mtype prim0
  | MI1                 of mtype prim1
  | MI1_fail            of prim1_fail
  | MI2                 of mtype prim2
  | MI3                 of prim3
  | MIerror             of string
  | MIcomment           of string list
  | MImich              of mtype Basics.inline_michelson
  | MIdip               of 'i
  | MIdipn              of int * 'i
  | MIloop              of 'i
  | MIloop_left         of 'i
  | MIiter              of 'i
  | MImap               of 'i
  | MIdrop
  | MIdropn             of int
  | MIdup               of int
  | MIdig               of int
  | MIdug               of int
  | MIif                of 'i * 'i
  | MIif_left           of 'i * 'i
  | MIif_none           of 'i * 'i
  | MIif_cons           of 'i * 'i
  | MIpush              of mtype * 'literal
  | MIseq               of 'i list
  | MIswap
  | MIunpair            of bool list
  | MIpairn             of int
  | MIfield             of ad_step list
  | MIsetField          of ad_step list
  | MIlambda            of mtype * mtype * 'i
  | MIcreate_contract   of
      { tparameter : mtype * string option
      ; tstorage : mtype
      ; code : 'i }
  | MIconcat_unresolved
[@@deriving eq, ord, show, map, fold]

type ('instr, 'literal) literal_f =
  | Int      of tezos_int
  | Bool     of bool
  | String   of string
  | Bytes    of string
  | Unit
  | Pair     of 'literal * 'literal
  | None_
  | Left     of 'literal
  | Right    of 'literal
  | Some_    of 'literal
  | Seq      of 'literal list
  | Elt      of ('literal * 'literal)
  | Instr    of 'instr
  | AnyMap   of ('literal * 'literal) list (* A (possibly empty) map or big map, recognized by 'sanitize'. *)
  | Constant of string
[@@deriving eq, ord, show, map, fold]

type instr = {instr : (instr, literal) instr_f}

and literal = {literal : (instr, literal) literal_f}
[@@deriving eq, ord, show, map, fold]

type tinstr = private
  { tinstr : (tinstr, tliteral) instr_f
  ; stack_in : stack Result.t
  ; stack_out : stack Result.t }

and tliteral = private
  { tliteral : (tinstr, tliteral) literal_f
  ; t : mtype Result.t }
[@@deriving eq, ord, show]

val count_bigmaps : tliteral -> int

val size_instr : instr -> int

module MLiteral : sig
  val int : tezos_int -> literal

  val small_int : int -> literal

  val bool : bool -> literal

  val string : string -> literal

  val bytes : string -> literal

  val unit : literal

  val left : literal -> literal

  val right : literal -> literal

  val some : literal -> literal

  val pair : literal -> literal -> literal

  val none : literal

  val list : literal list -> literal

  val set : literal list -> literal

  val mk_map : (literal * literal) list -> literal

  val sapling_empty_state : literal

  val instr : instr -> literal

  val to_michelson_string : (instr -> string) -> literal -> string

  val elt : literal -> literal -> literal

  val seq : literal list -> literal

  val constant : string -> literal
end

val unifiable_types : mtype -> mtype -> bool

val unify_stacks : stack -> stack -> stack option

val initial_stack : tparameter:mtype -> tstorage:mtype -> stack

val typecheck_instr :
     strict_dup:bool
  -> tparameter:mtype * string option
  -> stack
  -> instr
  -> tinstr

val erase_types_instr : tinstr -> instr

val erase_types_literal : tliteral -> literal

val strip_annots : mtype -> mtype

val strip_annot_variable : mtype -> mtype

(* String conversion *)
val string_of_mtype :
     ?full:unit
  -> ?human:unit
  -> ?protect:unit
  -> ?annot:string
  -> html:bool
  -> mtype
  -> string

val string_of_ok_stack : ?full:unit -> mtype list -> string

val string_of_stack : ?full:unit -> stack -> string

val string_of_ad_path : ad_step list -> string

val string_of_literal : literal -> string

val string_of_tliteral : tliteral -> string

module Of_micheline : sig
  val mtype : Micheline.t -> mtype

  val mtype_annotated : Micheline.t -> mtype * string option

  val instruction : Micheline.t -> instr

  val literal : Micheline.t -> literal
end

module To_micheline : sig
  val literal : literal -> Micheline.t

  val mtype : ?annot_field:string -> mtype -> Micheline.t

  val instruction : instr -> Micheline.t list
end

type 'instr view =
  { name : string
  ; pure : bool
  ; doc : string
  ; tparameter : mtype option
  ; treturn : mtype
  ; onchain_code : 'instr option
  ; offchain_code : 'instr }
[@@deriving show, map]

type contract =
  { tparameter : mtype * string option
  ; tstorage : mtype
  ; code : instr
  ; lazy_entry_points : literal option
  ; storage : literal option
  ; views : instr view list }
[@@deriving show]

(** A complete Michelson contract (equivalent to the contents of a
   [".tz"] file). *)
type tcontract = private
  { tparameter : mtype * string option
  ; tstorage : mtype
  ; code : tinstr
  ; lazy_entry_points : tliteral option
  ; storage : tliteral option
  ; views : tinstr view list }
[@@deriving show]

val typecheck_contract : strict_dup:bool -> contract -> tcontract

val erase_types_contract : tcontract -> contract

val to_micheline_tcontract : tcontract -> Micheline.t

val display_tcontract : tcontract -> string

val render_tcontract : tcontract -> string

val render_tcontract_no_types : tcontract -> string

val has_error : accept_missings:bool -> tinstr -> string list

val has_error_tcontract : accept_missings:bool -> tcontract -> string list

val unexpected_final_stack_error : string

type ('i, 'l) alg =
  { f_instr : ('i, 'l) instr_f -> 'i
  ; f_literal : ('i, 'l) literal_f -> 'l }

type ('i, 'l) talg =
  { f_tinstr :
         stack_in:stack Result.t
      -> stack_out:stack Result.t
      -> ('i, 'l) instr_f
      -> 'i
  ; f_tliteral : t:mtype Result.t -> ('i, 'l) literal_f -> 'l }

val cata_instr : ('i, 'l) alg -> instr -> 'i

val cata_tinstr : ('a, 'b) talg -> tinstr -> 'a

val has_arity : int * int -> (instr, literal) instr_f -> bool

val mtype_examples : mtype -> literal list

val unpair_size : bool list -> int

val is_hot : mtype -> Ternary.t

val is_duppable : mtype -> bool

val unifiable_ok_stacks : mtype list -> mtype list -> bool

val para_alg :
     p_instr:((instr * 'a, literal * 'b) instr_f -> 'a)
  -> p_literal:((instr * 'a, literal * 'b) literal_f -> 'b)
  -> (instr * 'a, literal * 'b) alg

val on_instrs : (instr -> instr) -> literal -> literal

type instr_spec =
  { name : string
  ; rule :
         tparameter:mtype * string option
      -> mtype list
      -> (stack Result.t -> tinstr, mtype -> tliteral) instr_f
      -> (tinstr, tliteral) instr_f * stack Result.t
  ; commutative : bool
  ; arities : (int * int) option }

val spec_of_prim1 : mtype Michelson_base.Primitive.prim1 -> instr_spec

val spec_of_instr : strict_dup:bool -> _ instr_f -> instr_spec

val arity : instr -> (int * int option) Result.t

val is_commutative : _ instr_f -> bool

val typecheck_literal :
     strict_dup:bool
  -> tparameter:mtype * string option
  -> mtype
  -> literal
  -> tliteral

val cata_literal : ('i, 'l) alg -> literal -> 'l

val tcata : ('a, 'b) talg -> (tinstr -> 'a) * (tliteral -> 'b)

val cata_tliteral : ('a, 'b) talg -> tliteral -> 'b

val render_tinstr : show_stack:bool -> int -> tinstr -> string

val display_tinstr : show_stack:bool -> new_line:bool -> int -> tinstr -> string

val sequence_literal_f :
  ('i Result.t, 'l Result.t) literal_f -> ('i, 'l) literal_f Result.t

val sequence_instr_f :
  ('i Result.t, 'l Result.t) instr_f -> ('i, 'l) instr_f Result.t

val display_instr : mtype -> instr -> string
