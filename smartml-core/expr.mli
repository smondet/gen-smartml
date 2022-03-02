(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Basics
open Untyped

(**

Expressions represent computations that are performed in a smart contract.

Expressions are built by calling the functions defined in this module
 or by using the SmartML PPX syntax.

{2 Direct Expressions}
Expressions can be built directly by using the functions in this module.
This is a complex and tedious process that should only be used by the
more adventurous developers.

{2 PPX Syntax}
This is the intended syntax for most circumstances.
This is both very convenient and powerful.

{3 Examples}

{[
    self.data.storedValue * 2

    fst (1, 2)
    snd (1, 2)
    left 2
    right 2
    sign 2
    sign 0
    sign (-2)
    to_int 2

    (* Arithmetic *)
    1
    2 <> 3
    2 * 3
    2 < 3
    3 > 2
    2 <= 2
    3 >= 3
    299 / 100
    299 % 100
    ediv 299 100
    2 + 3
    2 - 3
    sum [1; 2; 3; 4; 5]
    let two = local 2 in -two
    abs 2
    abs (-2)
    is_nat 2
    is_nat (-1)
    min 2 3
    max 2 3
    5 lsl 2
    23 lsr 2

    (* Booleans *)
    not b
    b1 || b2
    b1 && b2
    xor b1 b2
]}

{2 Types}
Expressions in this module are untyped, which means that they do not carry
 any type information. They are typed, by {!SmartML}, in a later stage, typically in a {!Basics.tscenario}.

 *)

type t = expr [@@deriving show]

type nullary_expr = line_no:line_no -> t

type unary_expr = line_no:line_no -> t -> t

type binary_expr = line_no:line_no -> t -> t -> t

type ternary_expr = line_no:line_no -> t -> t -> t -> t

(** {2 SmartML Specific Nullary Expressions} *)

val none : line_no:line_no -> t
(** Optional None. In a SmartML PPX context, [None].
 *)

val params : line_no:line_no -> t
(** The current entry point parameters. In a SmartML PPX context, [params].
 *)

val self : nullary_expr
(** The current contract.

Useful to access, e.g., storage by [self.data]. In a SmartML PPX context, [self].
 *)

val storage : line_no:line_no -> t
(** The current storage. In a SmartML PPX context, [data].
 *)

val operations : line_no:line_no -> t
(** The current operations accumulator. In a SmartML PPX context, [operations].
 *)

(** {2 Michelson Nullary Expressions} *)

val amount : nullary_expr
(** See {!Michelson_base.Primitive.Amount}. In a SmartML PPX context, [amount].
 *)

val balance : nullary_expr
(** See {!Michelson_base.Primitive.Balance}. In a SmartML PPX context, [balance].
 *)

val chain_id : nullary_expr
(** See {!Michelson_base.Primitive.Chain_id}. In a SmartML PPX context, [chain_id].
 *)

val level : nullary_expr
(** See {!Michelson_base.Primitive.Level}. In a SmartML PPX context, [level].
 *)

val now : nullary_expr
(** See {!Michelson_base.Primitive.Now}. In a SmartML PPX context, [now].
 *)

val self_address : nullary_expr
(** See {!Michelson_base.Primitive.Self_address}. In a SmartML PPX context, [self_address].
 *)

val sender : nullary_expr
(** See {!Michelson_base.Primitive.Sender}. In a SmartML PPX context, [sender].
 *)

val source : nullary_expr
(** See {!Michelson_base.Primitive.Source}. In a SmartML PPX context, [source].
 *)

val total_voting_power : nullary_expr
(** See {!Michelson_base.Primitive.Total_voting_power}. In a SmartML PPX context, [total_voting_power].
 *)

(** {2 SmartPy Specific Unary Expressions} *)

val absE : unary_expr
(** Absolute value. In a SmartML PPX context, [abs].
 *)

val attr : name:string -> unary_expr
(** Attribute of an expression. In a SmartML PPX context, [_expr_._attr_].
 *)

val signE : unary_expr
(** Sign of expression. In a SmartML PPX context, [sign].
 *)

val sum : unary_expr
(** Sum of expression list. In a SmartML PPX context, [sum].
 *)

(** {2 Michelson Unary Expressions} *)

val implicit_account : unary_expr
(** See {!Michelson_base.Primitive.Implicit_account}. In a SmartML PPX context, [implicit_account].
 *)

val is_nat : unary_expr
(** See {!Michelson_base.Primitive.IsNat}. In a SmartML PPX context, [is_nat].
 *)

val left : unary_expr
(** See {!Michelson_base.Primitive.Left}. In a SmartML PPX context, [left].
 *)

val negE : unary_expr
(** See {!Michelson_base.Primitive.Right}. In a SmartML PPX context, [right].
 *)

val notE : unary_expr
(** See {!Michelson_base.Primitive.Not}. In a SmartML PPX context, [not].
 *)

val right : unary_expr
(** See {!Michelson_base.Primitive.Right}. In a SmartML PPX context, [right].
 *)

val some : unary_expr
(** See {!Michelson_base.Primitive.Some_}. In a SmartML PPX context, [Some].
 *)

val to_int : unary_expr
(** See {!Michelson_base.Primitive.Int}. In a SmartML PPX context, [to_int].
 *)

val voting_power : unary_expr
(** See {!Michelson_base.Primitive.Voting_power}. In a SmartML PPX context, [voting_power].
 *)

val variant : name:string -> unary_expr

val isVariant : name:string -> unary_expr

val unbounded : unary_expr

val is_failing : unary_expr

val catch_exception : t:Type.t -> unary_expr

val type_annotation : t:Type.t -> unary_expr

val hash_key : unary_expr

val blake2b : unary_expr

val sha256 : unary_expr

val sha512 : unary_expr

val keccak : unary_expr

val sha3 : unary_expr

val pack : unary_expr

val resolve : unary_expr

val constant : line_no:line_no -> string -> Type.t -> t

val constant_scenario_var : line_no:line_no -> string -> t

val concat_list : unary_expr

val size : unary_expr

val listRev : unary_expr

val first : unary_expr

val second : unary_expr

val to_address : unary_expr

val read_ticket : unary_expr

val join_tickets : unary_expr

val pairing_check : unary_expr

(** {2 SmartPy Specific Binary Expressions} *)

val bin_op : op:Basics.binOp -> binary_expr
(** Binary operators. See {!Basics.binOp} for description and
    SmartML PPX syntax. *)

val add_seconds : binary_expr

val map_function : binary_expr

val call_lambda : binary_expr

val apply_lambda : binary_expr

val lsl_ : binary_expr

val lsr_ : binary_expr

val cons : binary_expr

val contains : binary_expr

val sapling_verify_update : binary_expr

val build_ticket : binary_expr

val split_ticket : binary_expr

val getOpt : binary_expr

(** {2 SmartPy Specific Ternary Expressions} *)

val updateMap : ternary_expr

val range : ternary_expr

val check_signature : ternary_expr

val eif : ternary_expr

val test_ticket : ternary_expr

val get_and_update : ternary_expr

val split_tokens : ternary_expr

val open_chest : ternary_expr

val slice : line_no:line_no -> offset:t -> length:t -> buffer:t -> t

(** {2 Misc} *)

val openVariant : line_no:line_no -> string -> t -> t option -> t

val item : line_no:line_no -> t -> t -> t option -> t option -> t

val cst : line_no:line_no -> Literal.t -> t

val bounded : line_no:line_no -> Literal.t -> t

val has_entry_point : string -> line_no:line_no -> t

val record : line_no:line_no -> (string * t) list -> t

val build_list : line_no:line_no -> elems:t list -> t

val build_map : line_no:line_no -> big:bool -> entries:(t * t) list -> t

val build_set : line_no:line_no -> entries:t list -> t

val unpack : line_no:line_no -> t -> Type.t -> t

val account_of_seed : seed:string -> line_no:line_no -> t

val make_signature :
     line_no:line_no
  -> secret_key:t
  -> message:t
  -> message_format:[ `Hex | `Raw ]
  -> t

val scenario_var : line_no:line_no -> string -> t

val variable : line_no:line_no -> string -> t

val match_cons : line_no:line_no -> string -> t

val self_entry_point : line_no:line_no -> string -> t

val listItems : line_no:line_no -> t -> bool -> t

val listKeys : line_no:line_no -> t -> bool -> t

val listValues : line_no:line_no -> t -> bool -> t

val listElements : line_no:line_no -> t -> bool -> t

val contract : line_no:line_no -> string option -> Type.t -> t -> t

val view : line_no:line_no -> string -> t -> t -> Type.t -> t

val static_view : line_no:line_no -> static_id -> string -> t -> t

val tuple : line_no:line_no -> t list -> t

val proj : line_no:line_no -> int -> t -> t

val inline_michelson : line_no:line_no -> Type.t inline_michelson -> t list -> t

val lambda :
     line_no:line_no
  -> string
  -> command
  -> clean_stack:bool
  -> with_storage:Type.with_storage option
  -> with_operations:bool
  -> t

val create_contract :
  line_no:line_no -> baker:t -> balance:t -> storage:t -> contract -> t

val lambdaParams : line_no:line_no -> string -> t

val transfer :
  line_no:line_no -> arg:expr -> amount:expr -> destination:expr -> t

val set_delegate : line_no:line_no -> expr -> t

val contract_data : line_no:line_no -> contract_id -> t

val ematch : line_no:line_no -> t -> (string * t) list -> t

val contract_address : line_no:line_no -> string option -> contract_id -> t

val contract_typed : line_no:line_no -> string option -> contract_id -> t

val contract_balance : line_no:line_no -> contract_id -> t

val contract_baker : line_no:line_no -> contract_id -> t

val allow_lambda_full_stack : t -> t

val sapling_empty_state : int -> t

val of_value : value -> t

val local : line_no:line_no -> string -> t

val meta_local : line_no:line_no -> string -> t

val private_ : line_no:line_no -> string -> t

val convert : unary_expr
