(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils_pure
open Control

include module type of Ids

module Literal = Literal

type typed = | [@@deriving eq, ord, show]

type untyped = | [@@deriving eq, ord, show]

val elim_typed : typed -> 'a

val elim_untyped : untyped -> 'a

type ('a, 's) extra =
  | U : ('a, untyped) extra
  | T : 'a -> ('a, typed) extra
[@@deriving eq, ord, show, fold]

val get_extra : ('a, typed) extra -> 'a

val map_extra :
     ('a -> 'b)
  -> ('switch -> 'switch)
  -> ('a, 'switch) extra
  -> ('b, 'switch) extra

type vClass =
  | Simple
  | Local
  | Scenario

type 'v record_f = (string * 'v) list [@@deriving eq, ord, show]

type binOp =
  | BAdd  (** Addition. PPX: [x + y]  *)
  | BAnd  (** And. PPX: [x && y]*)
  | BDiv  (** Division. PPX: [x / y]  *)
  | BEDiv  (** Euclidean Division. PPX: [x / y]  *)
  | BEq  (** Equal. PPX: [x = y]*)
  | BGe  (** Greater of Equal. PPX: [x >= y]  *)
  | BGt  (** Greater Than. PPX: [x > y]  *)
  | BLe  (** Lower or Equal. PPX: [x <= y]  *)
  | BLt  (** Lower Than. PPX: [x < y]  *)
  | BMax  (** Max. PPX: [max x y]  *)
  | BMin  (** Min. PPX: [min x y]  *)
  | BMod  (** Modulo. PPX: [x % y]  *)
  | BMul  of {overloaded : bool}
      (** Multiplication. PPX: [x * y] for non-overloaded version ([x] and
       [y] of the same type) and [mul x y] for overloaded one. *)
  | BNeq  (** Non-equal. PPX: [x <> y]*)
  | BOr  (** Or. PPX: [x || y] *)
  | BSub  (** Subtraction. PPX: [x - y]  *)
  | BXor  (** Xor. PPX: [xor x y]  *)
[@@deriving show, eq]

type micheline =
  | Int       of string
  | String    of string
  | Bytes     of string
  | Primitive of
      { name : string
      ; annotations : string list
      ; arguments : micheline list }
  | Sequence  of micheline list
[@@deriving eq, show, ord]

type 't inline_michelson =
  { name : string
  ; parsed : micheline
  ; typesIn : 't list
  ; typesOut : 't list }
[@@deriving eq, show, ord]

type line_no = (string * int) list [@@deriving eq, ord, show]

type ('command, 'type_, 'switch) entry_point =
  { channel : string
  ; tparameter_ep : [ `Absent | `Present | `Annotated of Type.t ]
  ; originate : bool
  ; lazify : bool option
  ; lazy_no_code : bool option
  ; line_no : line_no
  ; body : 'command
  ; tparameter_ep_derived : ('type_, 'switch) extra }
[@@deriving show, fold]

type view_kind =
  | Onchain
  | Offchain
[@@deriving eq, ord, show {with_path = false}, map, fold]

type ('command, 'type_, 'switch) view =
  { kind : view_kind
  ; name : string
  ; has_param : bool
  ; pure : bool
  ; body : 'command
  ; doc : string
  ; tparameter_derived : ('type_ option, 'switch) extra }
[@@deriving show {with_path = false}, fold]

type address =
  | Real  of string
  | Local of contract_id
[@@deriving eq, ord, show]

type 'type_ contract_derived =
  { tparameter : 'type_
  ; tparameter_lazy : 'type_ option
  ; tparameter_non_lazy : 'type_ option
  ; tparameter_full : 'type_
  ; tstorage : 'type_
  ; config : Config.t }
[@@deriving eq, ord, show, map, fold]

module Meta : sig
  type 'a t =
    | List  of 'a t list
    | Map   of ('a * 'a t) list
    | Other of 'a
    | View  of string
  [@@deriving eq, ord, show, map, fold]
end

type ('expr, 'command, 'type_, 'switch) contract_f =
  { template_id : static_id option
  ; balance : 'expr option
  ; storage : 'expr option
  ; baker : 'expr option
  ; tstorage_explicit : 'type_ option
  ; entry_points : ('command, 'type_, 'switch) entry_point list
  ; entry_points_layout : Layout.t option
  ; unknown_parts : string option
  ; flags : Config.flag list
  ; private_variables : (string * 'expr) list
  ; metadata : (string * 'expr Meta.t) list
  ; views : ('command, 'type_, 'switch) view list
  ; derived : ('type_ contract_derived, 'switch) extra }
[@@deriving eq, ord, show, fold]

val map_contract_f :
     ('expr1 -> 'expr2)
  -> ('command1 -> 'command2)
  -> ('type1 -> 'type2)
  -> ('switch -> 'switch)
  -> ('expr1, 'command1, 'type1, 'switch) contract_f
  -> ('expr2, 'command2, 'type2, 'switch) contract_f

type ('command, 'type_, 'switch) lambda_f =
  { name : string
  ; body : 'command
  ; clean_stack : bool
  ; with_storage : Type.with_storage option
  ; with_operations : bool
  ; tParams : ('type_, 'switch) extra
  ; tResult : ('type_, 'switch) extra }
[@@deriving show, fold]

type 'type_ lcontract =
  { address : string
  ; entry_point : string option
  ; type_ : 'type_ }
[@@deriving eq, ord, show, map, fold]

type 'type_ prim0 =
  | ECst              of Literal.t
  | ECstContract      of 'type_ lcontract
  | EBounded          of Literal.t
  | EMetaLocal        of string
  | EMatchCons        of string
  | EAccount_of_seed  of {seed : string}
  | EContract_address of contract_id * string option
  | EContract_balance of contract_id
  | EContract_baker   of contract_id
  | EContract_data    of contract_id
  | EContract_typed   of contract_id * string option
  | EConstant         of string * 'type_
  | EConstantVar      of string (* scenario var *)
[@@deriving eq, ord, show, map, fold]

type 'type_ prim1 =
  | EConcat_list
  | EAddress
  | EProject          of int
  | EImplicit_account
  | EListElements     of bool
  | EListItems        of bool
  | EListKeys         of bool
  | EListRev
  | EListValues       of bool
  | ENeg
  | EPack
  | EResolve
  | ESetDelegate
  | ESign
  | ESize
  | ESum
  | EToInt
  | EUnpack           of 'type_
  | EType_annotation  of 'type_
  | EAttr             of string
  | EVariant          of string
  | EIsVariant        of string
  | EReadTicket
  | EJoinTickets
  | EPairingCheck
  | EVotingPower
  | EUnbounded
  | EConvert
  | EStaticView       of static_id * string (* view name *)
[@@deriving eq, ord, show, map, fold]

type 'type_ prim2 =
  | EGetOpt
  | EBinOp       of binOp
  | EContains
  | ECallLambda
  | EApplyLambda
  | ECons
  | EAdd_seconds
  | ETicket
  | ESplitTicket
  | EView        of string (* view name *) * 'type_ (* return type *)
[@@deriving eq, ord, show, map, fold]

type 'type_ prim3 =
  | ESplit_tokens
  | ERange
  | EUpdate_map
  | EGet_and_update
  | ETest_ticket
[@@deriving eq, ord, show, map, fold]

type ('expr, 'command, 'type_, 'switch) expr_f =
  | EVar                 of string * vClass
  | EPrivate             of string
  | EMPrim0              of
      Michelson_base.Type.mtype Michelson_base.Primitive.prim0
  | EMPrim1              of
      Michelson_base.Type.mtype Michelson_base.Primitive.prim1 * 'expr
  | EMPrim1_fail         of Michelson_base.Primitive.prim1_fail * 'expr
  | EMPrim2              of
      Michelson_base.Type.mtype Michelson_base.Primitive.prim2 * 'expr * 'expr
  | EMPrim3              of
      Michelson_base.Primitive.prim3 * 'expr * 'expr * 'expr
  | EPrim0               of 'type_ prim0
  | EPrim1               of 'type_ prim1 * 'expr
  | EPrim2               of 'type_ prim2 * 'expr * 'expr
  | EPrim3               of 'type_ prim3 * 'expr * 'expr * 'expr
  | EOpenVariant         of string * 'expr * 'expr option
  | EItem                of
      { items : 'expr
      ; key : 'expr
      ; default_value : 'expr option
      ; missing_message : 'expr option }
  | ETuple               of 'expr list
  | ERecord              of (string * 'expr) list
  | EList                of 'expr list
  | EMap                 of bool * ('expr * 'expr) list
  | ESet                 of 'expr list
  | ESaplingVerifyUpdate of
      { state : 'expr
      ; transaction : 'expr }
  | EMichelson           of 'type_ inline_michelson * 'expr list
  | EMapFunction         of
      { f : 'expr
      ; l : 'expr }
  | ELambda              of ('command, 'type_, 'switch) lambda_f
  | ECreate_contract     of
      { contract_template : ('expr, 'command, 'type_, 'switch) contract_f
      ; baker : 'expr
      ; balance : 'expr
      ; storage : 'expr }
  | EContract            of
      { entry_point : string option
      ; arg_type : 'type_
      ; address : 'expr }
  | ESlice               of
      { offset : 'expr (* nat *)
      ; length : 'expr (* nat *)
      ; buffer : 'expr }
  | EMake_signature      of
      { secret_key : 'expr
      ; message : 'expr
      ; message_format : [ `Raw | `Hex ] }
  | ETransfer            of
      { arg : 'expr
      ; amount : 'expr
      ; destination : 'expr }
  | EMatch               of 'expr * (string * 'expr) list
  | EHasEntryPoint       of string
  | EIf                  of 'expr * 'expr * 'expr
  | EIsFailing           of 'expr
  | ECatchException      of 'type_ * 'expr
[@@deriving eq, ord, show, fold]

val map_expr_f :
     ('a -> 'b)
  -> ('c -> 'd)
  -> ('e -> 'f)
  -> ('switch -> 'switch)
  -> ('a, 'c, 'e, 'switch) expr_f
  -> ('b, 'd, 'f, 'switch) expr_f

type record_field_binding =
  { var : string
  ; field : string }
[@@deriving eq, ord, show]

type pattern =
  | Pattern_single of string
  | Pattern_tuple  of string list
  | Pattern_record of string * record_field_binding list
[@@deriving eq, ord, show]

type ('expr, 'command, 'type_) command_f =
  | CNever         of 'expr
  | CFailwith      of 'expr
  | CVerify        of 'expr * 'expr option (* message *)
  | CIf            of 'expr * 'command * 'command
  | CMatch         of 'expr * (string * string * 'command) list
  | CMatchProduct  of 'expr * pattern * 'command
  | CModifyProduct of 'expr * pattern * 'command
  | CMatchCons     of
      { expr : 'expr
      ; id : string
      ; ok_match : 'command
      ; ko_match : 'command }
  | CDefineLocal   of
      { var : string
      ; rhs : 'expr
      ; is_mutable : bool }
  | CSetVar        of 'expr * 'expr
  | CDelItem       of 'expr * 'expr
  | CUpdateSet     of 'expr * 'expr * bool
  | CBind          of string option * 'command * 'command
  | CFor           of string * 'expr * 'command
  | CWhile         of 'expr * 'command
  | CResult        of 'expr
  | CComment       of string
  | CSetType       of 'expr * 'type_
  | CSetResultType of 'command * 'type_
  | CTrace         of 'expr
  | CSetEntryPoint of string * 'expr
[@@deriving show, map, fold, eq, ord]

val string_of_line_no : line_no -> string

val head_line_no : line_no -> int

module Typed : sig
  type texpr = private
    { e : (texpr, tcommand, Type.t, typed) expr_f
    ; et : Type.t
    ; line_no : line_no }

  and tcommand = private
    { c : (texpr, tcommand, Type.t) command_f
    ; ct : Type.t
    ; line_no : line_no }
  [@@deriving eq, ord, show]

  val build_texpr :
       line_no:line_no
    -> (texpr, tcommand, Type.t, typed) expr_f
    -> Type.t
    -> texpr

  val build_tcommand :
    line_no:line_no -> (texpr, tcommand, Type.t) command_f -> Type.t -> tcommand
end

open Typed

module Untyped : sig
  type expr =
    { e : (expr, command, Type.t, untyped) expr_f
    ; line_no : line_no }

  and command =
    { c : (expr, command, Type.t) command_f
    ; line_no : line_no }
  [@@deriving eq, ord, show]
end

open Untyped

val equal_expr_modulo_line_nos : expr -> expr -> bool

val equal_command_modulo_line_nos : command -> command -> bool

type ('e, 'c, 't) tsyntax_alg =
  { f_texpr : line_no -> Type.t -> ('e, 'c, 't, typed) expr_f -> 'e
  ; f_tcommand : line_no -> Type.t -> ('e, 'c, 't) command_f -> 'c
  ; f_ttype : Type.t -> 't }

val cata_texpr : ('e, 'c, 't) tsyntax_alg -> texpr -> 'e

val cata_tcommand : ('e, 'c, 't) tsyntax_alg -> tcommand -> 'c

val monoid_talg : ('a -> 'a -> 'a) -> 'a -> ('a, 'a, 'a) tsyntax_alg

val para_texpr :
  (texpr * 'e, tcommand * 'c, Type.t * 't) tsyntax_alg -> texpr -> 'e

val para_tcommand :
  (texpr * 'e, tcommand * 'c, Type.t * 't) tsyntax_alg -> tcommand -> 'c

type ('e, 'c, 't) texpr_p =
  (texpr * 'e, tcommand * 'c, Type.t * 't, typed) expr_f

type ('e, 'c, 't) tcommand_p =
  (texpr * 'e, tcommand * 'c, Type.t * 't) command_f

val para_talg :
     p_texpr:(line_no -> Type.t -> ('e, 'c, 't) texpr_p -> 'e)
  -> p_tcommand:(line_no -> Type.t -> ('e, 'c, 't) tcommand_p -> 'c)
  -> p_ttype:(Type.t -> 't)
  -> (texpr * 'e, tcommand * 'c, Type.t * 't) tsyntax_alg

val monoid_para_talg :
     ('a -> 'a -> 'a)
  -> 'a
  -> (Typed.texpr * 'a, Typed.tcommand * 'a, Type.t * 'a) tsyntax_alg

type ('e, 'c, 't) syntax_alg =
  { f_expr : line_no -> ('e, 'c, 't, untyped) expr_f -> 'e
  ; f_command : line_no -> ('e, 'c, 't) command_f -> 'c
  ; f_type : Type.t -> 't }

val cata_expr : ('e, 'c, 't) syntax_alg -> expr -> 'e

val cata_command : ('e, 'c, 't) syntax_alg -> command -> 'c

val monoid_alg : ('a -> 'a -> 'a) -> 'a -> ('a, 'a, 'a) syntax_alg

type ('e, 'c, 't) expr_p =
  (expr * 'e, command * 'c, Type.t * 't, untyped) expr_f

type ('e, 'c, 't) command_p = (expr * 'e, command * 'c, Type.t * 't) command_f

val para_alg :
     p_expr:(line_no -> ('e, 'c, 't) expr_p -> 'e)
  -> p_command:(line_no -> ('e, 'c, 't) command_p -> 'c)
  -> p_type:(Type.t -> 't)
  -> (expr * 'e, command * 'c, Type.t * 't) syntax_alg

val para_expr : (expr * 'e, command * 'c, Type.t * 't) syntax_alg -> expr -> 'e

val para_command :
  (expr * 'e, command * 'c, Type.t * 't) syntax_alg -> command -> 'c

type lambda = (command, Type.t, untyped) lambda_f
[@@deriving eq, ord, show, map, fold]

type tlambda = (tcommand, Type.t, typed) lambda_f
[@@deriving eq, ord, show, map, fold]

type contract = {contract : (expr, command, Type.t, untyped) contract_f}
[@@deriving eq, ord, show]

type tcontract = {tcontract : (texpr, tcommand, Type.t, typed) contract_f}
[@@deriving eq, ord, show]

type 'v value_f =
  | Literal   of Literal.t
  | Bounded   of Literal.t Type.bounded_cases * Literal.t
  | Contract  of Type.t lcontract
  | Record    of Layout.t * (string * 'v) list
  | Variant   of Layout.t * Type.t Type.row * string * 'v
  | List      of Type.t * 'v list
  | Set       of Type.t * 'v list
  | Map       of Type.t * Type.t * bool * ('v * 'v) list
  | Tuple     of 'v list
  | Closure   of tlambda * 'v list
  | Operation of operation
  | Ticket    of string * 'v * Bigint.t

and value = {v : value value_f}

and tvalue =
  { t : Type.t
  ; tv : tvalue value_f }

and operation =
  | Transfer       of
      { params : value
      ; destination : Type.t lcontract
      ; amount : Bigint.t }
  | SetDelegate    of string option
  | CreateContract of
      { id : contract_id
      ; instance : instance }

and instance =
  { template : tcontract
  ; state : contract_state }

and contract_state =
  { balance : Bigint.t
  ; storage : value option
  ; baker : string option
  ; lazy_entry_points :
      (string * [ `Initial | `Absent | `Closure of tlambda * value list ]) list
  ; metadata : (string * value Meta.t) list }
[@@deriving eq, ord, show, map, fold]

val build_value : value value_f -> value

val cata_value : ('a value_f -> 'a) -> value -> 'a

val cata_tvalue : (Type.t -> 'a value_f -> 'a) -> tvalue -> 'a

val type_of_value : value -> Type.t

type tmessage =
  { channel : string
  ; params : value }
[@@deriving show]

type smart_except =
  [ `Expr of texpr
  | `Exprs of texpr list
  | `Expr_untyped of expr
  | `Value of value
  | `Literal of Literal.t
  | `Line of line_no
  | `Text of string
  | `Type of Type.t
  | `Br
  | `Rec of smart_except list
  ]
[@@deriving show]

type typing_constraint =
  | HasAdd           of texpr * texpr * texpr
  | HasMul           of texpr * texpr * texpr * bool (* overloaded *)
  | HasSub           of texpr * texpr * texpr
  | HasDiv           of texpr * texpr * texpr
  | HasBitArithmetic of texpr * texpr * texpr
  | HasMap           of texpr * texpr * texpr
  | IsComparable     of texpr
  | HasGetItem       of texpr * texpr * Type.t
  | HasContains      of texpr * texpr * line_no
  | HasSize          of texpr
  | HasSlice         of texpr
  | AssertEqual      of Type.t * Type.t * (unit -> smart_except list)
  | IsInt            of Type.t * (unit -> smart_except list)
  | SaplingVerify    of texpr * texpr
  | HasNeg           of texpr * Type.t
  | HasInt           of texpr
  | IsNotHot         of string * Type.t
  | IsAnyMap         of Type.t * Type.t * texpr
  | IsConvertible    of Type.t * Type.t
[@@deriving eq, show]

exception SmartExcept of smart_except list

exception ExecFailure of value * smart_except list

(** Contract execution results, see also {!Contract.execMessageInner}. *)
module Execution : sig
  (** Execution errors, see also the {!Error} module. *)

  type error = Exec_failure of value * smart_except list [@@deriving show]

  val error_of_exception : exn -> error

  val to_smart_except : error -> smart_except list

  type step =
    { command : tcommand
    ; iters : (string * (value * string option)) list
    ; locals : (string * value) list
    ; storage : value
    ; balance : Bigint.t
    ; operations : string list
    ; substeps : step list ref
    ; elements : (string * value) list }
  [@@deriving show]

  type 'html exec_message =
    { ok : bool
    ; contract : instance option
    ; operations : operation list
    ; error : error option
    ; html : 'html
    ; storage : value
    ; steps : step list }
  [@@deriving show]
end

type scenario_state =
  { constants : (string, value) Hashtbl.t
  ; contracts : (contract_id, instance) Hashtbl.t
  ; variables : (string, value) Hashtbl.t
  ; addresses : (contract_id, string) Hashtbl.t
  ; rev_addresses : (string, contract_id) Hashtbl.t
  ; next_dynamic_address_id : int ref
  ; mutable time : Bigint.t
  ; mutable level : Bigint.t }

val scenario_state : unit -> scenario_state

val copy_scenario_state : scenario_state -> scenario_state

val get_parameter_type : tcontract -> string -> Type.t option

(* Checks whether there are exists given sub-expressions or sub-commands
   for which one of the given predicates hold.*)

type 'a exists_in =
     exclude_create_contract:bool
  -> (texpr -> bool)
  -> (tcommand -> bool)
  -> 'a
  -> bool

val exists_expr : texpr exists_in

val exists_command : tcommand exists_in

val exists_contract : (texpr, tcommand, Type.t, typed) contract_f exists_in

type 'address account_or_address =
  | Account of Primitives.account
  | Address of 'address
[@@deriving show, map]

type ('expr, 'command, 'type_, 'switch_) action_f =
  | New_contract           of
      { id : contract_id
      ; contract : ('expr, 'command, 'type_, 'switch_) contract_f
      ; line_no : line_no
      ; accept_unknown_types : bool
      ; show : bool
      ; address : string }
  | Compute                of
      { var : string
      ; expression : 'expr
      ; line_no : line_no }
  | Simulation             of
      { id : contract_id
      ; line_no : line_no }
  | Message                of
      { id : contract_id
      ; valid : 'expr
      ; exception_ : 'expr option
      ; params : 'expr
      ; line_no : line_no
      ; title : string
      ; messageClass : string
      ; source : 'expr account_or_address option
      ; sender : 'expr account_or_address option
      ; chain_id : 'expr option
      ; time : 'expr option
      ; amount : 'expr
      ; level : 'expr option
      ; voting_powers : 'expr
      ; message : string
      ; show : bool
      ; export : bool }
  | ScenarioError          of {message : string}
  | Html                   of
      { tag : string
      ; inner : string
      ; line_no : line_no }
  | Verify                 of
      { condition : 'expr
      ; line_no : line_no }
  | Show                   of
      { expression : 'expr
      ; html : bool
      ; stripStrings : bool
      ; compile : bool
      ; line_no : line_no }
  | Exception              of smart_except list
  | Set_delegate           of
      { id : contract_id
      ; line_no : line_no
      ; baker : 'expr }
  | DynamicContract        of
      { id : dynamic_id
      ; model_id : contract_id
      ; line_no : line_no }
  | Add_flag               of
      { flag : Config.flag
      ; line_no : line_no }
  | Prepare_constant_value of
      { line_no : line_no
      ; var : string
      ; hash : string option
      ; expression : 'expr }
[@@deriving show]

type action = (expr, command, Type.t, untyped) action_f

type taction = (texpr, tcommand, Type.t, typed) action_f

(** A scenario is a list of actions. *)
type scenario_kind = {kind : string} [@@deriving show]

type ('expr, 'command, 'type_, 'switch) scenario_f =
  { shortname : string
  ; actions : (Config.t * ('expr, 'command, 'type_, 'switch) action_f) list
  ; kind : scenario_kind }

val map_scenario_f :
     ('expr -> 'a)
  -> ('command -> 'b)
  -> ('type_ -> 'c)
  -> ('switch -> 'switch)
  -> ('expr, 'command, 'type_, 'switch) scenario_f
  -> ('a, 'b, 'c, 'switch) scenario_f

type scenario = (expr, command, Type.t, untyped) scenario_f

type tscenario = (texpr, tcommand, Type.t, typed) scenario_f

val size_texpr : texpr -> int

val size_tcommand : tcommand -> int

val size_tcontract : (texpr, tcommand, Type.t, typed) contract_f -> int

val erase_types_expr : texpr -> expr

val erase_types_command : tcommand -> command

val erase_types_contract : tcontract -> contract

val layout_records_expr : texpr -> texpr

val layout_records_command : tcommand -> tcommand

val layout_records_instance : instance -> instance

module Syntax (M : MONAD) : sig
  open M

  val sequence_meta : 'a t Meta.t -> 'a Meta.t t

  val sequence_command_f :
    ('e t, 'c t, 't) command_f -> ('e, 'c, 't) command_f t

  val sequence_expr_f :
    ('e t, 'c t, 't, 'switch) expr_f -> ('e, 'c, 't, 'switch) expr_f t

  type ('e, 'c, 't) malg =
    { fm_expr : line_no -> ('e, 'c, 't, untyped) expr_f -> 'e t
    ; fm_command : line_no -> ('e, 'c, 't) command_f -> 'c t
    ; fm_type : Type.t -> 't }

  val cataM_expr : ('e, 'c, 't) malg -> expr -> 'e t

  val cataM_command : ('e, 'c, 'c) malg -> command -> 'c t
end

val check_initial_flag : line_no:line_no -> Config.flag -> unit

val build_contract :
     ?balance:expr
  -> ?storage:expr
  -> ?baker:expr
  -> ?tstorage_explicit:Type.t
  -> ?flags:Config.flag list
  -> ?private_variables:(string * expr) list
  -> ?metadata:(string * expr Meta.t) list
  -> ?views:(command, Type.t, untyped) view list
  -> ?entry_points_layout:Layout.t
  -> (command, Type.t, untyped) entry_point list
  -> contract

val build_entry_point :
     name:string
  -> ?tparameter:Type.t
  -> ?originate:bool
  -> ?lazify:bool
  -> ?lazy_no_code:bool
  -> ?line_no:line_no
  -> command
  -> (command, Type.t, untyped) entry_point

val address_of_contract_id : contract_id -> string
