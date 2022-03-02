(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils_pure
open Control
include Ids
module Literal = Literal

type typed = | [@@deriving eq, ord, show {with_path = false}]

type untyped = | [@@deriving eq, ord, show {with_path = false}]

let elim_typed = function
  | (_ : typed) -> .

let elim_untyped = function
  | (_ : untyped) -> .

type (_, _) extra =
  | U : ('a, untyped) extra
  | T : 'a -> ('a, typed) extra

let get_extra = function
  | T x -> x

let equal_extra :
    type a s.
    (a -> a -> bool) -> (s -> s -> bool) -> (a, s) extra -> (a, s) extra -> bool
    =
 fun eq _ x y ->
  match (x, y) with
  | U, U -> true
  | T x, T y -> eq x y

let compare_extra :
    type a s.
    (a -> a -> int) -> (s -> s -> int) -> (a, s) extra -> (a, s) extra -> int =
 fun cmp _ x y ->
  match (x, y) with
  | U, U -> 0
  | T x, T y -> cmp x y

let pp_extra :
    type a s.
       (Format.formatter -> a -> unit)
    -> (Format.formatter -> s -> unit)
    -> Format.formatter
    -> (a, s) extra
    -> unit =
 fun pp _ ppf -> function
  | U -> Format.fprintf ppf "U"
  | T x -> Format.fprintf ppf "T %a" pp x

let show_extra pp1 pp2 = Format.asprintf "%a" (pp_extra pp1 pp2)

let map_extra : type a b s. (a -> b) -> (s -> s) -> (a, s) extra -> (b, s) extra
    =
 fun f _ -> function
  | T x -> T (f x)
  | U -> U

let fold_extra :
    type a b s. (b -> a -> b) -> (b -> s -> b) -> b -> (a, s) extra -> b =
 fun f _ s -> function
  | T x -> f s x
  | U -> s

type vClass =
  | Simple
  | Local
  | Scenario
[@@deriving eq, ord, show {with_path = false}]

type 'v record_f = (string * 'v) list [@@deriving show {with_path = false}]

let sort_record r = List.sort (fun (lbl1, _) (lbl2, _) -> compare lbl1 lbl2) r

let equal_record_f eq_val x y =
  let eq_entry (lbl1, val1) (lbl2, val2) = lbl1 = lbl2 && eq_val val1 val2 in
  List.equal eq_entry (sort_record x) (sort_record y)

let compare_record_f cmp_val x y =
  let cmp_entry (lbl1, val1) (lbl2, val2) =
    match compare lbl1 lbl2 with
    | 0 -> cmp_val val1 val2
    | c -> c
  in
  List.compare cmp_entry (sort_record x) (sort_record y)

type binOp =
  | BAdd
  | BAnd
  | BDiv
  | BEDiv
  | BEq
  | BGe
  | BGt
  | BLe
  | BLt
  | BMax
  | BMin
  | BMod
  | BMul  of {overloaded : bool}
  | BNeq
  | BOr
  | BSub
  | BXor
[@@deriving show {with_path = false}, eq, ord]

type micheline =
  | Int       of string
  | String    of string
  | Bytes     of string
  | Primitive of
      { name : string
      ; annotations : string list
      ; arguments : micheline list }
  | Sequence  of micheline list
[@@deriving eq, show {with_path = false}, ord]

type 't inline_michelson =
  { name : string
  ; parsed : micheline
  ; typesIn : 't list
  ; typesOut : 't list }
[@@deriving eq, show {with_path = false}, map, fold, ord]

type line_no = (string * int) list
[@@deriving eq, ord, show {with_path = false}]

type ('command, 'type_, 'switch) entry_point =
  { channel : string
  ; tparameter_ep : [ `Absent | `Present | `Annotated of Type.t ]
  ; originate : bool
  ; lazify : bool option
  ; lazy_no_code : bool option
  ; line_no : line_no
  ; body : 'command
  ; tparameter_ep_derived : ('type_, 'switch) extra }
[@@deriving eq, ord, show {with_path = false}, map, fold]

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
[@@deriving eq, ord, show {with_path = false}, map, fold]

type address =
  | Real  of string
  | Local of contract_id
[@@deriving eq, ord, show {with_path = false}]

type 'type_ contract_derived =
  { tparameter : 'type_
  ; tparameter_lazy : 'type_ option
  ; tparameter_non_lazy : 'type_ option
  ; tparameter_full : 'type_
  ; tstorage : 'type_
  ; config : Config.t }
[@@deriving eq, ord, show {with_path = false}, map, fold]

module Meta = struct
  type 'a t =
    | List  of 'a t list
    | Map   of ('a * 'a t) list
    | Other of 'a
    | View  of string
  [@@deriving eq, ord, show {with_path = false}, map, fold]
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
[@@deriving show {with_path = false}, map, fold, eq, ord]

type ('command, 'type_, 'switch) lambda_f =
  { name : string
  ; body : 'command
  ; clean_stack : bool
  ; with_storage : Type.with_storage option
  ; with_operations : bool
  ; tParams : ('type_, 'switch) extra
  ; tResult : ('type_, 'switch) extra }
[@@deriving eq, ord, show {with_path = false}, map, fold]

type 'type_ lcontract =
  { address : string
  ; entry_point : string option
  ; type_ : 'type_ }
[@@deriving eq, ord, show {with_path = false}, map, fold]

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
[@@deriving eq, ord, show {with_path = false}, map, fold]

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
[@@deriving eq, ord, show {with_path = false}, map, fold]

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
[@@deriving eq, ord, show {with_path = false}, map, fold]

type 'type_ prim3 =
  | ESplit_tokens
  | ERange
  | EUpdate_map
  | EGet_and_update
  | ETest_ticket
[@@deriving eq, ord, show {with_path = false}, map, fold]

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
[@@deriving eq, ord, show {with_path = false}, map, fold]

type record_field_binding =
  { var : string
  ; field : string }
[@@deriving eq, ord, show {with_path = false}]

type pattern =
  | Pattern_single of string
  | Pattern_tuple  of string list
  | Pattern_record of string * record_field_binding list
[@@deriving eq, ord, show {with_path = false}]

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
[@@deriving show {with_path = false}, map, fold, eq, ord]

let string_of_line_no = function
  | [] -> "(no location info)"
  | (_, i) :: _ -> string_of_int i

let head_line_no = function
  | (_, hd) :: _ -> hd
  | [] -> -1

module Typed = struct
  type texpr =
    { e : (texpr, tcommand, Type.t, typed) expr_f
    ; et : Type.t
    ; line_no : line_no }

  and tcommand =
    { c : (texpr, tcommand, Type.t) command_f
    ; ct : Type.t
    ; line_no : line_no }
  [@@deriving show {with_path = false}, eq, ord]

  let build_texpr ~line_no e et = {e; et; line_no}

  let build_tcommand ~line_no c ct = {c; ct; line_no}
end

open Typed

module Untyped = struct
  type expr =
    { e : (expr, command, Type.t, untyped) expr_f
    ; line_no : line_no }

  and command =
    { c : (expr, command, Type.t) command_f
    ; line_no : line_no }
  [@@deriving eq, ord, show {with_path = false}]
end

open Untyped

let rec equal_expr_modulo_line_nos e1 e2 =
  equal_expr_f
    equal_expr_modulo_line_nos
    equal_command_modulo_line_nos
    Type.equal
    equal_untyped
    e1.e
    e2.e

and equal_command_modulo_line_nos c1 c2 =
  equal_command_f
    equal_expr_modulo_line_nos
    equal_command_modulo_line_nos
    Type.equal
    c1.c
    c2.c

type ('e, 'c, 't) tsyntax_alg =
  { f_texpr : line_no -> Type.t -> ('e, 'c, 't, typed) expr_f -> 'e
  ; f_tcommand : line_no -> Type.t -> ('e, 'c, 't) command_f -> 'c
  ; f_ttype : Type.t -> 't }

let cata_t {f_texpr; f_tcommand; f_ttype} =
  let rec ce {e; et; line_no} =
    f_texpr line_no et (map_expr_f ce cc f_ttype id e)
  and cc {c; ct; line_no} =
    match c with
    | CBind (None, c1, c2) ->
        (* Optimized case for tail recursion. *)
        let c1 = cc c1 in
        f_tcommand line_no ct (CBind (None, c1, cc c2))
    | _ -> f_tcommand line_no ct (map_command_f ce cc f_ttype c)
  in
  (ce, cc)

let cata_texpr alg = fst (cata_t alg)

let cata_tcommand alg = snd (cata_t alg)

type ('e, 'c, 't) texpr_p =
  (texpr * 'e, tcommand * 'c, Type.t * 't, typed) expr_f

type ('e, 'c, 't) tcommand_p =
  (texpr * 'e, tcommand * 'c, Type.t * 't) command_f

let para_talg ~p_texpr ~p_tcommand ~p_ttype =
  let f_texpr line_no et e =
    ({e = map_expr_f fst fst fst id e; et; line_no}, p_texpr line_no et e)
  in
  let f_tcommand line_no ct c =
    ({c = map_command_f fst fst fst c; ct; line_no}, p_tcommand line_no ct c)
  in
  let f_ttype t = (t, p_ttype t) in
  {f_texpr; f_tcommand; f_ttype}

let para_texpr alg e = snd (cata_texpr alg e)

let para_tcommand alg c = snd (cata_tcommand alg c)

(* A paramorphic algebra on a monoid. *)
let monoid_para_talg append empty =
  let append x y = append x (snd y) in
  let p_texpr _ _ =
    fold_expr_f append append append (fun _ -> elim_typed) empty
  in
  let p_tcommand _ _ = fold_command_f append append append empty in
  let p_ttype _ = empty in
  para_talg ~p_texpr ~p_tcommand ~p_ttype

type ('e, 'c, 't) syntax_alg =
  { f_expr : line_no -> ('e, 'c, 't, untyped) expr_f -> 'e
  ; f_command : line_no -> ('e, 'c, 't) command_f -> 'c
  ; f_type : Type.t -> 't }

let cata {f_expr; f_command; f_type} =
  let rec ce {e; line_no} = f_expr line_no (map_expr_f ce cc f_type id e)
  and cc {c; line_no} =
    match c with
    | CBind (None, c1, c2) ->
        (* Optimized case for tail recursion. *)
        let c1 = cc c1 in
        f_command line_no (CBind (None, c1, cc c2))
    | _ -> f_command line_no (map_command_f ce cc f_type c)
  in
  (ce, cc)

let cata_expr alg = fst (cata alg)

let cata_command alg = snd (cata alg)

(* An algebra on a monoid. *)
let monoid_alg append empty =
  { f_expr =
      (fun _ -> fold_expr_f append append append (fun _ -> elim_untyped) empty)
  ; f_command = (fun _ -> fold_command_f append append append empty)
  ; f_type = (fun _ -> empty) }

type ('e, 'c, 't) expr_p =
  (expr * 'e, command * 'c, Type.t * 't, untyped) expr_f

type ('e, 'c, 't) command_p = (expr * 'e, command * 'c, Type.t * 't) command_f

let para_alg ~p_expr ~p_command ~p_type =
  let f_expr line_no e =
    ({e = map_expr_f fst fst fst id e; line_no}, p_expr line_no e)
  in
  let f_command line_no c =
    ({c = map_command_f fst fst fst c; line_no}, p_command line_no c)
  in
  let f_type t = (t, p_type t) in
  {f_expr; f_command; f_type}

let para_expr alg e = snd (cata_expr alg e)

let para_command alg c = snd (cata_command alg c)

(* An algebra on a monoid. *)
let monoid_talg append empty =
  { f_texpr =
      (fun _ _ -> fold_expr_f append append append (fun _ -> elim_typed) empty)
  ; f_tcommand = (fun _ _ -> fold_command_f append append append empty)
  ; f_ttype = (fun _ -> empty) }

let size_talg = monoid_talg ( + ) 1

let size_tcommand = cata_tcommand size_talg

let size_texpr = cata_texpr size_talg

type lambda = (command, Type.t, untyped) lambda_f
[@@deriving eq, ord, show {with_path = false}, map, fold]

type tlambda = (tcommand, Type.t, typed) lambda_f
[@@deriving eq, ord, show {with_path = false}, map, fold]

type contract = {contract : (expr, command, Type.t, untyped) contract_f}
[@@deriving eq, ord, show {with_path = false}]

type tcontract = {tcontract : (texpr, tcommand, Type.t, typed) contract_f}
[@@deriving eq, ord, show {with_path = false}]

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
[@@deriving eq, ord, show {with_path = false}, map, fold]

let build_value v = {v}

let rec cata_value f {v} = f (map_value_f (cata_value f) v)

let rec cata_tvalue f {t; tv} = f t (map_value_f (cata_tvalue f) tv)

let type_of_value =
  let f = function
    | Literal l -> Type.type_of_literal l
    | Bounded (cases, l) ->
        Type.bounded (Type.type_of_literal l) cases.final cases.cases
    | Contract {type_} -> Type.contract type_
    | Record (layout, row) -> Type.record (Unknown.value layout) row
    | Variant (_, [("None", _); ("Some", t)], _, _) -> Type.option t
    | Variant (layout, row, _, _) -> Type.variant (Unknown.value layout) row
    | List (t, _) -> Type.list t
    | Set (telement, _) -> Type.set ~telement
    | Map (tkey, tvalue, big, _) -> Type.map ~big ~tkey ~tvalue
    | Tuple ts -> Type.tuple ts
    | Closure ({tParams; tResult; with_storage; with_operations}, args) ->
        let rec apply args t =
          match (args, t) with
          | [], _ -> t
          | _ :: args, Type.(F (TLambda (_, F (TTuple [_t1; t2]), t))) ->
              apply args (Type.(lambda no_effects) t2 t)
          | _ -> failwith "not a lambda"
        in
        apply
          args
          (Type.lambda
             { with_storage = Unknown.value with_storage
             ; with_operations = Unknown.value with_operations }
             (get_extra tParams)
             (get_extra tResult))
    | Operation _ -> Type.operation
    | Ticket (_, t, _) -> Type.ticket t
  in
  cata_value f

type tmessage =
  { channel : string
  ; params : value }
[@@deriving show {with_path = false}]

let size_tcontract ({entry_points} : _ contract_f) =
  List.fold_left
    (fun s (ep : _ entry_point) -> s + size_tcommand ep.body)
    0
    entry_points

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
[@@deriving show {with_path = false}]

type lazy_smart_except_list = unit -> smart_except list
[@@deriving show {with_path = false}]

let equal_lazy_smart_except_list _ _ = true

type typing_constraint =
  | HasAdd           of texpr * texpr * texpr
  | HasMul           of texpr * texpr * texpr * bool
  | HasSub           of texpr * texpr * texpr
  | HasDiv           of texpr * texpr * texpr
  | HasBitArithmetic of texpr * texpr * texpr
  | HasMap           of texpr * texpr * texpr
  | IsComparable     of texpr
  | HasGetItem       of texpr * texpr * Type.t
  | HasContains      of texpr * texpr * line_no
  | HasSize          of texpr
  | HasSlice         of texpr
  | AssertEqual      of Type.t * Type.t * lazy_smart_except_list
  | IsInt            of Type.t * lazy_smart_except_list
  | SaplingVerify    of texpr * texpr
  | HasNeg           of texpr * Type.t
  | HasInt           of texpr
  | IsNotHot         of string * Type.t
  | IsAnyMap         of Type.t * Type.t * texpr
  | IsConvertible    of Type.t * Type.t
[@@deriving eq, show {with_path = false}]

exception SmartExcept of smart_except list

exception ExecFailure of value * smart_except list

module Execution = struct
  type error = Exec_failure of value * smart_except list
  [@@deriving show {with_path = false}]

  let error_of_exception exn =
    let string s = build_value (Literal (Literal.string s)) in
    match exn with
    | Failure f -> Exec_failure (string f, [`Text "Failure:"; `Text f])
    | ExecFailure (value, message) -> Exec_failure (value, message)
    | SmartExcept l -> Exec_failure (string "Error", l)
    | _exn -> assert false

  let to_smart_except = function
    | Exec_failure (_, message) -> message

  type step =
    { command : tcommand
    ; iters : (string * (value * string option)) list
    ; locals : (string * value) list
    ; storage : value
    ; balance : Bigint.t
    ; operations : string list
    ; substeps : step list ref
    ; elements : (string * value) list }
  [@@deriving show {with_path = false}]

  type 'html exec_message =
    { ok : bool
    ; contract : instance option
    ; operations : operation list
    ; error : error option
    ; html : 'html
    ; storage : value
    ; steps : step list }
  [@@deriving show {with_path = false}]
end

type scenario_state =
  { constants :
      (string (* constant hash *), value (* constant value *)) Hashtbl.t
  ; contracts : (contract_id, instance) Hashtbl.t
  ; variables : (string, value) Hashtbl.t
  ; addresses : (contract_id, string) Hashtbl.t
  ; rev_addresses : (string, contract_id) Hashtbl.t
  ; next_dynamic_address_id : int ref
  ; mutable time : Bigint.t
  ; mutable level : Bigint.t }

let scenario_state () =
  { constants = Hashtbl.create 5
  ; contracts = Hashtbl.create 5
  ; variables = Hashtbl.create 5
  ; addresses = Hashtbl.create 5
  ; rev_addresses = Hashtbl.create 5
  ; next_dynamic_address_id = ref 0
  ; time = Bigint.zero_big_int
  ; level = Bigint.zero_big_int }

let copy_scenario_state
    { constants
    ; contracts
    ; variables
    ; addresses
    ; rev_addresses
    ; next_dynamic_address_id
    ; time
    ; level } =
  let contracts = Hashtbl.copy contracts in
  let variables = Hashtbl.copy variables in
  let addresses = Hashtbl.copy addresses in
  let rev_addresses = Hashtbl.copy rev_addresses in
  let next_dynamic_address_id = ref !next_dynamic_address_id in
  { constants
  ; contracts
  ; variables
  ; addresses
  ; rev_addresses
  ; next_dynamic_address_id
  ; time
  ; level }

let get_parameter_type {tcontract} name =
  let filtered_entry_points =
    List.filter
      (fun ({channel = x} : _ entry_point) -> name = x)
      tcontract.entry_points
  in
  match filtered_entry_points with
  | [{tparameter_ep_derived}] -> Some (get_extra tparameter_ep_derived)
  | _ -> None

type 'a exists_in =
     exclude_create_contract:bool
  -> (texpr -> bool)
  -> (tcommand -> bool)
  -> 'a
  -> bool

let exists_talg ~exclude_create_contract f_expr f_command =
  let sub b1 (_, b2) = b1 || b2 in
  let p_texpr line_no et e =
    let holds_below =
      match e with
      | ECreate_contract {baker; balance; storage} when exclude_create_contract
        ->
          snd baker || snd balance || snd storage
      | e -> fold_expr_f sub sub sub (fun _ -> elim_typed) false e
    in
    f_expr {e = map_expr_f fst fst fst id e; et; line_no} || holds_below
  in
  let p_tcommand line_no ct c =
    f_command {c = map_command_f fst fst fst c; ct; line_no}
    || fold_command_f sub sub sub false c
  in
  let p_ttype _ = false in
  para_talg ~p_texpr ~p_tcommand ~p_ttype

let exists_expr ~exclude_create_contract f_expr f_command =
  para_texpr (exists_talg ~exclude_create_contract f_expr f_command)

let exists_command ~exclude_create_contract f_expr f_command =
  para_tcommand (exists_talg ~exclude_create_contract f_expr f_command)

let exists_contract ~exclude_create_contract f_expr f_command c =
  let f_expr = exists_expr ~exclude_create_contract f_expr f_command in
  let f_command = exists_command ~exclude_create_contract f_expr f_command in
  let c = map_contract_f f_expr f_command id id c in
  fold_contract_f ( || ) ( || ) (curry fst) (curry fst) false c

type 'address account_or_address =
  | Account of Primitives.account
  | Address of 'address
[@@deriving show {with_path = false}, map]

type ('expr, 'command, 'type_, 'switch) action_f =
  | New_contract           of
      { id : contract_id
      ; contract : ('expr, 'command, 'type_, 'switch) contract_f
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
[@@deriving show {with_path = false}, map]

type action = (expr, command, Type.t, untyped) action_f

type taction = (texpr, tcommand, Type.t, typed) action_f

type scenario_kind = {kind : string} [@@deriving show {with_path = false}]

type ('expr, 'command, 'type_, 'switch) scenario_f =
  { shortname : string
  ; actions : (Config.t * ('expr, 'command, 'type_, 'switch) action_f) list
  ; kind : scenario_kind }
[@@deriving map]

type scenario = (expr, command, Type.t, untyped) scenario_f

type tscenario = (texpr, tcommand, Type.t, typed) scenario_f

let rec erase_types_alg =
  { f_texpr =
      (fun line_no _ e ->
        let e =
          match e with
          | ECreate_contract {contract_template = c; baker; balance; storage} ->
              let c = erase_contract_aux c in
              ECreate_contract {contract_template = c; baker; balance; storage}
          | ELambda l ->
              let l = {l with tParams = U; tResult = U} in
              ELambda l
          | ( EVar _ | EMPrim0 _ | EMPrim1 _ | EMPrim1_fail _ | EMPrim2 _
            | EMPrim3 _ | EPrim0 _ | EPrim1 _ | EPrim2 _ | EPrim3 _
            | EOpenVariant _ | ETuple _ | ERecord _ | EList _ | EMap _ | ESet _
            | EMatch _ | EItem _ | ESaplingVerifyUpdate _ | EMichelson _
            | EMapFunction _ | EContract _ | ESlice _ | EMake_signature _
            | ETransfer _ | EHasEntryPoint _ | EPrivate _ | EIf _ | EIsFailing _
            | ECatchException _ ) as e ->
              e
        in
        {e; line_no})
  ; f_tcommand = (fun line_no _ c -> {c; line_no})
  ; f_ttype = (fun t -> t) }

and erase_contract_aux c =
  { c with
    entry_points =
      List.map (fun ep -> {ep with tparameter_ep_derived = U}) c.entry_points
  ; views = List.map (fun ov -> {ov with tparameter_derived = U}) c.views
  ; derived = U }

let erase_types_command = cata_tcommand erase_types_alg

let erase_types_expr = cata_texpr erase_types_alg

let erase_types_contract {tcontract = c} =
  let c = map_contract_f erase_types_expr erase_types_command id id c in
  {contract = erase_contract_aux c}

let layout_records_f line_no et = function
  | ERecord l ->
      let layout =
        match Type.getRepr et with
        | TRecord {layout} -> Unknown.get layout
        | _ -> None
      in
      let fields =
        match layout with
        | None -> List.map (fun (x, _) -> (x, x)) l
        | Some layout ->
            let f Layout.{source; target} = (source, target) in
            List.map f (Binary_tree.to_list layout)
      in
      let e =
        ERecord
          (List.map
             (fun (n, _) ->
               let x = List.assoc n l in
               (n, x))
             fields)
      in
      {e; et; line_no}
  | e -> {e; et; line_no}

let layout_records_alg =
  { f_texpr = layout_records_f
  ; f_tcommand = (fun line_no ct c -> {c; ct; line_no})
  ; f_ttype = id }

let layout_records_expr = cata_texpr layout_records_alg

let layout_records_command = cata_tcommand layout_records_alg

let layout_records_tcontract {tcontract} =
  { tcontract =
      map_contract_f
        layout_records_expr
        layout_records_command
        (fun x -> x)
        (fun x -> x)
        tcontract }

let layout_records_instance {template; state} =
  {template = layout_records_tcontract template; state}

module Syntax (M : MONAD) = struct
  open M

  let rec sequence_meta =
    let open Meta in
    function
    | List xs ->
        let+ xs = map_list sequence_meta xs in
        List xs
    | Map xs ->
        let f (k, v) =
          let+ k = k
          and+ v = sequence_meta v in
          (k, v)
        in
        let+ xs = map_list f xs in
        Map xs
    | Other x ->
        let+ x = x in
        Other x
    | View _ as m -> return m

  let sequence_entry_point (x : _ entry_point) =
    let+ body = x.body in
    {x with body}

  let sequence_offchain_view (x : _ view) =
    let+ body = x.body in
    {x with body}

  let sequence_contract_f (c : _ contract_f) =
    let* balance = sequence_option c.balance in
    let* storage = sequence_option c.storage in
    let* baker = sequence_option c.baker in
    let* entry_points = map_list sequence_entry_point c.entry_points in
    let* private_variables = map_list sequence_snd c.private_variables in
    let* metadata =
      map_list
        (fun (x, y) ->
          let+ y = sequence_meta y in
          (x, y))
        c.metadata
    in
    let* views = map_list sequence_offchain_view c.views in
    return
      { c with
        balance
      ; storage
      ; baker
      ; entry_points
      ; private_variables
      ; metadata
      ; views }

  let sequence_command_f = function
    | CNever e -> (fun e -> CNever e) <$> e
    | CFailwith e -> (fun e -> CFailwith e) <$> e
    | CVerify (e, m) -> (fun e m -> CVerify (e, m)) <$> e <*> sequence_option m
    | CIf (e, c1, c2) -> (fun e c1 c2 -> CIf (e, c1, c2)) <$> e <*> c1 <*> c2
    | CMatch (scrutinee, cases) ->
        let* scrutinee = scrutinee in
        let* cases =
          flip map_list cases (fun (c, a, b) ->
              let* b = b in
              return (c, a, b))
        in
        return (CMatch (scrutinee, cases))
    | CMatchCons {expr; id; ok_match; ko_match} ->
        (fun expr ok_match ko_match ->
          CMatchCons {expr; id; ok_match; ko_match})
        <$> expr
        <*> ok_match
        <*> ko_match
    | CMatchProduct (e, p, c) ->
        let+ e = e
        and+ c = c in
        CMatchProduct (e, p, c)
    | CModifyProduct (e, p, c) ->
        let+ e = e
        and+ c = c in
        CModifyProduct (e, p, c)
    | CDefineLocal {var; rhs; is_mutable} ->
        (fun rhs -> CDefineLocal {var; rhs; is_mutable}) <$> rhs
    | CSetVar (e1, e2) -> (fun e1 e2 -> CSetVar (e1, e2)) <$> e1 <*> e2
    | CDelItem (e1, e2) -> (fun e1 e2 -> CDelItem (e1, e2)) <$> e1 <*> e2
    | CUpdateSet (e1, e2, b) ->
        (fun e1 e2 -> CUpdateSet (e1, e2, b)) <$> e1 <*> e2
    | CBind (x, c1, c2) ->
        let* c1 = c1 in
        let* c2 = c2 in
        return (CBind (x, c1, c2))
    | CFor (x, e, c) -> (fun e c -> CFor (x, e, c)) <$> e <*> c
    | CWhile (e, c) -> (fun e c -> CWhile (e, c)) <$> e <*> c
    | CResult e -> (fun e -> CResult e) <$> e
    | CComment _ as e -> return e
    | CTrace e -> (fun e -> CTrace e) <$> e
    | CSetType (e, t) -> (fun e -> CSetType (e, t)) <$> e
    | CSetResultType (c, t) -> (fun c -> CSetResultType (c, t)) <$> c
    | CSetEntryPoint (s, e) -> (fun e -> CSetEntryPoint (s, e)) <$> e

  let sequence_expr_f = function
    | (EVar _ | EPrivate _ | EMPrim0 _) as e -> return e
    | EMPrim1 (p, e) -> (fun e -> EMPrim1 (p, e)) <$> e
    | EMPrim1_fail (p, e) -> (fun e -> EMPrim1_fail (p, e)) <$> e
    | EMPrim2 (p, e1, e2) -> (fun e1 e2 -> EMPrim2 (p, e1, e2)) <$> e1 <*> e2
    | EMPrim3 (p, e1, e2, e3) ->
        (fun e1 e2 e3 -> EMPrim3 (p, e1, e2, e3)) <$> e1 <*> e2 <*> e3
    | EPrim0 _ as e -> return e
    | EPrim1 (p, e) -> (fun e -> EPrim1 (p, e)) <$> e
    | EPrim2 (p, e1, e2) -> (fun e1 e2 -> EPrim2 (p, e1, e2)) <$> e1 <*> e2
    | EPrim3 (p, e1, e2, e3) ->
        (fun e1 e2 e3 -> EPrim3 (p, e1, e2, e3)) <$> e1 <*> e2 <*> e3
    | EOpenVariant (v, e1, e2) ->
        (fun e1 e2 -> EOpenVariant (v, e1, e2)) <$> e1 <*> sequence_option e2
    | EItem {items; key; default_value; missing_message} ->
        (fun items key default_value missing_message ->
          EItem {items; key; default_value; missing_message})
        <$> items
        <*> key
        <*> sequence_option default_value
        <*> sequence_option missing_message
    | ETuple es -> (fun es -> ETuple es) <$> sequence_list es
    | ERecord es -> (fun es -> ERecord es) <$> map_list sequence_snd es
    | EList es -> (fun es -> EList es) <$> sequence_list es
    | EMap (big, es) -> (fun es -> EMap (big, es)) <$> map_list sequence_pair es
    | ESet es -> (fun es -> ESet es) <$> sequence_list es
    | ESaplingVerifyUpdate {state; transaction} ->
        (fun state transaction -> ESaplingVerifyUpdate {state; transaction})
        <$> state
        <*> transaction
    | EMichelson (instrs, es) ->
        (fun es -> EMichelson (instrs, es)) <$> sequence_list es
    | EMapFunction {f; l} -> (fun f l -> EMapFunction {f; l}) <$> f <*> l
    | ELambda
        { name
        ; tParams
        ; tResult
        ; body
        ; clean_stack
        ; with_storage
        ; with_operations } ->
        (fun body ->
          ELambda
            { name
            ; tParams
            ; tResult
            ; body
            ; clean_stack
            ; with_storage
            ; with_operations })
        <$> body
    | ECreate_contract {contract_template; baker; balance; storage} ->
        (fun contract_template baker balance storage ->
          ECreate_contract {contract_template; baker; balance; storage})
        <$> sequence_contract_f contract_template
        <*> baker
        <*> balance
        <*> storage
    | EContract {entry_point; arg_type; address} ->
        (fun address -> EContract {entry_point; arg_type; address}) <$> address
    | ESlice {offset; length; buffer} ->
        (fun offset length buffer -> ESlice {offset; length; buffer})
        <$> offset
        <*> length
        <*> buffer
    | EMake_signature {secret_key; message; message_format} ->
        (fun secret_key message ->
          EMake_signature {secret_key; message; message_format})
        <$> secret_key
        <*> message
    | ETransfer {arg; amount; destination} ->
        (fun arg amount destination -> ETransfer {arg; amount; destination})
        <$> arg
        <*> amount
        <*> destination
    | EMatch (scrutinee, cases) ->
        (fun scrutinee cases -> EMatch (scrutinee, cases))
        <$> scrutinee
        <*> map_list sequence_snd cases
    | EHasEntryPoint _ as e -> return e
    | EIf (c, t, e) ->
        let* c = c in
        let* t = t in
        let* e = e in
        return (EIf (c, t, e))
    | EIsFailing x ->
        let* x = x in
        return (EIsFailing x)
    | ECatchException (t, x) ->
        let* x = x in
        return (ECatchException (t, x))

  type ('e, 'c, 't) malg =
    { fm_expr : line_no -> ('e, 'c, 't, untyped) expr_f -> 'e t
    ; fm_command : line_no -> ('e, 'c, 't) command_f -> 'c t
    ; fm_type : Type.t -> 't }

  let cata {fm_expr; fm_command; fm_type} =
    let rec ce {e; line_no} =
      let* e = sequence_expr_f (map_expr_f ce cc fm_type id e) in
      fm_expr line_no e
    and cc {c; line_no} =
      let* c = sequence_command_f (map_command_f ce cc fm_type c) in
      fm_command line_no c
    in
    (ce, cc)

  let cataM_expr alg = fst (cata alg)

  let cataM_command alg = snd (cata alg)
end

let check_initial_flag ~line_no flag =
  if Config.is_initial_flag flag
  then
    raise
      (SmartExcept
         [ `Text
             (Printf.sprintf
                "Flag %S can only be set in the command line or at the \
                 beginning of a scenario."
                (Config.show_flag flag))
         ; `Line line_no ])

let build_contract
    ?balance
    ?storage
    ?baker
    ?tstorage_explicit
    ?(flags = [])
    ?(private_variables = [])
    ?(metadata = [])
    ?(views = [])
    ?entry_points_layout
    entry_points =
  { contract =
      { template_id = None
      ; balance
      ; storage
      ; baker
      ; tstorage_explicit
      ; entry_points
      ; entry_points_layout
      ; unknown_parts = None
      ; flags
      ; private_variables
      ; metadata
      ; views
      ; derived = U } }

let build_entry_point
    ~name ?tparameter ?(originate = true) ?lazify ?lazy_no_code ?line_no body =
  let line_no = Option.default body.line_no line_no in
  { channel = name
  ; tparameter_ep =
      ( match tparameter with
      | None -> `Absent
      | Some t -> `Annotated t )
  ; originate
  ; lazify
  ; lazy_no_code
  ; line_no
  ; body
  ; tparameter_ep_derived = U }

let static_addresses =
  [| "KT1TezoooozzSmartPyzzSTATiCzzzwwBFA1"
   ; "KT1Tezooo1zzSmartPyzzSTATiCzzzyfC8eF"
   ; "KT1Tezooo2zzSmartPyzzSTATiCzzzwqqQ4H"
   ; "KT1Tezooo3zzSmartPyzzSTATiCzzzseJjWC"
   ; "KT1Tezooo4zzSmartPyzzSTATiCzzzyPVdv3"
   ; "KT1Tezooo5zzSmartPyzzSTATiCzzzz48Z4p"
   ; "KT1Tezooo6zzSmartPyzzSTATiCzzztY1196"
   ; "KT1Tezooo7zzSmartPyzzSTATiCzzzvTbG1z"
   ; "KT1Tezooo8zzSmartPyzzSTATiCzzzzp29d1"
   ; "KT1Tezooo9zzSmartPyzzSTATiCzzztdBMLX"
   ; "KT1Tezoo1ozzSmartPyzzSTATiCzzzw8CmuY"
   ; "KT1Tezoo11zzSmartPyzzSTATiCzzzzYMWmQ"
   ; "KT1Tezoo12zzSmartPyzzSTATiCzzzw2ZWPM"
   ; "KT1Tezoo13zzSmartPyzzSTATiCzzzyrBos2"
   ; "KT1Tezoo14zzSmartPyzzSTATiCzzzrnXGeA"
   ; "KT1Tezoo15zzSmartPyzzSTATiCzzzzjjBRr"
   ; "KT1Tezoo16zzSmartPyzzSTATiCzzzyHEQ7h"
   ; "KT1Tezoo17zzSmartPyzzSTATiCzzzrQkWKN"
   ; "KT1Tezoo18zzSmartPyzzSTATiCzzzt4Mx4x"
   ; "KT1Tezoo19zzSmartPyzzSTATiCzzzzvx27N"
   ; "KT1Tezoo2ozzSmartPyzzSTATiCzzzt3UDWP"
   ; "KT1Tezoo21zzSmartPyzzSTATiCzzzzPeb4T"
   ; "KT1Tezoo22zzSmartPyzzSTATiCzzzzKyojP"
   ; "KT1Tezoo23zzSmartPyzzSTATiCzzzzLNLUR"
   ; "KT1Tezoo24zzSmartPyzzSTATiCzzzuimc59"
   ; "KT1Tezoo25zzSmartPyzzSTATiCzzzrmbVsM"
   ; "KT1Tezoo26zzSmartPyzzSTATiCzzzwDQDCm"
   ; "KT1Tezoo27zzSmartPyzzSTATiCzzztVDNZ1"
   ; "KT1Tezoo28zzSmartPyzzSTATiCzzzurgreu"
   ; "KT1Tezoo29zzSmartPyzzSTATiCzzzoiUVra"
   ; "KT1Tezoo3ozzSmartPyzzSTATiCzzzzpy1LW"
   ; "KT1Tezoo31zzSmartPyzzSTATiCzzzxZF7L4"
   ; "KT1Tezoo32zzSmartPyzzSTATiCzzzzJfoLr"
   ; "KT1Tezoo33zzSmartPyzzSTATiCzzzt9n3Ca"
   ; "KT1Tezoo34zzSmartPyzzSTATiCzzzxZRxk5"
   ; "KT1Tezoo35zzSmartPyzzSTATiCzzzxEBbbo"
   ; "KT1Tezoo36zzSmartPyzzSTATiCzzzzHx9Mg"
   ; "KT1Tezoo37zzSmartPyzzSTATiCzzzyWyuj1"
   ; "KT1Tezoo38zzSmartPyzzSTATiCzzzwrpEt4"
   ; "KT1Tezoo39zzSmartPyzzSTATiCzzzsxXjKw"
   ; "KT1Tezoo4ozzSmartPyzzSTATiCzzzwJqoPS"
   ; "KT1Tezoo41zzSmartPyzzSTATiCzzzsmVb4M"
   ; "KT1Tezoo42zzSmartPyzzSTATiCzzzwtnVrK"
   ; "KT1Tezoo43zzSmartPyzzSTATiCzzzqz2ouC"
   ; "KT1Tezoo44zzSmartPyzzSTATiCzzztSh3PS"
   ; "KT1Tezoo45zzSmartPyzzSTATiCzzzzteuw9"
   ; "KT1Tezoo46zzSmartPyzzSTATiCzzzyu6S2X"
   ; "KT1Tezoo47zzSmartPyzzSTATiCzzzxjfKR8"
   ; "KT1Tezoo48zzSmartPyzzSTATiCzzzxeybmd"
   ; "KT1Tezoo49zzSmartPyzzSTATiCzzzsYs8mm"
   ; "KT1Tezoo5ozzSmartPyzzSTATiCzzzwq7cM4"
   ; "KT1Tezoo51zzSmartPyzzSTATiCzzzsie8hD"
   ; "KT1Tezoo52zzSmartPyzzSTATiCzzzy6jfJn"
   ; "KT1Tezoo53zzSmartPyzzSTATiCzzzpzJxjJ"
   ; "KT1Tezoo54zzSmartPyzzSTATiCzzzsC58yB"
   ; "KT1Tezoo55zzSmartPyzzSTATiCzzzzB9g1d"
   ; "KT1Tezoo56zzSmartPyzzSTATiCzzzvJRMF4"
   ; "KT1Tezoo57zzSmartPyzzSTATiCzzzyRNXjo"
   ; "KT1Tezoo58zzSmartPyzzSTATiCzzzuoQJPC"
   ; "KT1Tezoo59zzSmartPyzzSTATiCzzzxrzq7o"
   ; "KT1Tezoo6ozzSmartPyzzSTATiCzzzxn4BAQ"
   ; "KT1Tezoo61zzSmartPyzzSTATiCzzzsCVUrJ"
   ; "KT1Tezoo62zzSmartPyzzSTATiCzzzzNq6gW"
   ; "KT1Tezoo63zzSmartPyzzSTATiCzzzvo99GZ"
   ; "KT1Tezoo64zzSmartPyzzSTATiCzzzxyJU8a"
   ; "KT1Tezoo65zzSmartPyzzSTATiCzzzzadcFa"
   ; "KT1Tezoo66zzSmartPyzzSTATiCzzzxPC5nQ"
   ; "KT1Tezoo67zzSmartPyzzSTATiCzzzzyq8B1"
   ; "KT1Tezoo68zzSmartPyzzSTATiCzzzxpBPfQ"
   ; "KT1Tezoo69zzSmartPyzzSTATiCzzzsPcuJf"
   ; "KT1Tezoo7ozzSmartPyzzSTATiCzzzxgiesf"
   ; "KT1Tezoo71zzSmartPyzzSTATiCzzzyGZS4Q"
   ; "KT1Tezoo72zzSmartPyzzSTATiCzzzzc5bP5"
   ; "KT1Tezoo73zzSmartPyzzSTATiCzzzzjqD8v"
   ; "KT1Tezoo74zzSmartPyzzSTATiCzzzqjbsH6"
   ; "KT1Tezoo75zzSmartPyzzSTATiCzzzweg7Kr"
   ; "KT1Tezoo76zzSmartPyzzSTATiCzzzzJQexk"
   ; "KT1Tezoo77zzSmartPyzzSTATiCzzzxzCsXF"
   ; "KT1Tezoo78zzSmartPyzzSTATiCzzzx3Yt9E"
   ; "KT1Tezoo79zzSmartPyzzSTATiCzzzy1eJ9s"
   ; "KT1Tezoo8ozzSmartPyzzSTATiCzzzuGszvB"
   ; "KT1Tezoo81zzSmartPyzzSTATiCzzzyn1ZHZ"
   ; "KT1Tezoo82zzSmartPyzzSTATiCzzzyr5ES9"
   ; "KT1Tezoo83zzSmartPyzzSTATiCzzzxUVHDg"
   ; "KT1Tezoo84zzSmartPyzzSTATiCzzzw2LQfK"
   ; "KT1Tezoo85zzSmartPyzzSTATiCzzzrZRBHF"
   ; "KT1Tezoo86zzSmartPyzzSTATiCzzzwwr66A"
   ; "KT1Tezoo87zzSmartPyzzSTATiCzzzx2asuv"
   ; "KT1Tezoo88zzSmartPyzzSTATiCzzzvVdzo9"
   ; "KT1Tezoo89zzSmartPyzzSTATiCzzzxMckJy"
   ; "KT1Tezoo9ozzSmartPyzzSTATiCzzzxX37Py"
   ; "KT1Tezoo91zzSmartPyzzSTATiCzzzyegtLQ"
   ; "KT1Tezoo92zzSmartPyzzSTATiCzzzvtdVek"
   ; "KT1Tezoo93zzSmartPyzzSTATiCzzzznaYBh"
   ; "KT1Tezoo94zzSmartPyzzSTATiCzzzwqPLoM"
   ; "KT1Tezoo95zzSmartPyzzSTATiCzzzrn7SPm"
   ; "KT1Tezoo96zzSmartPyzzSTATiCzzzw2aWmL"
   ; "KT1Tezoo97zzSmartPyzzSTATiCzzzyYc6Fd"
   ; "KT1Tezoo98zzSmartPyzzSTATiCzzzvpPECg"
   ; "KT1Tezoo99zzSmartPyzzSTATiCzzzzYoJJi"
   ; "KT1Tezo1oozzSmartPyzzSTATiCzzzw6i9jd" |]

let dynamic_addresses =
  [| "KT1TezoooozzSmartPyzzDYNAMiCzzpLu4LU"
   ; "KT1Tezooo1zzSmartPyzzDYNAMiCzztcr8AZ"
   ; "KT1Tezooo2zzSmartPyzzDYNAMiCzzxyHfG9"
   ; "KT1Tezooo3zzSmartPyzzDYNAMiCzzvqsJQk"
   ; "KT1Tezooo4zzSmartPyzzDYNAMiCzzywTMhC"
   ; "KT1Tezooo5zzSmartPyzzDYNAMiCzzvwBH3X"
   ; "KT1Tezooo6zzSmartPyzzDYNAMiCzzvyu5w3"
   ; "KT1Tezooo7zzSmartPyzzDYNAMiCzztDqbVQ"
   ; "KT1Tezooo8zzSmartPyzzDYNAMiCzzq2URWu"
   ; "KT1Tezooo9zzSmartPyzzDYNAMiCzzwMosaF"
   ; "KT1Tezoo1ozzSmartPyzzDYNAMiCzzzknqsi"
   ; "KT1Tezoo11zzSmartPyzzDYNAMiCzzufK1yA"
   ; "KT1Tezoo12zzSmartPyzzDYNAMiCzzp8MwtN"
   ; "KT1Tezoo13zzSmartPyzzDYNAMiCzzuFSRii"
   ; "KT1Tezoo14zzSmartPyzzDYNAMiCzzz7RGrK"
   ; "KT1Tezoo15zzSmartPyzzDYNAMiCzzvGSR2o"
   ; "KT1Tezoo16zzSmartPyzzDYNAMiCzzwFpt7J"
   ; "KT1Tezoo17zzSmartPyzzDYNAMiCzzykrvy7"
   ; "KT1Tezoo18zzSmartPyzzDYNAMiCzztCAt8v"
   ; "KT1Tezoo19zzSmartPyzzDYNAMiCzzs7uP4i"
   ; "KT1Tezoo2ozzSmartPyzzDYNAMiCzzwSnrN6"
   ; "KT1Tezoo21zzSmartPyzzDYNAMiCzzs5DfTE"
   ; "KT1Tezoo22zzSmartPyzzDYNAMiCzzrduLfP"
   ; "KT1Tezoo23zzSmartPyzzDYNAMiCzzwZfMMD"
   ; "KT1Tezoo24zzSmartPyzzDYNAMiCzzwjiGTo"
   ; "KT1Tezoo25zzSmartPyzzDYNAMiCzzyT75uA"
   ; "KT1Tezoo26zzSmartPyzzDYNAMiCzzwgevwP"
   ; "KT1Tezoo27zzSmartPyzzDYNAMiCzzuNESHj"
   ; "KT1Tezoo28zzSmartPyzzDYNAMiCzzstgZLG"
   ; "KT1Tezoo29zzSmartPyzzDYNAMiCzzua3qt8"
   ; "KT1Tezoo3ozzSmartPyzzDYNAMiCzzzydbTn"
   ; "KT1Tezoo31zzSmartPyzzDYNAMiCzzzV6Fv1"
   ; "KT1Tezoo32zzSmartPyzzDYNAMiCzztwKPMA"
   ; "KT1Tezoo33zzSmartPyzzDYNAMiCzztAevd2"
   ; "KT1Tezoo34zzSmartPyzzDYNAMiCzzyHyztq"
   ; "KT1Tezoo35zzSmartPyzzDYNAMiCzzzseQRS"
   ; "KT1Tezoo36zzSmartPyzzDYNAMiCzzxG7VFz"
   ; "KT1Tezoo37zzSmartPyzzDYNAMiCzzvanMaF"
   ; "KT1Tezoo38zzSmartPyzzDYNAMiCzzxow7N5"
   ; "KT1Tezoo39zzSmartPyzzDYNAMiCzzseasUf"
   ; "KT1Tezoo4ozzSmartPyzzDYNAMiCzzwCUzLT"
   ; "KT1Tezoo41zzSmartPyzzDYNAMiCzzuKdM6z"
   ; "KT1Tezoo42zzSmartPyzzDYNAMiCzzvoxbXQ"
   ; "KT1Tezoo43zzSmartPyzzDYNAMiCzzxHNrWB"
   ; "KT1Tezoo44zzSmartPyzzDYNAMiCzzqzWSLJ"
   ; "KT1Tezoo45zzSmartPyzzDYNAMiCzzwoma3L"
   ; "KT1Tezoo46zzSmartPyzzDYNAMiCzzwagsDr"
   ; "KT1Tezoo47zzSmartPyzzDYNAMiCzzx9gczA"
   ; "KT1Tezoo48zzSmartPyzzDYNAMiCzzujzDEL"
   ; "KT1Tezoo49zzSmartPyzzDYNAMiCzzyzNqLS"
   ; "KT1Tezoo5ozzSmartPyzzDYNAMiCzzyhNzVP"
   ; "KT1Tezoo51zzSmartPyzzDYNAMiCzztanVCy"
   ; "KT1Tezoo52zzSmartPyzzDYNAMiCzzukPXQf"
   ; "KT1Tezoo53zzSmartPyzzDYNAMiCzzwertzS"
   ; "KT1Tezoo54zzSmartPyzzDYNAMiCzzxrXndJ"
   ; "KT1Tezoo55zzSmartPyzzDYNAMiCzzwZDrJ6"
   ; "KT1Tezoo56zzSmartPyzzDYNAMiCzzz7MLT7"
   ; "KT1Tezoo57zzSmartPyzzDYNAMiCzzx1SNRa"
   ; "KT1Tezoo58zzSmartPyzzDYNAMiCzzspyLrZ"
   ; "KT1Tezoo59zzSmartPyzzDYNAMiCzzpFMM5S"
   ; "KT1Tezoo6ozzSmartPyzzDYNAMiCzzzc7Vf5"
   ; "KT1Tezoo61zzSmartPyzzDYNAMiCzzxQGTz1"
   ; "KT1Tezoo62zzSmartPyzzDYNAMiCzzzCFRxE"
   ; "KT1Tezoo63zzSmartPyzzDYNAMiCzzxSbaFw"
   ; "KT1Tezoo64zzSmartPyzzDYNAMiCzzrZxQjf"
   ; "KT1Tezoo65zzSmartPyzzDYNAMiCzzuE8zrh"
   ; "KT1Tezoo66zzSmartPyzzDYNAMiCzzxL3RNg"
   ; "KT1Tezoo67zzSmartPyzzDYNAMiCzzxDHbQ2"
   ; "KT1Tezoo68zzSmartPyzzDYNAMiCzzv9uuPs"
   ; "KT1Tezoo69zzSmartPyzzDYNAMiCzzziAEo3"
   ; "KT1Tezoo7ozzSmartPyzzDYNAMiCzzssee8N"
   ; "KT1Tezoo71zzSmartPyzzDYNAMiCzzy8o38T"
   ; "KT1Tezoo72zzSmartPyzzDYNAMiCzzvJQgMd"
   ; "KT1Tezoo73zzSmartPyzzDYNAMiCzzzQKejF"
   ; "KT1Tezoo74zzSmartPyzzDYNAMiCzzxGc1Le"
   ; "KT1Tezoo75zzSmartPyzzDYNAMiCzzwS4VD5"
   ; "KT1Tezoo76zzSmartPyzzDYNAMiCzzsu5trk"
   ; "KT1Tezoo77zzSmartPyzzDYNAMiCzzx7fRph"
   ; "KT1Tezoo78zzSmartPyzzDYNAMiCzzubSbwk"
   ; "KT1Tezoo79zzSmartPyzzDYNAMiCzzwzStmF"
   ; "KT1Tezoo8ozzSmartPyzzDYNAMiCzzq35Rq4"
   ; "KT1Tezoo81zzSmartPyzzDYNAMiCzzzQyrWb"
   ; "KT1Tezoo82zzSmartPyzzDYNAMiCzzuk3Zgh"
   ; "KT1Tezoo83zzSmartPyzzDYNAMiCzzzamuiH"
   ; "KT1Tezoo84zzSmartPyzzDYNAMiCzztuhJts"
   ; "KT1Tezoo85zzSmartPyzzDYNAMiCzzsEUusH"
   ; "KT1Tezoo86zzSmartPyzzDYNAMiCzzywXBn3"
   ; "KT1Tezoo87zzSmartPyzzDYNAMiCzzto1Zr6"
   ; "KT1Tezoo88zzSmartPyzzDYNAMiCzzzYK2k5"
   ; "KT1Tezoo89zzSmartPyzzDYNAMiCzzvEXcr3"
   ; "KT1Tezoo9ozzSmartPyzzDYNAMiCzzzhhryG"
   ; "KT1Tezoo91zzSmartPyzzDYNAMiCzzzsUuqY"
   ; "KT1Tezoo92zzSmartPyzzDYNAMiCzzvE65Pi"
   ; "KT1Tezoo93zzSmartPyzzDYNAMiCzzyo94pw"
   ; "KT1Tezoo94zzSmartPyzzDYNAMiCzzz3TFhN"
   ; "KT1Tezoo95zzSmartPyzzDYNAMiCzzuzjFR1"
   ; "KT1Tezoo96zzSmartPyzzDYNAMiCzzsS2Wj8"
   ; "KT1Tezoo97zzSmartPyzzDYNAMiCzzywXC4c"
   ; "KT1Tezoo98zzSmartPyzzDYNAMiCzzuttMz3"
   ; "KT1Tezoo99zzSmartPyzzDYNAMiCzzr8xqaW"
   ; "KT1Tezo1oozzSmartPyzzDYNAMiCzzxmuJG3" |]

let address_of_contract_id contract_id =
  match contract_id with
  | C_static {static_id} -> static_addresses.(static_id)
  | C_dynamic {dynamic_id} -> dynamic_addresses.(dynamic_id)
