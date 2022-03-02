(* Copyright 2019-2021 Smart Chain Arena LLC. *)

(** {1 Commands} *)

open Basics
open Untyped

(**

Commands represent actions than happen in a smart contract.
They can carry expressions that perform computations.

Commands are built by calling the functions defined in this module
 or by using the SmartML PPX syntax.

{2 Direct Commands}
Commands can be built directly by using the functions in this module.
This is a complex and tedious process that should only be used by the
more adventurous developers.

{2 PPX Syntax}
This is the intended syntax for most circumstances.
This is both very convenient and powerful.

{3 Examples, Commands in Entry Points}

{[
  let%entry_point double () =
  self.data.storedValue <- self.data.storedValue * 2;
]}

{[
  let%entry_point divide params =
    set_type params { divisor = int };
    verify (params.divisor > 5) "params.divisor is too small";
    self.data.storedValue <- self.data.storedValue / params.divisor
]}

{2 Types}
Commands in this module are untyped, which means that they do not carry
 any type information. They are typed, by {!SmartML}, in a later stage, typically in a {!Basics.tscenario}.

 *)

(** The type of commands. *)
type t = command

(** {2 Control} *)

val ifte : line_no:line_no -> expr -> t -> t -> t
(** Conditional command.

In a SmartML PPX context,
{[ if _condition_ then _then_consequence_ else _else_consequence_ ]}
 *)

val ifteSome : line_no:line_no -> Expr.t -> t -> t -> t

val mk_match : line_no:line_no -> expr -> (string * string * t) list -> t
(** Pattern matching of constructors.

In a SmartML PPX context,
{[ match _expression_ with
   | _constr_ _var -> ...
   | ... ]}
 *)

val for_loop : line_no:line_no -> string -> expr -> t -> t
(** A for loop.

In a SmartML PPX context,
{[ List.iter (fun _pat_ -> _command_) _expression_ ]}
or
{[ List.iter _command_ _expression_ ]}

 *)

val while_loop : line_no:line_no -> expr -> t -> t
(** A while_loop.

In a SmartML PPX context,
{[ while _condition_ do _command_ done ]}
 *)

val seq : line_no:line_no -> t list -> t
(** A sequence of commands.

In a SmartML PPX context,
{[ _command1_ ; _command2_; ... ]}
 *)

val mk_match_cons : line_no:line_no -> expr -> string -> t -> t -> t

val mk_match_product : line_no:line_no -> expr -> pattern -> t -> t

val mk_modify_product : line_no:line_no -> expr -> pattern -> t -> t

(** {2 Exceptions, Assertions, Never} *)

val sp_failwith : line_no:line_no -> expr -> t
(** Raise an exception with the corresponding expression.

In a SmartML PPX context,
{[ failwith _expression_ ]}

As in Michelson, {!failwith} can be applied to an expression of any type.
 *)

val verify : line_no:line_no -> expr -> expr option -> t
(** Dynamic verification of a condition with associated error message.

In a SmartML PPX context,
{[ verify _condition_ _error_message_ ]}

As for {!failwith}, the [_error_message_] is an expression of any type.
 *)

val never : line_no:line_no -> expr -> t
(** Closing of a never branch.

In a SmartML PPX context,
{[ never _condition_ ]}
 *)

(** {2 Setting Values} *)

val delItem : line_no:line_no -> expr -> expr -> t

val updateSet : line_no:line_no -> expr -> expr -> bool -> t

val set : line_no:line_no -> expr -> expr -> t

(** {2 Binding and Local Variables} *)

val define_local : line_no:line_no -> string -> expr -> bool -> t

val bind : line_no:line_no -> string option -> t -> t -> t

(** {2 Return Values} *)

val result : line_no:line_no -> expr -> t

val set_result_type : line_no:line_no -> t -> Type.t -> t

val set_type : line_no:line_no -> expr -> Type.t -> t
(** Type constraint.

In a SmartML PPX context,
{[ set_type _expression_ _type_ ]}
 *)

(** {2 Misc} *)

val trace : line_no:line_no -> expr -> t
(** Trace an expression in interpreted tests.

In a SmartML PPX context,
{[ trace _expression_ ]}
 *)

val comment : line_no:line_no -> string -> t

val set_entry_point : line_no:line_no -> string -> expr -> t
