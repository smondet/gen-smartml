(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Basics
open Untyped

let unit ~line_no = Expr.cst ~line_no Literal.unit

let open_some ~line_no e = Expr.openVariant ~line_no "Some" e None

let add_op ~line_no x =
  let ops = Expr.operations ~line_no in
  Command.set ~line_no ops (Expr.cons ~line_no x ops)

let add_operations ~line_no xs =
  Command.for_loop
    ~line_no
    "_op"
    xs
    (add_op ~line_no (Expr.variable ~line_no "_op"))

let transfer ~line_no arg amount destination =
  add_op ~line_no (Expr.transfer ~line_no ~arg ~amount ~destination)

let send ~line_no destination amount =
  transfer
    ~line_no
    (unit ~line_no)
    amount
    (open_some ~line_no (Expr.contract ~line_no None Type.unit destination))

let set_delegate ~line_no x = add_op ~line_no (Expr.set_delegate ~line_no x)

let create_contract ~line_no baker balance storage c =
  let op = Expr.create_contract ~line_no ~baker ~balance ~storage c in
  add_op ~line_no (Expr.attr ~name:"operation" ~line_no op)

let tez x = Literal.mutez (Big_int.mult_int_big_int 1000000 x)

let map_function_full_stack ~line_no xs f =
  let f =
    match f.e with
    | ELambda params -> {e = ELambda {params with clean_stack = false}; line_no}
    | _ ->
        print_endline (show_expr f);
        assert false
  in
  Expr.map_function ~line_no xs f
