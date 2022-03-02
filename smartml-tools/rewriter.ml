(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics
open Untyped
open Utils
open Control

let line_no = []

type step =
  | Proj of int
  | Attr of string
[@@deriving eq]

(* NB Stops at first map lookup *)
let rec path_of_lexpr ~config e =
  let module Printer = (val Printer.get config : Printer.Printer) in
  match e.e with
  | EVar (x, _) -> (x, [])
  | EPrim1 (EProject i, e) ->
      let root, steps = path_of_lexpr ~config e in
      (root, steps @ [Proj i])
  | EPrim1 (EAttr a, e) ->
      let root, steps = path_of_lexpr ~config e in
      (root, steps @ [Attr a])
  | EItem {items} -> path_of_lexpr ~config items
  | _ -> failwith ("path_of_lexpr: " ^ Printer.expr_to_string e)

(* Calculates all paths mentioned in the expression. *)
let all_accesses =
  let module W =
    Writer (struct
      type t = string * step list
    end)
  in
  let open W in
  let open Syntax (W) in
  let extend step (root, steps) = (root, steps @ [step]) in
  let close = Option.cata (return ()) write in
  let fm_expr _line_no = function
    | EVar (x, _) -> return (Some (x, []))
    | EPrim1 (EProject i, e) -> return (Option.map (extend (Proj i)) e)
    | EPrim1 (EAttr a, e) -> return (Option.map (extend (Attr a)) e)
    | e ->
        let* _ =
          sequence_expr_f (Basics.map_expr_f close close id elim_untyped e)
        in
        return None
  in
  let fm_command _line_no = assert false in
  let fm_type _line_no = assert false in
  let alg = {fm_expr; fm_command; fm_type} in
  fun e -> fst (run (cataM_expr alg e >>= close))

(* Check that neither the given path nor any of its prefixes is
   accessed. *)
let does_not_access (root, steps) e =
  let is_ok (root', steps') =
    root' <> root || not (List.is_prefix equal_step steps steps')
  in
  List.for_all is_ok (all_accesses e)

let embellish ~config =
  let attr a e = {e = EPrim1 (EAttr a, e); line_no} in
  let proj i e = {e = EPrim1 (EProject i, e); line_no} in
  let set_proj i lhs rhs = {c = CSetVar (proj i lhs, rhs); line_no} in
  let set_attr a lhs rhs = {c = CSetVar (attr a lhs, rhs); line_no} in
  let f_expr line_no e = {e; line_no} in
  let f_command = function
    | CIf (e, x, {c = CFailwith msg}) ->
        CBind (None, {c = CVerify (e, Some msg); line_no}, x)
    | CSetVar (lhs, rhs) when equal_expr_modulo_line_nos lhs rhs ->
        CResult (Expr.cst ~line_no:[] Literal.unit)
    | CSetVar (lhs, {e = ETuple [x; y]})
      when does_not_access (path_of_lexpr ~config (proj 0 lhs)) y ->
        CBind (None, set_proj 0 lhs x, set_proj 1 lhs y)
    | CSetVar (lhs, {e = ETuple [x; y]})
      when does_not_access (path_of_lexpr ~config (proj 1 lhs)) x ->
        CBind (None, set_proj 1 lhs y, set_proj 0 lhs x)
    | CSetVar (lhs, {e = ERecord [(a0, x0); (a1, x1)]})
      when does_not_access (path_of_lexpr ~config (attr a0 lhs)) x1 ->
        CBind (None, set_attr a0 lhs x0, set_attr a1 lhs x1)
    | CBind (None, {c = CResult {e = EPrim0 (ECst Literal.Unit)}}, {c}) -> c
    | CBind (None, {c}, {c = CResult {e = EPrim0 (ECst Literal.Unit)}}) -> c
    | CSetVar
        ( lhs
        , { e =
              EPrim2
                ( (EBinOp (BAdd | BMul _) as prim2)
                , ({e = EPrim0 (ECst _)} as x)
                , y ) } ) ->
        CSetVar (lhs, {e = EPrim2 (prim2, y, x); line_no})
    | CBind
        ( _
        , {c = CDefineLocal {var = x; rhs = e}}
        , {c = CSetVar (lhs, {e = EVar (x', _)})} )
      when x = x' ->
        CSetVar (lhs, e) (* FIXME invalid if x is used again later *)
    | CBind (Some x, {c = CResult e}, {c = CSetVar (lhs, {e = EVar (x', _)})})
      when x = x' ->
        CSetVar (lhs, e) (* FIXME invalid if x is used again later *)
    | CBind (x2, {c = CBind (x1, e1, e2)}, e3) ->
        CBind (x1, e1, {c = CBind (x2, e2, e3); line_no})
    | CBind (Some x, {c = CMatch (s, clauses)}, c) ->
        let f (cons, arg, rhs) =
          (cons, arg, {c = CBind (Some x, rhs, c); line_no})
        in
        CMatch (s, List.map f clauses)
    | CBind (_, {c = CFailwith _ as fw}, _) -> fw
    | c -> c
  in
  let f_command line_no c = {c = f_command c; line_no} in
  {f_expr; f_command; f_type = id}

let rec fixpoint n f x =
  if n = 0 then failwith "fixpoint: failed to converge";
  let y = f x in
  if equal_command_modulo_line_nos x y then x else fixpoint (n - 1) f y

let embellish ~config = fixpoint 100 (cata_command (embellish ~config))
