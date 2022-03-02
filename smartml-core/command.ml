(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Basics
open Untyped

type t = command

let build ~line_no c = {c; line_no}

let ifte ~line_no c t e = build ~line_no (CIf (c, t, e))

let ifteSome ~line_no c t e =
  ifte ~line_no (Expr.isVariant ~line_no ~name:"Some" c) t e

let mk_match ~line_no scrutinee cases =
  build ~line_no (CMatch (scrutinee, cases))

let mk_match_cons ~line_no expr id ok_match ko_match =
  build ~line_no (CMatchCons {expr; id; ok_match; ko_match})

let mk_match_product ~line_no s p c = build ~line_no (CMatchProduct (s, p, c))

let mk_modify_product ~line_no s p c = build ~line_no (CModifyProduct (s, p, c))

let sp_failwith ~line_no message = build ~line_no (CFailwith message)

let never ~line_no message = build ~line_no (CNever message)

let verify ~line_no e message = build ~line_no (CVerify (e, message))

let for_loop ~line_no name e c = build ~line_no (CFor (name, e, c))

let while_loop ~line_no e l = build ~line_no (CWhile (e, l))

let delItem ~line_no x y = build ~line_no (CDelItem (x, y))

let updateSet ~line_no x y add = build ~line_no (CUpdateSet (x, y, add))

let rec set ~line_no x y =
  match x.e with
  | EVar ("__operations__", _) -> build ~line_no (CSetVar (x, y))
  | EVar ("__storage__", _) | EItem _ | EVar _ | EPrim1 (EAttr _, _) ->
    ( match y.e with
    | EPrim3
        ( EUpdate_map
        , key
        , {e = EPrim1 (EVariant "None", {e = EPrim0 (ECst Unit)})}
        , map )
      when x = map ->
        delItem ~line_no x key
    | EPrim3 (EUpdate_map, key, {e = EPrim1 (EVariant "Some", v)}, map)
      when x = map ->
        set ~line_no (Expr.item ~line_no map key None None) v
    | _ -> build ~line_no (CSetVar (x, y)) )
  | _ ->
      raise
        (Basics.SmartExcept
           [ `Text "Syntax Error"
           ; `Br
           ; `Expr_untyped x
           ; `Text "is not a variable"
           ; `Line line_no ])

let define_local ~line_no var rhs is_mutable =
  build ~line_no (CDefineLocal {var; rhs; is_mutable})

let rec bind ~line_no x c1 c2 =
  match c1.c with
  | CBind (y, c1a, c1b) -> bind ~line_no y c1a (bind ~line_no x c1b c2)
  | _ -> build ~line_no (CBind (x, c1, c2))

let result ~line_no x = build ~line_no (CResult x)

let rec seq ~line_no commands =
  let line_no =
    match (line_no, commands) with
    | [], c :: _ -> c.line_no
    | _ -> line_no
  in
  match commands with
  | [] -> result ~line_no (Expr.cst ~line_no Literal.unit)
  | [x] -> x
  | {c = CResult {e = EPrim0 (ECst Literal.Unit)}} :: xs -> seq ~line_no xs
  | x :: xs -> bind ~line_no None x (seq ~line_no xs)

let comment ~line_no s = build ~line_no (CComment s)

let set_result_type ~line_no c t = build ~line_no (CSetResultType (c, t))

let set_type ~line_no e t = build ~line_no (CSetType (e, t))

let trace ~line_no x = build ~line_no (CTrace x)

let set_entry_point ~line_no n e = build ~line_no (CSetEntryPoint (n, e))
