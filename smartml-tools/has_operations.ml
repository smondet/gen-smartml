(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Basics
open Typed

(* TODO recognize ELambda with effects *)
type t =
  | HO_none
  | HO_at_most_one
  | HO_many
[@@deriving show {with_path = false}, eq, ord]

let or_ ho1 ho2 =
  match (ho1, ho2) with
  | HO_none, x | x, HO_none -> x
  | HO_at_most_one, x | x, HO_at_most_one -> x
  | _ -> HO_many

let add ho1 ho2 =
  match (ho1, ho2) with
  | HO_none, x | x, HO_none -> x
  | _ -> HO_many

let widen = function
  | HO_none -> HO_none
  | _ -> HO_many

let has_operations =
  let p_tcommand _line_no _ct = function
    | CIf (_, x, y) -> or_ (snd x) (snd y)
    | CMatch (_, cases) ->
        List.fold_left
          (fun acc (_, _, body) -> or_ acc (snd body))
          HO_none
          cases
    | CMatchCons {ok_match; ko_match} -> or_ (snd ok_match) (snd ko_match)
    | CSetVar (({e = EVar ("__operations__", _)}, _), rhs) ->
      ( match (fst rhs).e with
      | EList [] -> HO_none
      | EPrim2 (ECons, _, {e = EVar ("__operations__", _)}) -> HO_at_most_one
      | _ -> HO_many )
    | CBind (_, c1, c2) -> add (snd c1) (snd c2)
    | CFor (_, e, c) | CWhile (e, c) -> add (snd e) (widen (snd c))
    | ( CMatchProduct _ | CModifyProduct _ | CSetVar _ | CSetResultType _
      | CComment _ | CSetType _ | CResult _ | CUpdateSet _ | CDelItem _
      | CDefineLocal _ | CNever _ | CVerify _ | CFailwith _ | CTrace _
      | CSetEntryPoint _ ) as c ->
        let c = map_command_f snd snd snd c in
        fold_command_f add add (fun x _ -> x) HO_none c
  in
  let p_texpr _line_no _et = function
    | EPrim2 (ECallLambda, _, ({et}, _)) ->
      ( match Type.getRepr et with
      | TLambda ({with_operations}, _, _) ->
        ( match Unknown.get with_operations with
        | Some false -> HO_none
        | Some true -> HO_many
        | None -> assert false )
      | _ -> assert false )
    | e ->
        let e = map_expr_f snd snd snd (fun x -> x) e in
        fold_expr_f add add (fun x _ -> x) (fun x _ -> x) HO_none e
  in
  let p_ttype _ = () in
  para_tcommand (para_talg ~p_texpr ~p_tcommand ~p_ttype)
