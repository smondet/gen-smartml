(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics
open Typed
open Printf
open Utils
open Control
open Ternary

module Action = struct
  type state = {constraints : (line_no * typing_constraint) list}

  type 'a t = state -> 'a * state

  let run x s = x s

  let return (x : 'a) : 'a t = fun s -> (x, s)

  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
   fun s ->
    let y, s = x s in
    f y s

  let map f x = bind x (fun x -> return (f x))

  let apply f x = bind f (fun f -> bind x (fun x -> return (f x)))

  let add_constraint line_no c {constraints} =
    ((), {constraints = (line_no, c) :: constraints})
end

include Action
include Monad (Action)

let unresolved_error ~line_no = function
  | AssertEqual _ -> assert false
  | HasSub (e, e1, e2) ->
      [ `Text "Type Error"
      ; `Exprs [e1; e2]
      ; `Text "cannot be subtracted in "
      ; `Expr e
      ; `Line line_no ]
  | HasDiv (e, e1, e2) ->
      [ `Text "Type Error"
      ; `Exprs [e1; e2]
      ; `Text "cannot be divided in "
      ; `Expr e
      ; `Br
      ; `Text "Allowed types are (int|nat, int|nat) and (tez, tez|nat)"
      ; `Line line_no ]
  | HasMap (e, e1, e2) ->
      [ `Text "Type Error"
      ; `Exprs [e1; e2]
      ; `Text "cannot be map in "
      ; `Expr e
      ; `Line line_no ]
  | HasBitArithmetic (e, e1, e2) ->
      [ `Text "Type Error"
      ; `Exprs [e1; e2]
      ; `Text "cannot be xor-ed in "
      ; `Expr e
      ; `Line line_no ]
  | IsComparable e ->
      [`Text "Type error"; `Expr e; `Text "doesn't have a comparable type"]
  | HasGetItem (l, pos, _t) ->
      [`Text "Type Error"; `Expr l; `Text "cannot get item"; `Expr pos]
  | HasContains (items, member, line_no) ->
      [ `Text "Type Error"
      ; `Expr items
      ; `Text "cannot contains"
      ; `Expr member
      ; `Line line_no ]
  | HasSize e ->
      [`Text "Type Error"; `Expr e; `Text "has no length or size"; `Line line_no]
  | HasSlice e ->
      [`Text "Type Error"; `Expr e; `Text "cannot be sliced"; `Line line_no]
  | HasAdd (e, e1, e2) ->
      [ `Text "Type Error"
      ; `Exprs [e1; e2]
      ; `Text "cannot be added in "
      ; `Expr e
      ; `Line line_no ]
  | HasMul (e, e1, e2, _overloaded) ->
      [ `Text "Type Error"
      ; `Exprs [e1; e2]
      ; `Text "cannot be multiplied "
      ; `Expr e
      ; `Line line_no ]
  | IsInt (_, pp) -> pp ()
  | SaplingVerify (state, transaction) ->
      [`Text "memo_size error"; `Exprs [state; transaction]; `Line line_no]
  | HasNeg (e, _) ->
      [`Text "Type Error"; `Expr e; `Text "cannot be negated"; `Line line_no]
  | HasInt e ->
      [`Text "Type Error"; `Expr e; `Text "cannot be cast to Int"; `Line line_no]
  | IsNotHot (n, t) ->
      [ `Text "Variable"
      ; `Text n
      ; `Text "of type"
      ; `Type t
      ; `Text "cannot be used twice because it contains a ticket."
      ; `Line line_no ]
  | IsAnyMap (tk, tv, e) ->
      [ `Expr e
      ; `Text "of type"
      ; `Type e.et
      ; `Text "is not a map from"
      ; `Type tk
      ; `Text "to"
      ; `Type tv
      ; `Line line_no ]
  | IsConvertible (t1, t2) ->
      [ `Type t1
      ; `Text "does not have the same Michelson type as"
      ; `Type t2
      ; `Line line_no ]

let assertEqual ~config ~pp t1 t2 =
  match Unifier.assertEqual ~config t1 t2 with
  | Ok () -> return ()
  | Failed e ->
      raise
        (SmartExcept ((`Text "Type Error" :: `Br :: e) @ [`Br; `Rec (pp ())]))

let is_comparable =
  Type.cata (function
      | TInt _ | TBounded _
       |T0
          ( T_unit | T_never | T_bool | T_string | T_chain_id | T_bytes
          | T_mutez | T_key_hash | T_key | T_signature | T_timestamp | T_address
            ) ->
          Yes
      | T0
          ( T_operation | T_bls12_381_g1 | T_bls12_381_g2 | T_bls12_381_fr
          | T_chest | T_chest_key )
       |T1 ((T_list | T_set | T_contract | T_ticket), _)
       |T2 ((T_map | T_big_map), _, _)
       |TLambda _ | TSaplingState _ | TSaplingTransaction _ | TSecretKey ->
          No
      | TUnknown {contents = UExact c} -> c
      | TVar _ | TUnknown {contents = UUnknown _} -> Maybe
      | ( TTuple _ | TRecord _ | TVariant _
        | T1 (T_option, _)
        | TUnknown {contents = URecord _ | UTuple _ | UVariant _} ) as t ->
          Type.fold_f and_ Yes t
      | T0 (T_nat | T_int | T_sapling_transaction _ | T_sapling_state _)
       |T2 ((T_pair _ | T_or _ | T_lambda), _, _) ->
          assert false)

let apply_constraint ~config line_no c =
  let pp () = unresolved_error ~line_no c in
  let unresolved = add_constraint line_no c in
  match c with
  | AssertEqual (t1, t2, pp) -> assertEqual ~config ~pp t1 t2
  | HasSub (e, e1, _e2) ->
    ( match Type.getRepr e1.et with
    | T0 T_mutez -> assertEqual ~config ~pp e1.et e.et
    | TInt _ -> assertEqual ~config ~pp e.et (Type.int ())
    | T0 T_timestamp -> assertEqual ~config ~pp e.et (Type.int ())
    | TUnknown _ -> unresolved
    | _ -> raise (SmartExcept (pp ())) )
  | HasDiv (e, e1, e2) ->
      let fix_cst_intOrNat_as_nat x =
        match (x.e, Type.getRepr x.et) with
        | EPrim0 (ECst _), TInt {isNat} ->
          ( match Unknown.get isNat with
          | None ->
              let* () = assertEqual ~config x.et (Type.nat ()) ~pp in
              return (Some true)
          | x -> return x )
        | _, TInt {isNat} -> return (Unknown.get isNat)
        | _ -> return None
      in
      let* a_b =
        match (Type.getRepr e1.et, Type.getRepr e2.et) with
        | _, T0 T_mutez ->
            let* () = assertEqual ~config ~pp e1.et Type.token in
            return (`OK (Type.nat (), Type.token))
        | T0 T_mutez, TInt _ ->
            let* () = assertEqual ~config ~pp e2.et (Type.nat ()) in
            return (`OK (Type.token, Type.token))
        | TInt _, TInt _ ->
            let* r1 = fix_cst_intOrNat_as_nat e1 in
            let* r2 = fix_cst_intOrNat_as_nat e2 in
            ( match (r1, r2) with
            | Some true, Some true -> return (`OK (Type.nat (), Type.nat ()))
            | Some false, Some true
             |Some true, Some false
             |Some false, Some false ->
                return (`OK (Type.int (), Type.nat ()))
            | None, _ | _, None -> return `Unknown )
        | _ -> raise (SmartExcept (pp ()))
      in
      ( match a_b with
      | `Unknown -> unresolved
      | `OK (a, b) ->
          let target_type = Type.option (Type.pair a b) in
          let pp () =
            [ `Text "Type Error"
            ; `Expr e
            ; `Text "is not compatible with type"
            ; `Type target_type
            ; `Br
            ; `Line line_no ]
          in
          assertEqual ~config ~pp e.et target_type )
  | HasMap (e, l, f) ->
    ( match Type.getRepr f.et with
    | TLambda ({with_storage; with_operations}, s', t') ->
      ( match (Unknown.get with_storage, Unknown.get with_operations) with
      | Some (Some _), _ | _, Some true ->
          raise
            (SmartExcept
               [ `Text "Type Error"
               ; `Text "Cannot map effectful lambda"
               ; `Line line_no ])
      | None, _ | _, None -> unresolved
      | _ ->
        ( match Type.getRepr l.et with
        | T1 (T_list, t) ->
            let* () = assertEqual ~config ~pp t s' in
            assertEqual ~config ~pp e.et (Type.list t')
        | T2 (T_map, tkey, tvalue) ->
            let* () = assertEqual ~config ~pp (Type.key_value tkey tvalue) s' in
            assertEqual ~config ~pp e.et (Type.map ~big:false ~tkey ~tvalue:t')
        | TUnknown _ -> unresolved
        | _ -> raise (SmartExcept (pp ())) ) )
    | T2 (T_lambda, _, _) -> assert false
    | _ -> raise (SmartExcept (pp ())) )
  | HasBitArithmetic (e, e1, _e2) ->
    ( match Type.getRepr e1.et with
    | T0 T_bool -> return ()
    | TInt _ -> assertEqual ~config ~pp e.et (Type.nat ())
    | TUnknown _ -> unresolved
    | _ -> raise (SmartExcept (pp ())) )
  | IsComparable e ->
    ( match is_comparable e.et with
    | Yes -> return ()
    | No -> raise (SmartExcept (pp ()))
    | Maybe -> unresolved )
  | HasGetItem (l, pos, t) ->
    ( match Type.getRepr l.et with
    | T2 ((T_map | T_big_map), tkey, tvalue) ->
        let* () = assertEqual ~config ~pp tkey pos.et in
        assertEqual ~config ~pp tvalue t
    | T1 (T_list, _) ->
        let pp () =
          [ `Rec (pp ())
          ; `Br
          ; `Text "A list is not a map."
          ; `Br
          ; `Text
              (sprintf
                 " You can use:\n\
                 \ - sp.utils.vector(..) to create a map from a list,\n\
                 \ - sp.utils.matrix(..) to create a map of maps from a list \
                  of lists,\n\
                 \ - sp.utils.cube(..) for a list of lists of lists.") ]
        in
        raise (SmartExcept (pp ()))
    | TUnknown _ -> unresolved
    | _ -> raise (SmartExcept (pp ())) )
  | HasContains (items, member, _line_no) ->
    ( match Type.getRepr items.et with
    | T1 (T_set, telement) -> assertEqual ~config ~pp telement member.et
    | T2 ((T_map | T_big_map), tkey, _) ->
        assertEqual ~config ~pp tkey member.et
    | TUnknown _ -> unresolved
    | _ -> raise (SmartExcept (pp ())) )
  | HasSize e ->
    ( match Type.getRepr e.et with
    | T0 (T_nat | T_int | T_sapling_transaction _ | T_sapling_state _) ->
        assert false
    | T1 (T_list, _) | T0 (T_string | T_bytes) | T1 (T_set, _) -> return ()
    | T2 (T_map, _, _) -> return ()
    | TUnknown _ -> unresolved
    | _ -> raise (SmartExcept (pp ())) )
  | HasSlice e ->
    ( match Type.getRepr e.et with
    | T0 (T_string | T_bytes) | TBounded _ -> return ()
    | TUnknown _ -> unresolved
    | _ -> raise (SmartExcept (pp ())) )
  | HasAdd (_e, e1, _e2) ->
    ( match Type.getRepr e1.et with
    | TInt _
     |T0
        ( T_mutez | T_string | T_bytes | T_bls12_381_g1 | T_bls12_381_g2
        | T_bls12_381_fr ) ->
        return ()
    | TUnknown _ -> unresolved
    | _ -> raise (SmartExcept (pp ())) )
  | HasMul (e, e1, e2, overloaded) ->
      let integer_mul e e1 e2 =
        let* () = assertEqual ~config e.et (Type.intOrNat ()) ~pp in
        let* () = assertEqual ~config e1.et (Type.intOrNat ()) ~pp in
        let* () = assertEqual ~config e2.et (Type.intOrNat ()) ~pp in
        match (Type.getRepr e.et, Type.getRepr e1.et, Type.getRepr e2.et) with
        | TInt {isNat}, TInt {isNat = isNat1}, TInt {isNat = isNat2} ->
          begin
            match
              (Unknown.get isNat, Unknown.get isNat1, Unknown.get isNat2)
            with
            | _, Some true, _ -> assertEqual ~config e.et e2.et ~pp
            | _, _, Some true -> assertEqual ~config e.et e1.et ~pp
            | _, Some false, _ | _, _, Some false ->
                assertEqual ~config e.et (Type.int ()) ~pp
            | Some true, None, None ->
                let* () = assertEqual ~config e1.et (Type.nat ()) ~pp in
                assertEqual ~config e2.et (Type.nat ()) ~pp
            | (Some false | None), None, None -> unresolved
          end
        | _ -> assert false
      in
      ( match (Type.getRepr e.et, Type.getRepr e1.et, Type.getRepr e2.et) with
      | TInt _, _, _ ->
          if not overloaded
          then return () (* we know that all types are equal *)
          else integer_mul e e1 e2
      | _, TInt _, TInt _ -> integer_mul e e1 e2
      | _, TInt _, T0 T_mutez ->
          let* () = assertEqual ~config e1.et (Type.nat ()) ~pp in
          assertEqual ~config e.et Type.token ~pp
      | _, T0 T_mutez, TInt _ ->
          let* () = assertEqual ~config e2.et (Type.nat ()) ~pp in
          assertEqual ~config e.et Type.token ~pp
      | _, TInt _, T0 T_bls12_381_fr | _, T0 T_bls12_381_fr, TInt _ ->
          assertEqual ~config e.et Type.bls12_381_fr ~pp
      | ( _
        , T0 (T_bls12_381_g1 | T_bls12_381_g2 | T_bls12_381_fr)
        , T0 T_bls12_381_fr )
       |T0 (T_bls12_381_g1 | T_bls12_381_g2), _, T0 T_bls12_381_fr ->
          assertEqual ~config e.et e1.et ~pp
      | _, T0 (T_bls12_381_g1 | T_bls12_381_g2), _ ->
          let* () = assertEqual ~config e.et e1.et ~pp in
          assertEqual ~config e2.et Type.bls12_381_fr ~pp
      | TUnknown _, _, _ -> unresolved
      | _ -> raise (SmartExcept (pp ())) )
  | IsInt (t, pp) ->
    ( match Type.getRepr t with
    | TInt _ -> return ()
    | TUnknown _ -> unresolved
    | _ -> raise (SmartExcept (pp ())) )
  | SaplingVerify (state, transaction) ->
    begin
      match (Type.getRepr state.et, Type.getRepr transaction.et) with
      | TSaplingState {memo = m1}, TSaplingTransaction {memo = m2} ->
          let r = Unknown.equalize m1 m2 in
          if not r then raise (SmartExcept (pp ()));
          ( match (Unknown.get m1, Unknown.get m2) with
          | Some m1, Some m2 when m1 = m2 -> return ()
          | Some _, Some _ -> raise (SmartExcept (pp ()))
          | _ -> unresolved )
      | _ -> raise (SmartExcept (pp ()))
    end
  | HasNeg (e, t) ->
    ( match (Type.getRepr t, Type.getRepr e.et) with
    | T0 (T_bls12_381_g1 | T_bls12_381_g2 | T_bls12_381_fr), _
     |_, T0 (T_bls12_381_g1 | T_bls12_381_g2 | T_bls12_381_fr) ->
        assertEqual ~config t e.et ~pp
    | TInt _, _ | _, TInt _ ->
        let* () = assertEqual ~config e.et (Type.intOrNat ()) ~pp in
        assertEqual ~config t (Type.int ()) ~pp
    | _, TUnknown _ -> unresolved
    | _ -> raise (SmartExcept (pp ())) )
  | HasInt e ->
      let pp () =
        [ `Text "Type Error"
        ; `Expr e
        ; `Text "cannot be cast to Int"
        ; `Line e.line_no ]
      in
      ( match Type.getRepr e.et with
      | TInt _ -> assertEqual ~config e.et (Type.nat ()) ~pp
      | T0 T_bls12_381_fr -> return ()
      | TUnknown _ -> unresolved
      | _ -> raise (SmartExcept (pp ())) )
  | IsNotHot (_, t) ->
    ( match Type.is_hot t with
    | No -> return ()
    | Yes -> raise (SmartExcept (pp ()))
    | Maybe -> unresolved )
  | IsAnyMap (tk, tv, e) ->
    ( match Type.getRepr e.et with
    | T2 ((T_map | T_big_map), tkey, tvalue) ->
        let* () = assertEqual ~config tk tkey ~pp in
        let* () = assertEqual ~config tv tvalue ~pp in
        return ()
    | TUnknown _ -> unresolved
    | _ -> raise (SmartExcept (pp ())) )
  | IsConvertible (t1, t2) ->
      let t1 = Type.(F (getRepr t1)) in
      let t2 = Type.(F (getRepr t2)) in
      if Type.has_unknowns t1 || Type.has_unknowns t2
      then unresolved
      else
        let t1 = Typing.mtype_of_type ~with_annots:false t1 in
        let t2 = Typing.mtype_of_type ~with_annots:false t2 in
        if Michelson.equal_mtype t1 t2
           (* NB We could call Michelson.unifiable_types and propagate the
              resulting substitution here. *)
        then return ()
        else raise (SmartExcept (pp ()))

let default_constraint ~config line_no c =
  let pp () = unresolved_error ~line_no c in
  let unresolved = add_constraint line_no c in
  match c with
  | IsAnyMap (tkey, tvalue, e) ->
      assertEqual ~config ~pp e.et (Type.map ~big:false ~tkey ~tvalue)
  | _ -> unresolved

let show_constraint =
  let open Type in
  function
  | HasAdd (_e, e1, e2) -> Format.asprintf "HasAdd %a, %a" pp e1.et pp e2.et
  | HasMul (_e, e1, e2, _) -> Format.asprintf "HasMul %a, %a" pp e1.et pp e2.et
  | HasSub (_e, e1, e2) -> Format.asprintf "HasSub %a, %a" pp e1.et pp e2.et
  | HasDiv (_e, e1, e2) -> Format.asprintf "HasDiv %a, %a" pp e1.et pp e2.et
  | HasBitArithmetic (_e, e1, e2) ->
      Format.asprintf "HasBitArithmetic %a, %a" pp e1.et pp e2.et
  | HasMap (_e, e1, e2) -> Format.asprintf "HasMap %a, %a" pp e1.et pp e2.et
  | IsComparable e -> Format.asprintf "IsComparable %a" pp e.et
  | HasGetItem (e1, e2, _) ->
      Format.asprintf "HasGetItem %a, %a" pp e1.et pp e2.et
  | HasContains (e1, e2, _line_no) ->
      Format.asprintf "HasContains %a, %a" pp e1.et pp e2.et
  | HasSize e -> Format.asprintf "HasSize %a" pp e.et
  | HasSlice e -> Format.asprintf "HasSlice %a" pp e.et
  | AssertEqual (t1, t2, _pp) ->
      Format.asprintf "AssertEqual %a, %a" pp t1 pp t2
  | IsInt (t, _pp) -> Format.asprintf "IsInt %a" pp t
  | SaplingVerify (state, transaction) ->
      Format.asprintf "SaplingVerify %a %a" pp state.et pp transaction.et
  | HasNeg (e, t) -> Format.asprintf "HasNeg %a %a" pp e.et pp t
  | HasInt e -> Format.asprintf "HasInt %a" pp e.et
  | IsNotHot (n, t) -> Format.asprintf "HasInt %s %a" n pp t
  | IsAnyMap (tk, tv, e) ->
      Format.asprintf "IsAnyMap %a, %a, %a" pp tk pp tv pp e.et
  | IsConvertible (t1, t2) -> Format.asprintf "IsConvertible %a, %a" pp t1 pp t2

let run ~config constraints =
  let verbosity = 0 in
  let debug v f = if verbosity >= v then f () in
  let step f (line_no, c) =
    let (), {constraints} = run (f ~config line_no c) {constraints = []} in
    constraints
  in
  let eq (l1, c1) (l2, c2) = l1 = l2 && equal_typing_constraint c1 c2 in
  let progress cs cs' =
    List.length cs <> List.length cs' || not (List.for_all2 eq cs cs')
  in
  let rec pass i = function
    | [] -> []
    | cs when i > 100 -> cs
    | cs ->
        debug 2 (fun () ->
            List.iter (fun (_, c) -> printf "    %s\n" (show_constraint c)) cs);
        debug 1 (fun () -> printf "  Pass #%d...\n" i);
        let cs' = List.concat_map (step apply_constraint) cs in
        let p = progress cs cs' in
        debug 3 (fun () -> printf "    Progress: %b\n" p);
        if p
        then pass (i + 1) cs'
        else
          let cs'' = List.concat_map (step default_constraint) cs in
          let p = progress cs' cs'' in
          debug 3 (fun () -> printf "    Progress (defaulting): %b\n" p);
          if p then pass (i + 1) cs'' else cs''
  in
  debug 1 (fun () -> print_endline "Constraint resolution...");
  let cs = pass 1 (List.rev constraints) in
  let mandatory = function
    | _, IsComparable _ -> false
    | _, IsNotHot _ -> false
    | _ -> true
  in
  ( match List.find_opt mandatory cs with
  | None -> ()
  | Some (line_no, c) ->
      raise
        (SmartExcept
           ( `Text "Missing typing information"
           :: `Br
           :: unresolved_error ~line_no c )) );
  debug 1 (fun () -> print_endline "All constraints resolved.")
