(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Basics
open Printf
open Utils
open Control

type 'a unification_result =
  | Ok     of 'a
  | Failed of smart_except list

module Action = struct
  type _ t =
    | Fail      : smart_except list -> 'a t
    | GetConfig : Config.t t
    | Return    : 'a -> 'a t
    | Bind      : 'a t * ('a -> 'b t) -> 'b t

  let fail e = Fail e

  let get_config = GetConfig

  let run (config : Config.t) =
    let rec run : type a. a t -> a unification_result = function
      | Fail e -> Failed e
      | Return x -> Ok x
      | Bind (x, f) ->
        ( match run x with
        | Ok x -> run (f x)
        | Failed e -> Failed e )
      | GetConfig -> Ok config
    in
    run

  let return x = Return x

  let bind x f = Bind (x, f)

  let map f x = bind x (fun x -> return (f x))

  let apply f x = bind f (fun f -> bind x (fun x -> return (f x)))
end

include Action
include Monad (Action)

let equalize ~pp r1 r2 =
  let r = Unknown.equalize r1 r2 in
  if not r then fail (pp ()) else return ()

let fail_if c e = if c then fail e else return ()

let bounded ~pp cases1 cases2 =
  let rec merge acc l1 l2 =
    match (l1, l2) with
    | l, [] -> List.rev acc @ l
    | a :: l1, b :: l2 when Literal.equal a b -> merge (a :: acc) l1 l2
    | a :: l1, (b :: _ as l2) when Literal.compare a b < 0 ->
        merge (a :: acc) l1 l2
    | l1, b :: l2 -> merge (b :: acc) l1 l2
  in
  let merge cases1 cases2 =
    let l = merge [] cases1.Type.cases cases2.Type.cases in
    let final1 = cases1.final in
    let final2 = cases2.final in
    if (final1 && List.length l <> List.length cases1.Type.cases)
       || (final2 && List.length l <> List.length cases2.Type.cases)
    then None
    else Some Type.{final = final1 || final2; cases = l}
  in
  let eq = Type.equal_bounded_cases Literal.equal in
  let r = Unknown.equalize_merge eq merge cases1 cases2 in
  if not r then fail (pp ()) else return ()

let assertEqual_layout ~pp l1 l2 =
  let* config = get_config in
  let module Printer = (val Printer.get config : Printer.Printer) in
  equalize
    ~pp:(fun () ->
      [ `Text "Non matching layouts"
      ; `Br
      ; `Text (Printer.layout_to_string l1)
      ; `Br
      ; `Text "and"
      ; `Br
      ; `Text (Printer.layout_to_string l2)
      ; `Br
      ; `Rec (pp ()) ])
    l1
    l2

let rec assertEqual t1 t2 =
  match (Type.getRepr t1, Type.getRepr t2) with
  | T0 (T_nat | T_int | T_sapling_transaction _ | T_sapling_state _), _
   |_, T0 (T_nat | T_int | T_sapling_transaction _ | T_sapling_state _) ->
      assert false
  | T0 c, T0 c' when Michelson_base.Type.equal_type0 c c' -> return ()
  | T1 (c, t1), T1 (c', t1') when Michelson_base.Type.equal_type1 c c' ->
      assertEqual t1 t1'
  | T2 (c, t1, t2), T2 (c', t1', t2') when Michelson_base.Type.equal_type2 c c'
    ->
      let* () = assertEqual t1 t1' in
      assertEqual t2 t2'
  | TBounded {t = t1; cases = cases1}, TBounded {t = t2; cases = cases2} ->
      let* () = assertEqual t1 t2 in
      bounded ~pp:(fun () -> [`Text "bounded mismatch"]) cases1 cases2
  | TSecretKey, TSecretKey -> return ()
  | TSaplingTransaction {memo = m1}, TSaplingTransaction {memo = m2}
   |TSaplingState {memo = m1}, TSaplingState {memo = m2} ->
      equalize ~pp:(fun () -> [`Text "memo_size mismatch"]) m1 m2
  | TInt {isNat = isNat1}, TInt {isNat = isNat2} when isNat1 == isNat2 ->
      return ()
  | TInt {isNat = isNat1}, TInt {isNat = isNat2} ->
      equalize
        ~pp:(fun () -> [`Text "Type sp.TInt / sp.TNat mismatch"])
        isNat1
        isNat2
  | TUnknown t1, TUnknown t2 when t1 == t2 -> return ()
  | TUnknown ({contents = UTuple l1} as r1), TTuple l2 ->
      let checkInside (i, t) =
        match List.nth_opt l2 i with
        | None ->
            fail
              [ `Text "Missing component"
              ; `Text (sprintf "%d" i)
              ; `Text "in type"
              ; `Type t2 ]
        | Some t2 -> assertEqual t t2
      in
      let* () = iter_list checkInside l1 in
      r1 := UExact t2;
      return ()
  | TUnknown ({contents = UVariant l1} as r1), TVariant {row = l2}
   |TUnknown ({contents = URecord l1} as r1), TRecord {row = l2} ->
      let checkInside (name, t) =
        match List.assoc_opt name l2 with
        | None ->
            fail
              [ `Text "Missing field or variant"
              ; `Text (sprintf "'%s'" name)
              ; `Text "in type"
              ; `Type t2 ]
        | Some t2 -> assertEqual t t2
      in
      let* () = iter_list checkInside l1 in
      r1 := UExact t2;
      return ()
  | T2 ((T_map | T_big_map), _, _), TUnknown _
   |T1 (T_set, _), TUnknown _
   |TRecord _, TUnknown _
   |TTuple _, TUnknown _
   |TVariant _, TUnknown _ ->
      assertEqual t2 t1
  | TUnknown ({contents = UUnknown _} as r), _ ->
      r := UExact t2;
      return ()
  | _, TUnknown ({contents = UUnknown _} as r) ->
      r := UExact t1;
      return ()
  | ( TUnknown ({contents = UTuple l1_} as r1)
    , TUnknown ({contents = UTuple l2_} as r2) ) ->
      let rec acc l1 l2 =
        match (l1, l2) with
        | [], [] -> return []
        | [], x | x, [] -> return x
        | a :: l1, (b :: _ as l2) when fst a < fst b ->
            let* r = acc l1 l2 in
            return (a :: r)
        | (a :: _ as l1), b :: l2 when fst b < fst a ->
            let* r = acc l1 l2 in
            return (b :: r)
        | (a, t1_) :: l1, (_b, t2_) :: l2 ->
            let* () = assertEqual t1_ t2_ in
            let* r = acc l1 l2 in
            return ((a, t1_) :: r)
      in
      let* l = acc (List.sort compare l1_) (List.sort compare l2_) in
      r1 := UTuple l;
      r2 := UExact t1;
      return ()
  | ( TUnknown ({contents = URecord l1_} as r1)
    , TUnknown ({contents = URecord l2_} as r2) ) ->
      let rec acc l1 l2 =
        match (l1, l2) with
        | [], [] -> return []
        | [], x | x, [] -> return x
        | a :: l1, (b :: _ as l2) when fst a < fst b ->
            let* r = acc l1 l2 in
            return (a :: r)
        | (a :: _ as l1), b :: l2 when fst b < fst a ->
            let* r = acc l1 l2 in
            return (b :: r)
        | (a, t1_) :: l1, (_b, t2_) :: l2 ->
            let* () = assertEqual t1_ t2_ in
            let* r = acc l1 l2 in
            return ((a, t1_) :: r)
      in
      let* l = acc (List.sort compare l1_) (List.sort compare l2_) in
      r1 := URecord l;
      r2 := UExact t1;
      return ()
  | ( TUnknown ({contents = UVariant l1_} as r1)
    , TUnknown ({contents = UVariant l2_} as r2) ) ->
      let rec acc l1 l2 =
        match (l1, l2) with
        | [], [] -> return []
        | [], x | x, [] -> return x
        | a :: l1, (b :: _ as l2) when fst a < fst b ->
            let* r = acc l1 l2 in
            return (a :: r)
        | (a :: _ as l1), b :: l2 when fst b < fst a ->
            let* r = acc l1 l2 in
            return (b :: r)
        | (a, t1_) :: l1, (b, t2_) :: l2 ->
            assert (a = b);
            let* () = assertEqual t1_ t2_ in
            let* r = acc l1 l2 in
            return ((a, t1_) :: r)
      in
      let* l = acc (List.sort compare l1_) (List.sort compare l2_) in
      r1 := UVariant l;
      r2 := UExact t1;
      return ()
  | TLambda (es1, t11, t12), TLambda (es2, t21, t22) ->
      if Unknown.equalize es1.with_storage es2.with_storage
         && Unknown.equalize es1.with_operations es2.with_operations
      then
        let* () = assertEqual t11 t21 in
        assertEqual t12 t22
      else fail [`Text "Lambdas have different effects"]
  | TTuple ts1, TTuple ts2 ->
      if List.length ts1 = List.length ts2
      then iter2_list assertEqual ts1 ts2
      else fail [`Text "Tuples differ in lengths:"; `Type t1; `Type t2]
  | T1 (T_ticket, t1), T1 (T_ticket, t2) -> assertEqual t1 t2
  | T1 (T_list, t1), T1 (T_list, t2) -> assertEqual t1 t2
  | TRecord {layout = lo1; row = l1_}, TRecord {layout = lo2; row = l2_} ->
      let rec acc l1 l2 =
        match (l1, l2) with
        | [], [] -> return ()
        | [], (a, _) :: _ | (a, _) :: _, [] ->
            fail
              [ `Text (sprintf "missing field %s in record" a)
              ; `Type t1
              ; `Type t2 ]
        | (a, t1_) :: l1, (b, t2_) :: l2 ->
            let* () =
              fail_if
                (a <> b)
                [ `Text
                    (sprintf "non matching names [%s] and [%s] in records" a b)
                ; `Br
                ; `Type t1
                ; `Br
                ; `Type t2 ]
            in
            let* () = assertEqual t1_ t2_ in
            acc l1 l2
      in
      let* () = acc (List.sort compare l1_) (List.sort compare l2_) in
      let pp_layout () =
        [`Text (sprintf "Different layouts for types"); `Type t1; `Br; `Type t2]
      in
      assertEqual_layout ~pp:pp_layout lo1 lo2
  | T1 (T_set, t1), T1 (T_set, t2) -> assertEqual t1 t2
  | TVariant {layout = lo1; row = l1}, TVariant {layout = lo2; row = l2} ->
      let pp_layout () =
        [`Text (sprintf "Different layouts for types"); `Type t1; `Br; `Type t2]
      in
      let* () = assertEqual_layout ~pp:pp_layout lo1 lo2 in
      let rec acc l1 l2 =
        match (l1, l2) with
        | [], [] -> return ()
        | [], _ | _, [] ->
            fail
              [ `Text "Incompatible variant types:"
              ; `Br
              ; `Type t1
              ; `Br
              ; `Text "and"
              ; `Br
              ; `Type t2 ]
        | (a, t1) :: l1, (b, t2) :: l2 ->
            let* () =
              fail_if
                (a <> b)
                [ `Text
                    (sprintf
                       "non matching names [%s] and [%s] in variant \
                        constructors"
                       a
                       b)
                ; `Type t1
                ; `Text "and"
                ; `Type t2 ]
            in
            let* () = assertEqual t1 t2 in
            acc l1 l2
      in
      acc (List.sort compare l1) (List.sort compare l2)
  | _ -> fail [`Type t1; `Text "is not"; `Type t2]

let assertEqual ~config t1 t2 = run config (assertEqual t1 t2)
