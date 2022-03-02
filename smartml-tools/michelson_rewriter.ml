(* Copyright 2019-2021 Smart Chain Arena LLC. *)

(** Michelson-to-michelson code simplification. *)

open SmartML
open Utils
open Control
open Michelson

let check_rest_invariant = false

let mk_instr instr = {instr}

let un_instr {instr} = instr

let seq xs = MIseq xs

let seqi xs = MIseq (List.map mk_instr xs)

let iseq xs = mk_instr (seq xs)

let iseqi xs = iseq (List.map mk_instr xs)

let to_seq = function
  | MIseq is -> List.map un_instr is
  | i -> [i]

let of_seq = function
  | [i] -> i
  | is -> MIseq (List.map mk_instr is)

type rule =
     (instr, literal) instr_f list
  -> ((instr, literal) instr_f list * (instr, literal) instr_f list) option

(** All rules in a group are tried in order until a normal form has been reached. *)
type group = rule list

(** The groups in a pipeline are run sequentially. *)
type pipeline = group list

let ( $ ) xs rest = Some (xs, rest)

let rewrite_none = None

(** {1 Rule helpers} *)

let cAr = MIfield [A]

let cDr = MIfield [D]

(** {1 Our concrete rule sets} *)

(** Does the instruction operate only on the top of the stack? *)

let rec fails {instr} =
  match instr with
  | MI1_fail _ -> true
  | MIseq xs ->
    ( match Base.List.last xs with
    | Some x -> fails x
    | None -> false )
  | MIif (l, r) | MIif_left (l, r) | MIif_none (l, r) | MIif_cons (l, r) ->
      fails l && fails r
  | MImap x | MIiter x | MIloop x | MIdip x | MIdipn (_, x) -> fails x
  | _ -> false

(** Does the instruction push something on top of the stack, without
   modifying or looking at anything beneath? *)
let is_pure_push = function
  | MIpush _ | MIlambda _
   |MI0
      ( Nil _ | Sender | Amount | Now | None_ _ | Unit_ | Empty_set _
      | Empty_map _ | Empty_bigmap _ | Source | Balance | Self _ | Self_address
        )
   |MI1_fail Never ->
      true
  | _ -> false

(** Does the instruction push something on top of the stack, without
   modying anything beneath? *)
let is_pushy = function
  | MIdup _ -> true
  | x -> is_pure_push x

let rec may_fail = function
  | MI2 Exec | MIerror _ | MImich _ | MI1_fail _ | MI2 (View _) -> true
  | MI2 (Lsl | Lsr | Add | Sub | Mul) -> true (* overflow on some types *)
  | MIseq l -> List.exists (fun x -> may_fail x.instr) l
  | MIif (i1, i2) | MIif_cons (i1, i2) | MIif_none (i1, i2) | MIif_left (i1, i2)
    ->
      may_fail i1.instr || may_fail i2.instr
  | MIdip i | MIdipn (_, i) | MIloop i | MIloop_left i | MIiter i | MImap i ->
      may_fail i.instr
  | MIcomment _ | MIdrop | MIdropn _ | MIdup _ | MIdig _ | MIdug _
   |MI0
      ( Sender | Source | Amount | Balance | Level | Now | Self _ | Self_address
      | Chain_id | Total_voting_power | Sapling_empty_state _ | Unit_ | None_ _
      | Nil _ | Empty_set _ | Empty_map _ | Empty_bigmap _ )
   |MI1
      ( Car | Cdr | Some_ | Eq | Abs | Neg | Int | IsNat | Neq | Le | Lt | Ge
      | Gt | Not | Concat1 | Size | Address | Implicit_account | Pack | Hash_key
      | Blake2b | Sha256 | Sha512 | Keccak | Sha3 | Set_delegate | Read_ticket
      | Join_tickets | Pairing_check | Voting_power | Left _ | Right _
      | Contract _ | Unpack _ | Getn _ | Cast _ | Rename _ )
   |MI2
      ( Pair _ | Xor | Ediv | And | Or | Cons | Compare | Concat2 | Get | Mem
      | Apply | Sapling_verify_update | Ticket | Split_ticket | Updaten _ )
   |MI3
      ( Slice | Update | Get_and_update | Transfer_tokens | Check_signature
      | Open_chest )
   |MIswap | MIpush _ | MIunpair _ | MIpairn _ | MIfield _ | MIsetField _
   |MIlambda _ | MIcreate_contract _ | MIconcat_unresolved ->
      false

let may_diverge instr =
  let f_instr = function
    | MI2 Exec | MIloop _ | MIloop_left _ -> true
    | MIcreate_contract _ -> false
    | e -> fold_instr_f ( || ) (curry fst) false e
  in
  let f_literal _ = () in
  cata_instr {f_instr; f_literal} {instr}

let harmless i = not (may_fail i || may_diverge i)

let pushify_all : rule = function
  | MI0 (Nil t) :: rest -> [MIpush (mt_list t, MLiteral.list [])] $ rest
  | MI0 (Empty_bigmap (k, v)) :: rest ->
      [MIpush (mt_big_map k v, MLiteral.mk_map [])] $ rest
  | MIlambda (t1, t2, body) :: rest ->
      [MIpush (mt_lambda t1 t2, MLiteral.instr body)] $ rest
  | _ -> rewrite_none

let unfold_macros : rule = function
  (* Unfold nullary data constructors: *)
  | MI0 (Nil t) :: rest when not (equal_mtype t mt_operation) ->
      [MIpush (mt_list t, MLiteral.list [])] $ rest
  | MI0 (None_ t) :: rest -> [MIpush (mt_option t, MLiteral.none)] $ rest
  | MI0 Unit_ :: rest -> [MIpush (mt_unit, MLiteral.unit)] $ rest
  | MI0 (Empty_set t) :: rest -> [MIpush (mt_set t, MLiteral.set [])] $ rest
  | MI0 (Empty_map (k, v)) :: rest ->
      [MIpush (mt_map k v, MLiteral.mk_map [])] $ rest
  | MIswap :: rest -> [MIdig 1] $ rest
  (* Unfold SET_C[AD]+R: *)
  | MIsetField [A] :: rest -> [cDr; MIdig 1; MI2 (Pair (None, None))] $ rest
  | MIsetField [D] :: rest -> [cAr; MI2 (Pair (None, None))] $ rest
  | MIsetField (A :: ops) :: rest ->
      [ MIunpair [true; true]
      ; MIdig 1
      ; MIsetField ops
      ; MIdig 1
      ; MI2 (Pair (None, None)) ]
      $ rest
  | MIsetField (D :: ops) :: rest ->
      [MIunpair [true; true]; MIsetField ops; MI2 (Pair (None, None))] $ rest
  | MIpairn 2 :: rest -> [MI2 (Pair (None, None))] $ rest
  | MIdropn n :: rest ->
      let rec aux acc = function
        | 0 -> acc
        | n ->
            if n < 0
            then [MIerror "DROP n with n negative"]
            else aux (MIdrop :: acc) (n - 1)
      in
      aux [] n $ rest
  | _ -> rewrite_none

let unfold_unpair : rule = function
  | MIunpair [true; true] :: MIswap :: rest ->
      [MIdup 1; MIfield [A]; MIswap; MIfield [D]] $ rest
  | MIunpair [true; true] :: rest ->
      [MIdup 1; MIfield [D]; MIswap; MIfield [A]] $ rest
  | MIfield op1 :: MIfield op2 :: rest -> [MIfield (op1 @ op2)] $ rest
  | _ -> rewrite_none

let unfold_mifield : rule = function
  | MIfield (f :: (_ :: _ as op)) :: rest -> [MIfield [f]; MIfield op] $ rest
  | MI1 (Getn 1) :: rest -> [MIfield [A]] $ rest
  | MI1 (Getn 2) :: rest -> [MIfield [D]] $ rest
  | _ -> rewrite_none

let extract_unpair_drops : rule = function
  | MIunpair fields :: rest when List.exists not fields ->
      let length = List.length fields in
      let size = unpair_size fields in
      if size = length
      then rewrite_none
      else
        let rec drop acc i = function
          | [] -> List.concat (List.rev acc)
          | true :: rest -> drop acc (i + 1) rest
          | false :: rest -> drop ([MIdig i; MIdrop] :: acc) i rest
        in
        let fields' = List.replicate length true in
        if fields = fields'
        then rewrite_none
        else MIunpair fields' :: drop [] 0 fields $ rest
  | _ -> rewrite_none

let unfold_dupn : rule = function
  | MIdup n :: rest when n > 1 -> [MIdig (n - 1); MIdup 1; MIdug n] $ rest
  | MIdig 1 :: rest -> [MIswap] $ rest
  | _ -> rewrite_none

let fold_dupn : rule = function
  | MIdig n1 :: MIdup 1 :: MIdug n2 :: rest when n2 = n1 + 1 ->
      [MIdup (n1 + 1)] $ rest
  | _ -> rewrite_none

let fold_getn : rule = function
  | MIfield [D] :: MIfield [A] :: rest -> [MI1 (Getn 3)] $ rest
  | MIfield [D] :: MIfield [D] :: rest -> [MI1 (Getn 4)] $ rest
  | MIfield [D] :: MI1 (Getn n) :: rest -> [MI1 (Getn (n + 2))] $ rest
  | MI1 (Getn k) :: MI1 (Getn n) :: rest when Int.rem k 2 = 0 ->
      [MI1 (Getn (k + n))] $ rest
  | _ -> rewrite_none

let fold_self_address : rule = function
  | MI0 (Self None) :: MI1 Address :: rest -> [MI0 Self_address] $ rest
  | _ -> rewrite_none

let unfold_self_address : rule = function
  | MI0 Self_address :: rest -> [MI0 (Self None); MI1 Address] $ rest
  | _ -> rewrite_none

let fold_macros_etc : rule = function
  (* Fold nullary data constructors: *)
  | MIpush ({mt = MT1 (T_list, t)}, {literal = Seq []}) :: rest ->
      [MI0 (Nil t)] $ rest
  | MIpush ({mt = MT1 (T_set, t)}, {literal = Seq []}) :: rest ->
      [MI0 (Empty_set t)] $ rest
  | MIpush ({mt = MT2 (T_map, k, v)}, {literal = AnyMap []}) :: rest ->
      [MI0 (Empty_map (k, v))] $ rest
  | MIpush ({mt = MT2 (T_big_map, k, v)}, {literal = AnyMap []}) :: rest ->
      [MI0 (Empty_bigmap (k, v))] $ rest
  | MIpush ({mt = MT1 (T_option, t)}, {literal = None_}) :: rest ->
      [MI0 (None_ t)] $ rest
  | MIpush ({mt = MT0 T_unit}, {literal = Unit}) :: rest -> [MI0 Unit_] $ rest
  | MIpush ({mt = MT2 (T_lambda, t1, t2)}, {literal = Instr body}) :: rest ->
      [MIlambda (t1, t2, body)] $ rest
  (* PUSH-PUSH and PUSH-DUP appear to have the same cost gas-wise,
     but for the latter storage is cheaper: *)
  | MIpush (t1, l1) :: MIpush (t2, l2) :: rest
    when equal_mtype t1 t2 && equal_literal l1 l2 ->
      [MIpush (t1, l1); MIdup 1] $ rest
  | MIdrop :: MIdrop :: rest -> [MIdropn 2] $ rest
  | MIdrop :: MIdropn n :: rest | MIdropn n :: MIdrop :: rest ->
      [MIdropn (n + 1)] $ rest
  | MIdropn m :: MIdropn n :: rest -> [MIdropn (m + n)] $ rest
  | MIdig 1 :: rest -> [MIswap] $ rest
  | _ -> rewrite_none

let is_fail = function
  | MIseq [{instr = MIpush _}; {instr = MI1_fail Failwith}] -> true
  | _ -> false

let is_pair_fail = function
  | MIseq [{instr = MI2 (Pair _)}; {instr = MI1_fail Failwith}] -> true
  | _ -> false

let cond_check_last cond x y rest =
  match (List.rev (to_seq x.instr), List.rev (to_seq y.instr)) with
  | (MIpush _ as i1) :: x, i2 :: y when equal_instr {instr = i1} {instr = i2} ->
      [cond (iseqi (List.rev x)) (iseqi (List.rev y)); i1] $ rest
  | (MIpush _ as i) :: x, MI1_fail Failwith :: _ ->
      [cond (iseqi (List.rev x)) y; i] $ rest
  | MI1_fail Failwith :: _, (MIpush _ as i) :: y ->
      [cond x (iseqi (List.rev y)); i] $ rest
  | _ -> rewrite_none

let replay_if_like if_like i1 i2 =
  match if_like with
  | MIif _ -> MIif (i1, i2)
  | MIif_cons _ -> MIif_cons (i1, i2)
  | MIif_none _ -> MIif_none (i1, i2)
  | MIif_left _ -> MIif_left (i1, i2)
  | _ -> assert false

let has_prefix_drop = function
  | MIdrop | MIseq ({instr = MIdrop} :: _) -> true
  | _ -> false

let remove_prefix_drop = function
  | MIdrop -> seq []
  | MIseq ({instr = MIdrop} :: i) -> seq i
  | _ -> assert false

(* OCaml's mod can return negative numbers, so let's fix that. *)
let pos_mod k n = ((k mod n) + n) mod n

let dig_dug ~with_comments n =
  let rec f shift = function
    | MIdig n' :: rest when n = n' -> f (shift + 1) rest
    | MIdug n' :: rest when n = n' -> f (shift - 1) rest
    | MIswap :: rest when n = 1 -> f (shift + 1) rest
    | MIcomment _ :: ((MIdig n' | MIdug n') :: _ as rest)
      when with_comments && n = n' ->
        f shift rest
    | MIcomment _ :: (MIswap :: _ as rest) when with_comments && n = 1 ->
        f shift rest
    | rest ->
        let digs = pos_mod shift (n + 1) in
        let dugs = n + 1 - digs in
        let instrs =
          if digs <= dugs
          then List.init digs (fun _ -> MIdig n)
          else List.init dugs (fun _ -> MIdug n)
        in
        (instrs, rest)
  in
  fun x ->
    let y, rest = f 0 x in
    let consumed = List.take (List.length x - List.length rest) x in
    if List.equal equal_instr (List.map mk_instr consumed) (List.map mk_instr y)
    then rewrite_none
    else y $ rest

let conditionals xs =
  match List.map (map_instr_f un_instr id) xs with
  | MIif
      ( MIseq ({instr = MIdig n} :: {instr = MIdrop} :: xs)
      , MIseq ({instr = MIdig n'} :: {instr = MIdrop} :: ys) )
    :: rest
    when n = n' && n >= 1 ->
      [MIdig (n + 1); MIdrop; MIif (seq xs, seq ys)] $ rest
  | MIif
      ( MIseq ({instr = MIdig 2} :: {instr = MIdrop} :: xs)
      , MIseq
          ({instr = MIdig 1}
          :: {instr = MIdrop} :: {instr = MIdig 1} :: {instr = MIdrop} :: ys) )
    :: rest ->
      [ MIdig 3
      ; MIdrop
      ; MIif (seq xs, seq ({instr = MIdig 1} :: {instr = MIdrop} :: ys)) ]
      $ rest
  (* min / max *)
  | MIif
      ( MIseq [{instr = MIdrop}; {instr = MIdig n}; {instr = MIdrop}]
      , MIseq
          [ {instr = MIdig 1}
          ; {instr = MIdrop}
          ; {instr = MIdig n'}
          ; {instr = MIdrop} ] )
    :: rest
    when n = n' && n > 1 ->
      [MIdig (n + 2); MIdrop; MIif (seqi [MIdrop], seqi [MIdig 1; MIdrop])]
      $ rest
  | MIif (MIseq [], MIseq []) :: rest
   |MIif_none (MIseq [], MIdrop) :: rest
   |MIif_left (MIdrop, MIdrop) :: rest
   |MIif_cons (MIseq [{instr = MIdrop}; {instr = MIdrop}], MIseq []) :: rest ->
      [MIdrop] $ rest
  | MI1 Not :: MIif (a, b) :: rest -> [MIif (b, a)] $ rest
  | MIif
      ( MIpush ({mt = MT0 T_bool}, {literal = Bool true})
      , MIpush ({mt = MT0 T_bool}, {literal = Bool false}) )
    :: rest ->
      [] $ rest
  | MIif
      ( MIpush ({mt = MT0 T_bool}, {literal = Bool false})
      , MIpush ({mt = MT0 T_bool}, {literal = Bool true}) )
    :: rest ->
      [MI1 Not] $ rest
  | MIif ((MIpush ({mt = MT0 T_bool}, {literal = Bool _}) as x), y)
    :: MI1 Not :: rest
   |MIif (x, (MIpush ({mt = MT0 T_bool}, {literal = Bool _}) as y))
    :: MI1 Not :: rest ->
      [MIif (seqi [x; MI1 Not], seqi [y; MI1 Not])] $ rest
  | ( ( MIif (i1, i2)
      | MIif_left (i1, i2)
      | MIif_none (i1, i2)
      | MIif_cons (i1, i2) ) as if_like )
    :: MIdrop :: rest ->
      [replay_if_like if_like (seqi [i1; MIdrop]) (seqi [i2; MIdrop])] $ rest
  | ( ( MIif (i1, i2)
      | MIif_left (i1, i2)
      | MIif_none (i1, i2)
      | MIif_cons (i1, i2) ) as if_like )
    :: MIdig n :: MIdrop :: rest ->
      [ replay_if_like
          if_like
          (seqi [i1; MIdig n; MIdrop])
          (seqi [i2; MIdig n; MIdrop]) ]
      $ rest
  | (MIif (MIseq ({instr = MIdig n} :: {instr = MIdrop} :: i1), i2) as if_like)
    :: rest
    when is_fail i2 && n >= 1 ->
      [MIdig (n + 1); MIdrop; replay_if_like if_like (seq i1) i2] $ rest
  | (MIif (i2, MIseq ({instr = MIdig n} :: {instr = MIdrop} :: i1)) as if_like)
    :: rest
    when is_fail i2 && n >= 1 ->
      [MIdig (n + 1); MIdrop; replay_if_like if_like i2 (seq i1)] $ rest
  | ( MIif_left (i2, MIseq ({instr = MIdig n} :: {instr = MIdrop} :: i1)) as
    if_like )
    :: rest
    when is_fail i2 && n >= 1 ->
      [MIdig n; MIdrop; replay_if_like if_like i2 (seq i1)] $ rest
  | ( MIif_none
        ( i2
        , MIseq
            ({instr = MIdrop} :: {instr = MIdig n} :: {instr = MIdrop} :: i1) )
    as if_like )
    :: rest
    when is_fail i2 && n >= 1 ->
      [ MIdig (n + 1)
      ; MIdrop
      ; replay_if_like if_like i2 (seq ({instr = MIdrop} :: i1)) ]
      $ rest
  | ( MIif_none (i2, MIseq ({instr = MIdig n} :: {instr = MIdrop} :: i1)) as
    if_like )
    :: rest
    when is_pair_fail i2 && n >= 3 ->
      [MIdig n; MIdrop; replay_if_like if_like i2 (seq i1)] $ rest
  | ( MIif_none (i1, MIseq ({instr = MIdig n} :: {instr = MIdrop} :: i2)) as
    if_like )
    :: rest
    when is_fail i1 && n >= 1 ->
      [MIdig n; MIdrop; replay_if_like if_like i1 (seq i2)] $ rest
  | ( ( MIif_left (MIseq ({instr = MIdig n} :: {instr = MIdrop} :: i1), i2)
      | MIif_cons (MIseq ({instr = MIdig n} :: {instr = MIdrop} :: i1), i2) ) as
    if_like )
    :: rest
    when is_fail i2 && n >= 1 ->
      let n =
        match if_like with
        | MIif_cons _ -> n - 1
        | _ -> n
      in
      [MIdig n; MIdrop; replay_if_like if_like (seq i1) i2] $ rest
  | ( MIif_left
        ( MIseq ({instr = MIdig 1} :: {instr = MIdrop} :: i1)
        , MIseq ({instr = MIdrop} :: {instr = MIdrop} :: i2) ) as if_like )
    :: rest ->
      [ MIdig 1
      ; MIdrop
      ; replay_if_like if_like (seq i1) (seq ({instr = MIdrop} :: i2)) ]
      $ rest
  | ( ( MIif (MIseq ({instr = MIdrop} :: i1), MIseq ({instr = MIdrop} :: i2))
      | MIif_left
          ( MIseq ({instr = MIdig 1} :: {instr = MIdrop} :: i1)
          , MIseq ({instr = MIdig 1} :: {instr = MIdrop} :: i2) )
      | MIif_none
          ( MIseq ({instr = MIdrop} :: i1)
          , MIseq ({instr = MIdig 1} :: {instr = MIdrop} :: i2) )
      | MIif_cons
          ( MIseq ({instr = MIdig 2} :: {instr = MIdrop} :: i1)
          , MIseq ({instr = MIdrop} :: i2) ) ) as if_like )
    :: rest ->
      [MIdig 1; MIdrop; replay_if_like if_like (seq i1) (seq i2)] $ rest
  (* drop | fail *)
  | (MIif (i1, i2) as if_like) :: rest when has_prefix_drop i1 && is_fail i2 ->
      [MIdig 1; MIdrop; replay_if_like if_like (remove_prefix_drop i1) i2]
      $ rest
  (* fail | drop *)
  | (MIif (i1, i2) as if_like) :: rest when has_prefix_drop i2 && is_fail i1 ->
      [MIdig 1; MIdrop; replay_if_like if_like i1 (remove_prefix_drop i2)]
      $ rest
  | ( ( MIif_cons
          ( (MIseq ({instr = MIdrop} :: {instr = MIdrop} :: _) as i1)
          , ((MIdrop | MIseq ({instr = MIdrop} :: _)) as i2) )
      | MIif
          ( ((MIdrop | MIseq ({instr = MIdrop} :: _)) as i1)
          , ((MIdrop | MIseq ({instr = MIdrop} :: _)) as i2) )
      | MIif_none
          ( ((MIdrop | MIseq ({instr = MIdrop} :: _)) as i1)
          , (MIseq ({instr = MIdrop} :: {instr = MIdrop} :: _) as i2) )
      | MIif_left
          ( (MIseq ({instr = MIdrop} :: {instr = MIdrop} :: _) as i1)
          , (MIseq ({instr = MIdrop} :: {instr = MIdrop} :: _) as i2) ) ) as
    if_like )
    :: rest ->
      [ MIdig 1
      ; MIdrop
      ; replay_if_like if_like (remove_prefix_drop i1) (remove_prefix_drop i2)
      ]
      $ rest
  | MIif_left
      ( MIseq
          [ {instr = MIdrop}
          ; {instr = MIpush ({mt = MT0 T_bool}, {literal = Bool b1})} ]
      , MIseq
          [ {instr = MIdrop}
          ; {instr = MIpush ({mt = MT0 T_bool}, {literal = Bool b2})} ] )
    :: MIif (x, y) :: rest
    when b1 = not b2 ->
      [ MIif_left
          ( seqi [MIdrop; (if b1 then x else y)]
          , seqi [MIdrop; (if b1 then y else x)] ) ]
      $ rest
  | MIif_none
      ( MIpush ({mt = MT0 T_bool}, {literal = Bool b2})
      , MIseq
          [ {instr = MIdrop}
          ; {instr = MIpush ({mt = MT0 T_bool}, {literal = Bool b1})} ] )
    :: MIif (x, y) :: rest
    when b1 = not b2 ->
      [MIif_none ((if b1 then y else x), seqi [MIdrop; (if b1 then x else y)])]
      $ rest
  | MIif_none (b, MIseq [{instr = MIdrop}; {instr = MIdrop}]) :: rest
    when is_fail b ->
      [MIdig 1; MIdrop; MIif_none (b, MIdrop)] $ rest
  | _ -> rewrite_none

let conditionals : rule =
 fun x ->
  let f = List.map (map_instr_f mk_instr id) in
  Option.map (fun (x, y) -> (f x, f y)) (conditionals x)

let remove_comments : pipeline =
  [ [ (function
      | MIcomment _ :: rest -> [] $ rest
      | _ -> rewrite_none) ] ]

let is_iter_cons = function
  | MIiter {instr = MI2 Cons} -> true
  | MIiter {instr = MIseq [{instr = MIcomment _}; {instr = MI2 Cons}]} -> true
  | _ -> false

let main : rule =
  let open Big_int in
  function
  | MIcomment a :: MIcomment b :: rest ->
      let remove_double =
        let rec aux acc = function
          | a :: b :: rest when a = b -> aux acc (b :: rest)
          | a :: rest -> aux (a :: acc) rest
          | [] -> List.rev acc
        in
        aux []
      in
      [MIcomment (remove_double (a @ b))] $ rest
  (* Flatten sequences: *)
  | MIseq is :: rest -> List.map un_instr is $ rest
  (* Superfluous SWAP: *)
  | MIdup 1 :: MIdig 1 :: rest -> [MIdup 1] $ rest
  | p1 :: p2 :: MIdig 1 :: rest when is_pure_push p1 && is_pure_push p2 ->
      [p2; p1] $ rest
  | i :: (MIpush _ :: MI1_fail Failwith :: _ as rest) when not (may_fail i) ->
      [] $ rest
  | i :: MIdrop :: rest when is_pushy i && harmless i -> [] $ rest
  | i :: MIdrop :: rest when has_arity (1, 1) i && harmless i -> [MIdrop] $ rest
  | i :: MIdrop :: rest when has_arity (2, 1) i && harmless i ->
      [MIdrop; MIdrop] $ rest
  | i :: MIdrop :: MIdrop :: rest when has_arity (2, 1) i ->
      [MIdig 2; MIdrop; i; MIdrop] $ rest
  | i :: MIdrop :: MIdig n :: MIdrop :: rest when has_arity (2, 1) i ->
      [MIdig (n + 2); MIdrop; i; MIdrop] $ rest
  | i :: MIdrop :: rest when has_arity (3, 1) i && harmless i ->
      [MIdrop; MIdrop; MIdrop] $ rest
  (* Remove DIPs: *)
  | MIdip {instr = MIdrop} :: rest -> [MIdig 1; MIdrop] $ rest
  | MIdip {instr = MIseq []} :: rest -> [] $ rest
  | MIdip i1 :: MIdip i2 :: rest -> [MIdip (iseq [i1; i2])] $ rest
  | MIdup 1 :: MIdip {instr} :: rest when has_arity (1, 1) instr ->
      [MIdup 1; instr; MIdig 1] $ rest
  (* Push literals: *)
  | MIpush (t, l) :: MI1 Some_ :: rest ->
      [MIpush (mt_option t, MLiteral.some l)] $ rest
  | MIpush (tl, x) :: MI1 (Left (annot_left, annot_right, tr)) :: rest ->
      [MIpush (mt_or ?annot_left ?annot_right tl tr, MLiteral.left x)] $ rest
  | MIpush (tr, x) :: MI1 (Right (annot_left, annot_right, tl)) :: rest ->
      [MIpush (mt_or ?annot_left ?annot_right tl tr, MLiteral.right x)] $ rest
  | MIpush (t2, l2)
    :: MIpush (t1, l1) :: MI2 (Pair (annot_fst, annot_snd)) :: rest ->
      [MIpush (mt_pair ?annot_fst ?annot_snd t1 t2, MLiteral.pair l1 l2)] $ rest
  | MIpush (t2, l2)
    :: (MIcomment _ as c)
       :: MIpush (t1, l1) :: MI2 (Pair (annot_fst, annot_snd)) :: rest ->
      [c; MIpush (mt_pair ?annot_fst ?annot_snd t1 t2, MLiteral.pair l1 l2)]
      $ rest
  | MIpush ({mt = MT2 (T_pair _, fst, _)}, {literal = Pair (x, _)})
    :: MIfield (A :: l) :: rest ->
      [MIpush (fst, x); MIfield l] $ rest
  | MIpush ({mt = MT2 (T_pair _, _, snd)}, {literal = Pair (_, x)})
    :: MIfield (D :: l) :: rest ->
      [MIpush (snd, x); MIfield l] $ rest
  | MI0 (Nil _) :: MIpush (ts, l1) :: MI2 Cons :: rest ->
      [MIpush (mt_list ts, MLiteral.list [l1])] $ rest
  | MI0 (Nil _) :: MIdig 1 :: MI0 (Nil _) :: MIdig 1 :: i1 :: i2 :: rest
    when is_iter_cons i1 && is_iter_cons i2 ->
      [] $ rest
  | MIpush (ts, {literal = Seq xs}) :: MIpush (_, l1) :: MI2 Cons :: rest ->
      [MIpush (ts, MLiteral.list (l1 :: xs))] $ rest
  | MIpush ({mt = MT0 T_bool}, {literal = Bool b}) :: MI1 Not :: rest ->
      [MIpush (mt_bool, MLiteral.bool (not b))] $ rest
  | MIpush ({mt = MT0 T_bool}, {literal = Bool b1})
    :: MIpush ({mt = MT0 T_bool}, {literal = Bool b2}) :: MI2 And :: rest ->
      [MIpush (mt_bool, MLiteral.bool (b1 && b2))] $ rest
  | MIpush ({mt = MT0 T_bool}, {literal = Bool b2})
    :: MIpush ({mt = MT0 T_bool}, {literal = Bool b1}) :: MI2 Or :: rest ->
      [MIpush (mt_bool, MLiteral.bool (b1 || b2))] $ rest
  (* Pairs *)
  | MI2 (Pair _) :: MIfield (D :: l) :: rest -> [MIdrop; MIfield l] $ rest
  | MI2 (Pair _) :: (MIcomment _ as com) :: MIfield [D] :: rest ->
      [MIdrop; com] $ rest
  | MI2 (Pair _) :: MIfield (A :: l) :: rest ->
      [MIdig 1; MIdrop; MIfield l] $ rest
  | MI2 (Pair _) :: (MIcomment _ as c) :: MIfield (A :: l) :: rest ->
      [MIdig 1; MIdrop; c; MIfield l] $ rest
  | MIfield op1 :: MIfield op2 :: rest -> [MIfield (op1 @ op2)] $ rest
  | MIfield [] :: rest -> [] $ rest
  (* LOOPS: *)
  | MIpush (_, {literal = Bool false}) :: MIloop _ :: rest -> [] $ rest
  | MI1 (Right _) :: MIloop_left _ :: rest -> [] $ rest
  (* DIP after DUP: *)
  | MIdup 1 :: MIdip {instr = MIdup 1} :: rest -> [MIdup 1; MIdup 1] $ rest
  | MIdup 1 :: MIdip {instr = MIdrop} :: rest -> [] $ rest
  (* Commutative operations: *)
  | MIdig 1 :: comBin :: rest when is_commutative comBin -> [comBin] $ rest
  | MIdig 1 :: MI2 Compare :: MI1 Eq :: rest -> [MI2 Compare; MI1 Eq] $ rest
  | MIdig 1 :: MI2 Compare :: MI1 Neq :: rest -> [MI2 Compare; MI1 Neq] $ rest
  | MI1 Eq :: MI1 Not :: rest -> [MI1 Neq] $ rest
  | MI1 Neq :: MI1 Not :: rest -> [MI1 Eq] $ rest
  | MIdig 1 :: MI2 Compare :: MI1 Lt :: rest -> [MI2 Compare; MI1 Gt] $ rest
  | MIdig 1 :: MI2 Compare :: MI1 Gt :: rest -> [MI2 Compare; MI1 Lt] $ rest
  (* Bubble up DROP: *)
  | push :: MIdig 1 :: MIdrop :: rest when is_pure_push push ->
      [MIdrop; push] $ rest
  | MIdig 1 :: MIdrop :: MIdrop :: rest -> [MIdrop; MIdrop] $ rest
  | MIdip i :: MIdrop :: rest -> [MIdrop; i.instr] $ rest
  (* Bubble up DIP: *)
  | mono :: MIdip i :: rest when has_arity (1, 1) mono -> [MIdip i; mono] $ rest
  | p :: MIdip {instr} :: rest when is_pure_push p -> [instr; p] $ rest
  (* Bubble up SWAP: *)
  | p :: MIdig 1 :: mono :: rest when has_arity (1, 1) mono && is_pure_push p ->
      [mono; p; MIdig 1] $ rest
  | m1 :: MIdig 1 :: m2 :: MIdig 1 :: rest
    when has_arity (1, 1) m1 && has_arity (1, 1) m2 ->
      [MIdig 1; m2; MIdig 1; m1] $ rest
  (* DIG & DUG: *)
  | MIdig n1 :: (MIcomment _ as c) :: MIdug n2 :: rest when n1 = n2 ->
      [c] $ rest
  | MIdug n1 :: (MIcomment _ as c) :: MIdig n2 :: rest when n1 = n2 ->
      [c] $ rest
  | MIdig n1 :: MIdig n2 :: MIdrop :: rest when n1 >= 1 && n2 >= 1 ->
      if n1 >= n2
      then [MIdig (n2 - 1); MIdrop; MIdig (n1 - 1)] $ rest
      else [MIdig n2; MIdrop; MIdig n1] $ rest
  | push :: MIdig n :: MIdrop :: rest when is_pure_push push && n > 1 ->
      [MIdig (n - 1); MIdrop; push] $ rest
  | MIdup 1 :: MIdig n :: MIdrop :: rest when n > 1 ->
      [MIdig (n - 1); MIdrop; MIdup 1] $ rest
  | MIdup k :: MIdig n :: MIdrop :: rest when n > 1 ->
      if n = k
      then [MIdig (n - 1)] $ rest
      else if n > k
      then [MIdig (n - 1); MIdrop; MIdup k] $ rest
      else [MIdig (n - 1); MIdrop; MIdup (k - 1)] $ rest
  | MIdup k :: MIdig n :: rest when n = k -> [MIdig (n - 1); MIdup 1] $ rest
  | MIdug n1 :: mono :: MIdig n2 :: rest
    when n1 = n2 && has_arity (1, 1) mono && n1 > 1 ->
      [MIdig 1; mono; MIdig 1] $ rest
  | MIdig n :: MIdig 1 :: mono :: MIdig 1 :: rest
    when n >= 1 && has_arity (1, 1) mono ->
      [mono; MIdig n] $ rest
  | MIdug n1 :: MIdig n2 :: MIdrop :: rest when n1 <> n2 && n1 >= 1 && n2 >= 1
    ->
      if n1 > n2
      then [MIdig (n2 + 1); MIdrop; MIdug (n1 - 1)] $ rest
      else [MIdig n2; MIdrop; MIdug n1] $ rest
  | bin :: MIdig n :: MIdrop :: rest when has_arity (2, 1) bin && n >= 1 ->
      [MIdig (n + 1); MIdrop; bin] $ rest
  | i :: MIdig n :: MIdrop :: rest when has_arity (1, 2) i && n >= 2 ->
      [MIdig (n - 1); MIdrop; i] $ rest
  | i :: MIdig 1 :: MIdrop :: MIdig n :: MIdrop :: rest
    when has_arity (1, 2) i && n >= 1 ->
      [MIdig n; MIdrop; i; MIdig 1; MIdrop] $ rest
  | i :: MIdig n :: MIdrop :: rest when has_arity (1, 2) i && n >= 2 ->
      [MIdig (n - 1); MIdrop; i] $ rest
  | i :: MIdig 1 :: MIdrop :: MIdrop :: rest
    when has_arity (1, 2) i && harmless i ->
      [MIdrop] $ rest
  | i :: MIdrop :: MIdrop :: rest when has_arity (1, 2) i && harmless i ->
      [MIdrop] $ rest
  | i :: MIdig n :: MIdrop :: rest when has_arity (1, 2) i && n >= 2 ->
      [MIdig (n - 1); MIdrop; i] $ rest
  | MI2 (Pair _) :: MIunpair [true; true] :: rest -> [] $ rest
  | MI2 (Pair _) :: (MIcomment _ as c) :: MIunpair [true; true] :: rest ->
      [c] $ rest
  | MIunpair [true; true] :: MI2 (Pair _) :: rest -> [] $ rest
  | MI1 Read_ticket :: MIdrop :: rest -> [] $ rest
  | MIdup 1 :: MIfield [D] :: MIdig 1 :: MIfield [A] :: rest ->
      [MIunpair [true; true]] $ rest
  | MIdup 1 :: MIfield [A] :: MIdig 1 :: MIfield [D] :: rest ->
      [MIunpair [true; true]; MIdig 1] $ rest
  | ternary :: MIdig n :: MIdrop :: rest when has_arity (3, 1) ternary && n >= 1
    ->
      [MIdig (n + 2); MIdrop; ternary] $ rest
  | ternary :: MIdig n :: MIdrop :: rest when has_arity (3, 2) ternary && n >= 2
    ->
      [MIdig (n + 1); MIdrop; ternary] $ rest
  | (MIcomment _ as comment) :: push :: MI1_fail Failwith :: rest
    when is_pure_push push ->
      [push; MI1_fail Failwith; comment] $ rest
  (* | MIdrop :: push :: MIfailwith :: rest when is_pure_push push ->
   *     [push :: MIfailwith] $ rest *)
  | MIdup 1 :: MI1_fail Never :: rest -> [MI1_fail Never] $ rest
  | MIdup 1 :: MI1_fail Failwith :: rest -> [MI1_fail Failwith] $ rest
  | MIdug n :: (MI2 (Pair _) as pair) :: MI2 Exec :: MI1_fail Failwith :: rest
    when n > 2 (* for lazy errors *) ->
      [MIdrop; pair; MI2 Exec; MI1_fail Failwith] $ rest
  | MIdug n
    :: (MIpush _ as push)
       :: MIdig k
          :: (MI2 (Pair _) as pair) :: MI2 Exec :: MI1_fail Failwith :: rest
    when n > 2 && k > 2 (* for lazy errors *) ->
      [MIdrop; push; MIdig k; pair; MI2 Exec; MI1_fail Failwith] $ rest
  | (MIcreate_contract _ as create_contract) :: MIdig n :: MIdrop :: rest
    when n > 1 ->
      [MIdig (n + 1); MIdrop; create_contract] $ rest
  | MI2 (Pair _)
    :: MIcomment _
       :: MIdup 1
          :: MIfield [A]
             :: MI0 (Nil {mt = MT0 T_operation})
                :: MIdig 1
                   :: MI2 Cons :: MIcomment _ :: MIdig 1 :: MIfield [D] :: rest
   |MI2 (Pair _)
    :: MIdup 1
       :: MIfield [A]
          :: MI0 (Nil {mt = MT0 T_operation})
             :: MIdig 1 :: MI2 Cons :: MIdig 1 :: MIfield [D] :: rest ->
      (* ad-hoc rule for usual CREATE_CONTRACT output *)
      [MI0 (Nil mt_operation); MIdig 1; MI2 Cons; MIdig 1] $ rest
  | MIcomment comment :: MIdrop :: rest -> [MIdrop; MIcomment comment] $ rest
  | MIcomment comment :: MIdig 1 :: rest -> [MIdig 1; MIcomment comment] $ rest
  | MIcomment comment :: MIdig n :: MIdrop :: rest ->
      [MIdig n; MIdrop; MIcomment comment] $ rest
  | mono :: MIdig n :: MIdrop :: rest when n >= 1 && has_arity (1, 1) mono ->
      [MIdig n; MIdrop; mono] $ rest
  | (MIiter {instr = MI2 Cons} as mono) :: MIdig n :: MIdrop :: rest when n > 1
    ->
      [MIdig (n + 1); MIdrop; mono] $ rest
  | (MIpush _ as push)
    :: MIdig 1
       :: (MIiter {instr = MI2 Cons} as mono) :: MIdig 1 :: MIdrop :: rest ->
      [MIdig 1; MIdrop; push; MIdig 1; mono] $ rest
  | MIdug n1 :: MIdrop :: rest when n1 >= 1 ->
      [MIdig 1; MIdrop; MIdug (n1 - 1)] $ rest
  | MIdup 1 :: MIdug n :: MIdrop :: rest when n > 0 -> [MIdug (n - 1)] $ rest
  | MIdup 1 :: MIdip {instr = MIdig 1} :: rest -> [MIdup 1; MIdug 2] $ rest
  | push :: MIdig 1 :: MIdup 1 :: MIdug 2 :: rest when is_pure_push push ->
      [MIdup 1; push; MIdig 1] $ rest
  | MIdup 1 :: push :: bin :: MIdig 1 :: MIdrop :: rest
    when is_pure_push push && has_arity (2, 1) bin ->
      [push; bin] $ rest
  (* Constant folding: *)
  | MIpush (_, {literal = Int a})
    :: MIpush (_, {literal = Int b}) :: MI2 Compare :: rest ->
      [MIpush (mt_int, MLiteral.small_int (compare_big_int b a))] $ rest
  | MIpush ({mt = MT0 T_int}, {literal = Int a}) :: MI1 Eq :: rest ->
      [MIpush (mt_bool, MLiteral.bool (eq_big_int a zero_big_int))] $ rest
  | MIpush ({mt = MT0 T_int}, {literal = Int a}) :: MI1 Lt :: rest ->
      [MIpush (mt_bool, MLiteral.bool (lt_big_int a zero_big_int))] $ rest
  | MIpush ({mt = MT0 T_int}, {literal = Int a}) :: MI1 Gt :: rest ->
      [MIpush (mt_bool, MLiteral.bool (gt_big_int a zero_big_int))] $ rest
  | MIpush ({mt = MT0 T_bool}, {literal = Bool true}) :: MIif (a, _) :: rest ->
      [a.instr] $ if fails a then [] else rest
  | MIpush ({mt = MT0 T_bool}, {literal = Bool false}) :: MIif (_, b) :: rest ->
      [b.instr] $ if fails b then [] else rest
  | (MI1_fail _ as fail) :: _ :: _ -> [fail] $ []
  | MIpush (t, {literal = AnyMap xs})
    :: MIpush (_, {literal = Some_ value})
       :: MIpush (_, key) :: MI3 Update :: rest ->
      [MIpush (t, MLiteral.mk_map ((key, value) :: xs))] $ rest
  | MIpush (t, {literal = Seq xs})
    :: MIpush (_, {literal = Bool true}) :: MIpush (_, x) :: MI3 Update :: rest
    ->
      [MIpush (t, MLiteral.set (x :: xs))] $ rest
  (* Pushing the same thing twice (will be unfolded again): *)
  | MIpush (t, l) :: MIdup 1 :: rest -> [MIpush (t, l); MIpush (t, l)] $ rest
  | MIif (x, y) :: rest -> cond_check_last (fun x y -> MIif (x, y)) x y rest
  | MIif_none (x, y) :: rest ->
      cond_check_last (fun x y -> MIif_none (x, y)) x y rest
  | MIif_left (x, y) :: rest ->
      cond_check_last (fun x y -> MIif_left (x, y)) x y rest
  | MIif_cons (x, y) :: rest ->
      cond_check_last (fun x y -> MIif_cons (x, y)) x y rest
  | MIlambda (_, _, {instr}) :: MIdig 1 :: MI2 Exec :: rest -> [instr] $ rest
  | MIdig n1 :: MIdrop :: MIdig n2 :: MIdrop :: rest when n1 > n2 ->
      [MIdig n2; MIdrop; MIdig (n1 - 1); MIdrop] $ rest
  | (MIdig n | MIdug n) :: _ as instrs -> dig_dug ~with_comments:false n instrs
  | _ -> rewrite_none

let unpair : rule = function
  | MIunpair [] :: rest -> [] $ rest
  | MIunpair [true] :: rest -> [] $ rest
  | MIunpair [false] :: rest -> [MIdrop] $ rest
  | MIunpair [true; true] :: MIdrop :: rest -> [MIfield [D]] $ rest
  | MIunpair [true; false] :: MIdrop :: rest -> [MIdrop] $ rest
  | MIunpair (false :: (_ :: _ :: _ as fields)) :: rest
   |MIunpair (true :: (_ :: _ :: _ as fields)) :: MIdrop :: rest ->
      [MIfield [D]; MIunpair fields] $ rest
  | MIunpair (_ :: _ :: _ :: _ as fields) :: MIdig n :: MIdrop :: rest ->
      let k = unpair_size fields in
      if n >= k
      then [MIdig (n - k + 1); MIdrop; MIunpair fields] $ rest
      else
        let rec drop_field n fields =
          match (n, fields) with
          | 0, true :: fields -> false :: fields
          | n, select :: fields ->
              select :: drop_field (n - if select then 1 else 0) fields
          | _, [] -> assert false
        in
        [MIunpair (drop_field n fields)] $ rest
  | MIunpair [true; true] :: MIdig 1 :: MIdrop :: rest -> [MIfield [A]] $ rest
  | _ -> rewrite_none

let normalize f =
  let rec norm = function
    | {instr = MIseq xs} -> norm_seq xs
    | {instr} -> mk_instr (map_instr_f norm1 id instr)
  and norm1 {instr} = norm_seq (List.map mk_instr (to_seq instr))
  and norm_seq xs = norm_seq_aux [] (List.rev xs)
  and norm_seq_aux acc = function
    | [] -> {instr = of_seq acc}
    | i :: is ->
        let {instr} = norm i in
        let acc = instr :: acc in
        ( match f acc with
        | None -> norm_seq_aux acc is
        | Some (result, rest) ->
            if check_rest_invariant
            then
              assert (
                let i instr = {instr} in
                List.is_suffix equal_instr (List.map i rest) (List.map i acc) );
            norm_seq_aux rest (List.map mk_instr (List.rev result) @ is) )
  in
  norm1

let run_group rs =
  let comp f g x =
    match f x with
    | Some x -> Some x
    | None -> g x
  in
  normalize (List.fold_left comp (fun _ -> None) rs)

let run groups =
  List.fold_left (fun f g x -> g (f x)) id (List.map run_group groups)

let run_on_tcontract groups c =
  if List.length (has_error_tcontract ~accept_missings:true c) > 0
  then c
  else
    let c = erase_types_contract c in
    let c =
      { c with
        code = run groups c.code
      ; views = List.map (map_view (run groups)) c.views }
    in
    typecheck_contract ~strict_dup:false c

let simplify ~config =
  [ [unfold_macros; main; unpair; conditionals]
  ; [extract_unpair_drops]
  ; [unpair]
  ; [fold_macros_etc]
  ; [unfold_mifield]
  ; ( match config.Config.protocol with
    | Delphi -> [unfold_unpair; unfold_dupn; unfold_self_address]
    | Edo | Florence | Granada | Hangzhou | Ithaca ->
        [fold_dupn; fold_getn; fold_self_address] )
  ; [extract_unpair_drops] ]

let pushify = [[unfold_macros; pushify_all; main; conditionals]]

let collapse_drops =
  let c1 = function
    | MIdrop :: rest -> [MIdropn 1] $ rest
    | MIdropn n1 :: MIdropn n2 :: rest -> [MIdropn (n1 + n2)] $ rest
    | _ -> rewrite_none
  in
  let c2 = function
    | MIdropn 1 :: rest -> [MIdrop] $ rest
    | _ -> rewrite_none
  in
  [[c1]; [c2]]
