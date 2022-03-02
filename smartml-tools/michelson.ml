(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Control
include Michelson_base.Type
include Michelson_base.Primitive
include Michelson_base.Typing
open Printf

let full_types_and_tags = false

let cata_mtype_stripped f =
  cata_mtype (fun ?annot_type:_ ?annot_variable:_ x -> f x)

let has_missing_type =
  cata_mtype_stripped (function
      | MT_var t -> [t]
      | x -> fold_mtype_f ( @ ) [] x)

let remove_annots =
  cata_mtype_stripped (function
      | MT2 (T_pair _, fst, snd) -> mt_pair fst snd
      | MT2 (T_or _, left, right) -> mt_or left right
      | x -> mk_mtype x)

type ad_step =
  | A
  | D
[@@deriving eq, ord, show {with_path = false}]

type tezos_int = Bigint.t [@@deriving eq, ord, show {with_path = false}]

type stack =
  | Stack_ok     of mtype list
  | Stack_failed
[@@deriving eq, ord, show {with_path = false}]

type ('i, 'literal) instr_f =
  | MI0                 of mtype prim0
  | MI1                 of mtype prim1
  | MI1_fail            of prim1_fail
  | MI2                 of mtype prim2
  | MI3                 of prim3
  | MIerror             of string
  | MIcomment           of string list
  | MImich              of mtype Basics.inline_michelson
  | MIdip               of 'i
  | MIdipn              of int * 'i
  | MIloop              of 'i
  | MIloop_left         of 'i
  | MIiter              of 'i
  | MImap               of 'i
  | MIdrop
  | MIdropn             of int
  | MIdup               of int
  | MIdig               of int
  | MIdug               of int
  | MIif                of 'i * 'i
  | MIif_left           of 'i * 'i
  | MIif_none           of 'i * 'i
  | MIif_cons           of 'i * 'i
  | MIpush              of mtype * 'literal
  | MIseq               of 'i list
  | MIswap
  | MIunpair            of bool list
  | MIpairn             of int
  | MIfield             of ad_step list
  | MIsetField          of ad_step list
  | MIlambda            of mtype * mtype * 'i
  | MIcreate_contract   of
      { tparameter : mtype * string option
      ; tstorage : mtype
      ; code : 'i }
  | MIconcat_unresolved
[@@deriving eq, ord, show {with_path = false}, map, fold]

type ('instr, 'literal) literal_f =
  | Int      of tezos_int
  | Bool     of bool
  | String   of string
  | Bytes    of string
  | Unit
  | Pair     of 'literal * 'literal
  | None_
  | Left     of 'literal
  | Right    of 'literal
  | Some_    of 'literal
  | Seq      of 'literal list
  | Elt      of ('literal * 'literal)
  | Instr    of 'instr
  | AnyMap   of ('literal * 'literal) list
  | Constant of string
[@@deriving eq, ord, show {with_path = false}, map, fold]

let sequence_literal_f =
  let open Result in
  function
  | (Int _ | Bool _ | String _ | Bytes _ | Unit | None_ | Constant _) as l ->
      Ok l
  | Pair (x, y) -> map2 (fun x y -> Pair (x, y)) x y
  | Left x -> map (fun x -> Left x) x
  | Right x -> map (fun x -> Right x) x
  | Some_ x -> map (fun x -> Some_ x) x
  | Seq xs -> map (fun x -> Seq x) (sequence_list xs)
  | Elt (x, y) -> map2 (fun x y -> Elt (x, y)) x y
  | AnyMap xs -> map (fun x -> AnyMap x) (map_list (uncurry (map2 pair)) xs)
  | Instr x -> map (fun x -> Instr x) x

let sequence_instr_f =
  let open Result in
  function
  | MIdip x -> map (fun x -> MIdip x) x
  | MIdipn (n, x) -> map (fun x -> MIdipn (n, x)) x
  | MIloop x -> map (fun x -> MIloop x) x
  | MIloop_left x -> map (fun x -> MIloop_left x) x
  | MIiter x -> map (fun x -> MIiter x) x
  | MImap x -> map (fun x -> MImap x) x
  | MIif (i1, i2) -> map2 (fun i1 i2 -> MIif (i1, i2)) i1 i2
  | MIif_left (i1, i2) -> map2 (fun i1 i2 -> MIif_left (i1, i2)) i1 i2
  | MIif_none (i1, i2) -> map2 (fun i1 i2 -> MIif_none (i1, i2)) i1 i2
  | MIif_cons (i1, i2) -> map2 (fun i1 i2 -> MIif_cons (i1, i2)) i1 i2
  | MIlambda (t1, t2, i) -> map (fun x -> MIlambda (t1, t2, x)) i
  | MIcreate_contract {tparameter; tstorage; code} ->
      map (fun code -> MIcreate_contract {tparameter; tstorage; code}) code
  | MIseq is -> map (fun is -> MIseq is) (sequence_list is)
  | MIpush (t, l) -> map (fun l -> MIpush (t, l)) l
  | ( MI0 _ | MI1 _ | MI1_fail _ | MI2 _
    | MI3
        ( Slice | Update | Get_and_update | Transfer_tokens | Check_signature
        | Open_chest )
    | MIdrop | MIswap | MIerror _ | MIcomment _ | MImich _ | MIdropn _ | MIdup _
    | MIdig _ | MIdug _ | MIunpair _ | MIpairn _ | MIfield _ | MIsetField _
    | MIconcat_unresolved ) as instr ->
      return instr

type instr = {instr : (instr, literal) instr_f}

and literal = {literal : (instr, literal) literal_f}
[@@deriving eq, ord, show {with_path = false}, map, fold]

type ('i, 'l) alg =
  { f_instr : ('i, 'l) instr_f -> 'i
  ; f_literal : ('i, 'l) literal_f -> 'l }

let cata {f_instr; f_literal} =
  let rec cata_instr {instr} =
    f_instr (map_instr_f cata_instr cata_literal instr)
  and cata_literal {literal} =
    f_literal (map_literal_f cata_instr cata_literal literal)
  in
  (cata_instr, cata_literal)

let cata_instr alg = fst (cata alg)

let cata_literal alg = snd (cata alg)

module MLiteral = struct
  let compare x y = compare_literal x y

  let int i = {literal = Int i}

  let small_int i = {literal = Int (Bigint.of_int i)}

  let bool x = {literal = Bool x}

  let string x = {literal = String x}

  let bytes x = {literal = Bytes x}

  let unit = {literal = Unit}

  let left x = {literal = Left x}

  let right x = {literal = Right x}

  let some x = {literal = Some_ x}

  let pair x1 x2 = {literal = Pair (x1, x2)}

  let none = {literal = None_}

  let list xs = {literal = Seq xs}

  let set xs = {literal = Seq (Base.List.dedup_and_sort ~compare xs)}

  let seq xs = {literal = Seq xs}

  let elt k v = {literal = Elt (k, v)}

  let mk_map xs =
    { literal =
        AnyMap
          (Base.List.dedup_and_sort
             ~compare:(fun (k1, _) (k2, _) -> compare k1 k2)
             xs) }

  let sapling_empty_state = seq []

  let constant hash = {literal = Constant hash}

  let instr body = {literal = Instr body}

  let rec to_michelson_string instr_to_string ~protect : literal -> string =
    let continue ~protect = to_michelson_string instr_to_string ~protect in
    let open Printf in
    let prot ~protect s = if protect then sprintf "(%s)" s else s in
    fun {literal} ->
      match literal with
      | Int i -> Big_int.string_of_big_int i
      | Unit -> "Unit"
      | String s -> sprintf "%S" s
      | Bool true -> "True"
      | Bool false -> "False"
      | Pair (l, r) ->
          prot
            ~protect
            (sprintf
               "Pair %s %s"
               (continue ~protect:true l)
               (continue ~protect:true r))
      | None_ -> "None"
      | Left l -> prot ~protect (sprintf "Left %s" (continue ~protect:true l))
      | Right l -> prot ~protect (sprintf "Right %s" (continue ~protect:true l))
      | Some_ l -> prot ~protect (sprintf "Some %s" (continue ~protect:true l))
      | Bytes string_bytes -> "0x" ^ Misc.Hex.hexcape string_bytes
      | Seq xs ->
          sprintf
            "{%s}"
            (String.concat
               "; "
               (List.filter_map
                  (fun x ->
                    let x = continue ~protect:false x in
                    if x = "" then None else Some x)
                  xs))
      | AnyMap xs ->
          let f (k, v) = {literal = Elt (k, v)} in
          let xs = {literal = Seq (List.map f xs)} in
          continue ~protect xs
      | Elt (k, v) ->
          sprintf
            "Elt %s %s"
            (continue ~protect:true k)
            (continue ~protect:true v)
      | Instr i -> instr_to_string i
      | Constant s -> prot ~protect (sprintf "constant %S" s)

  let to_michelson_string = to_michelson_string ~protect:true
end

let string_of_ad_path p =
  let f = function
    | A -> "A"
    | D -> "D"
  in
  String.concat "" (List.map f p)

let insert_field_annot a e =
  let open Base.Sexp in
  match a with
  | None | Some "" -> e
  | Some a ->
      let a = Atom ("%" ^ a) in
      ( match e with
      | Atom s -> List [Atom s; a]
      | List (Atom s :: xs) -> List (Atom s :: a :: xs)
      | _ -> assert false )

let s_expression_of_mtype ?full ?human =
  let is_full = full = Some () in
  let is_human = human = Some () && not is_full in
  let open Base.Sexp in
  let f ?annot_type ?annot_variable mt =
    let annots =
      let get pref = function
        | None -> None
        | Some s -> Some (Atom (pref ^ s))
      in
      Base.List.filter_opt [get ":" annot_type; get "@" annot_variable]
    in
    let mk = function
      | [] -> assert false
      | [x] -> x
      | xs -> List xs
    in
    let atom s = mk (Atom s :: annots) in
    let call s l = List ((Atom s :: annots) @ l) in
    match annot_variable with
    | Some a when is_human ->
      ( match Base.String.split ~on:'.' a with
      | [] -> assert false
      | hd :: xs ->
          let rec collapse = function
            | ("left" | "right") :: xs -> collapse xs
            | [last] -> Atom ("@" ^ hd ^ "%" ^ last)
            | _ -> Atom ("@" ^ a)
          in
          collapse xs )
    | _ ->
      ( match mt with
      | MT0 t ->
          let s, memo = string_of_type0 t in
          Option.cata (atom s) (fun memo -> call s [Atom memo]) memo
      | MT1 (t, t1) -> call (string_of_type1 t) [t1]
      | MT2 (t, t1, t2) ->
          let t, a1, a2 = string_of_type2 t in
          call t [insert_field_annot a1 t1; insert_field_annot a2 t2]
      | MT_var s -> call "missing_type_conversion" [Atom s] )
  in
  cata_mtype f

let buffer_mtype_sexp ~html b =
  let rec buffer : Base.Sexp.t -> _ = function
    | Atom s ->
        Buffer.add_string b (Base.Sexp.Private.mach_maybe_esc_str s)
        (* See how this is used in `_opam/lib/sexplib0/sexp.ml` *)
    | List [] -> Buffer.add_string b "()"
    | List (h :: t) ->
        let is_partial =
          match h with
          | Atom "missing_type_conversion" when html -> true
          | _ -> false
        in
        Buffer.add_char b '(';
        if is_partial then Buffer.add_string b "<span class='partialType'>";
        buffer h;
        Base.List.iter t ~f:(fun elt ->
            Buffer.add_char b ' ';
            buffer elt);
        if is_partial then Buffer.add_string b "</span>";
        Buffer.add_char b ')'
  in
  buffer

let buffer_mtype ?full ?human ?protect ?annot ~html b t =
  let s_expr = s_expression_of_mtype ?full ?human t in
  let s_expr = insert_field_annot annot s_expr in
  let sexp_to_string_flat = buffer_mtype_sexp ~html b in
  match (protect, s_expr) with
  | None, List l -> Misc.buffer_concat b " " sexp_to_string_flat l
  | None, Atom a -> Buffer.add_string b (Base.Sexp.Private.mach_maybe_esc_str a)
  | Some (), any -> sexp_to_string_flat any

let string_of_mtype ?full ?human ?protect ?annot ~html t =
  Misc.with_buffer (fun b ->
      buffer_mtype ?full ?human ?protect ?annot ~html b t)

let string_of_tparameter ~html (tparameter, annot) =
  string_of_mtype ~protect:() ~html ?annot tparameter

let memo_string_of_mtype_human =
  Misc.memoize ~clear_after:1000 (fun _f (full, se) ->
      string_of_mtype
        ?full:(if full then Some () else None)
        ~human:()
        ~html:false
        se)

let memo_string_of_mtype_human ?full t =
  memo_string_of_mtype_human (full = Some (), t)

let string_of_ok_stack ?full stack =
  String.concat " : " (List.map (memo_string_of_mtype_human ?full) stack)

let string_of_stack ?full = function
  | Stack_ok stack -> string_of_ok_stack ?full stack
  | Stack_failed -> "FAILED"

let strip_annots {mt} = mk_mtype mt

let strip_annot_variable {mt; annot_type} = mk_mtype mt ?annot_type

(** {1 Stack helpers} *)

type tliteral =
  { tliteral : (tinstr, tliteral) literal_f
  ; t : mtype Result.t }

and tinstr =
  { tinstr : (tinstr, tliteral) instr_f
  ; stack_in : stack Result.t
  ; stack_out : stack Result.t }
[@@deriving eq, ord, show {with_path = false}]

type ('i, 'l) talg =
  { f_tinstr :
         stack_in:stack Result.t
      -> stack_out:stack Result.t
      -> ('i, 'l) instr_f
      -> 'i
  ; f_tliteral : t:mtype Result.t -> ('i, 'l) literal_f -> 'l }

let tcata {f_tinstr; f_tliteral} =
  let rec cata_tinstr {tinstr; stack_in; stack_out} =
    f_tinstr ~stack_in ~stack_out (map_instr_f cata_tinstr cata_tliteral tinstr)
  and cata_tliteral {tliteral; t} =
    f_tliteral ~t (map_literal_f cata_tinstr cata_tliteral tliteral)
  in
  (cata_tinstr, cata_tliteral)

let cata_tinstr alg = fst (tcata alg)

let cata_tliteral alg = snd (tcata alg)

(* Paramorphisms on instructions and literals. *)
let para_alg ~p_instr ~p_literal =
  let f_instr i = ({instr = map_instr_f fst fst i}, p_instr i) in
  let f_literal l = ({literal = map_literal_f fst fst l}, p_literal l) in
  {f_instr; f_literal}

let _size_tinstr, _size_tliteral =
  let f_tinstr ~stack_in:_ ~stack_out:_ = fold_instr_f ( + ) ( + ) 1 in
  let f_tliteral ~t:_ = fold_literal_f ( + ) ( + ) 1 in
  tcata {f_tinstr; f_tliteral}

let erase_types_instr, erase_types_literal =
  let f_tinstr ~stack_in:_ ~stack_out:_ instr = {instr} in
  let f_tliteral ~t:_ literal = {literal} in
  tcata {f_tinstr; f_tliteral}

type instr_spec =
  { name : string
  ; rule :
         tparameter:mtype * string option
      -> mtype list
      -> (stack Result.t -> tinstr, mtype -> tliteral) instr_f
      -> (tinstr, tliteral) instr_f * stack Result.t
  ; commutative : bool
  ; arities : (int * int) option }

let mk_spec_raw name ?commutative ?arities rule =
  {name; commutative = commutative = Some (); arities; rule}

let mk_spec name ?commutative ?arities rule =
  let rule ~tparameter stack instr =
    let err msg =
      let tinstr =
        map_instr_f
          (fun x -> x (Error (name ^ " error")))
          (fun _ -> assert false)
          instr
      in
      (tinstr, Error msg)
    in
    match rule ~tparameter stack instr with
    | Some x -> x
    | None -> err (name ^ ": unexpected stack")
  in
  {name; commutative = commutative = Some (); arities; rule}

let mk_spec_no_sub name ?commutative ?arities rule =
  let rule ~tparameter stack instr =
    let tinstr =
      map_instr_f (fun _ -> assert false) (fun _ -> assert false) instr
    in
    let stack = rule ~tparameter stack in
    Some (tinstr, stack)
  in
  mk_spec name ?commutative ?arities rule

let mk_spec_basic name ?commutative ~arities:(a_in, a_out) rule =
  let rule ~tparameter:_ stack instr =
    match rule stack with
    | None -> None
    | Some xs ->
        assert (List.length xs = a_out);
        let tinstr =
          map_instr_f (fun _ -> assert false) (fun _ -> assert false) instr
        in
        let stack = Ok (Stack_ok (xs @ List.drop a_in stack)) in
        Some (tinstr, stack)
  in
  mk_spec name ?commutative ~arities:(a_in, a_out) rule

let mk_spec_const name t =
  mk_spec_basic ~arities:(0, 1) name (fun _ -> Some [t])

(** {1 Unification}  *)

let unifiable_types t u = Base.Result.is_ok (unify_types t u)

let unify_stack_elements t1 t2 =
  match unify_types t1 t2 with
  | Ok t -> Some t
  | Error _ -> None

let rec unify_ok_stacks s1 s2 =
  match (s1, s2) with
  | se1 :: s1, se2 :: s2 ->
      Option.map2
        (fun x xs -> x :: xs)
        (unify_stack_elements se1 se2)
        (unify_ok_stacks s1 s2)
  | [], [] -> Some []
  | _ -> None

let unifiable_ok_stacks t u = Option.is_some (unify_ok_stacks t u)

let unify_stacks s1 s2 =
  match (s1, s2) with
  | Stack_ok s1, Stack_ok s2 ->
      Option.map (fun x -> Stack_ok x) (unify_ok_stacks s1 s2)
  | Stack_failed, s2 -> Some s2
  | s1, Stack_failed -> Some s1

let initial_stack ~tparameter ~tstorage =
  Stack_ok
    [ mt_pair
        {tparameter with annot_variable = Some "parameter"}
        {tstorage with annot_variable = Some "storage"} ]

(** {1 Michelson instructions} *)

let mi_seq =
  let rule ~tparameter:_ stack = function
    | MIseq xs ->
        let rec f r stack = function
          | [] -> Some (MIseq (List.rev r), stack)
          | x :: xs ->
              let (x : tinstr) = x stack in
              f (x :: r) x.stack_out xs
        in
        f [] (Ok (Stack_ok stack)) xs
    | _ -> assert false
  in
  mk_spec "(instruction sequence)" rule

let mi_comment =
  let rule ~tparameter:_ stack = Ok (Stack_ok stack) in
  mk_spec_no_sub "(comment instruction)" rule

let mi_error s =
  let rule ~tparameter:_ _stack = Error s in
  mk_spec_no_sub "(error instruction)" rule

let mi_failwith =
  let rule ~tparameter:_ = function
    | _ :: _ -> Ok Stack_failed
    | [] -> Error "FAILWITH on empty stack"
  in
  mk_spec_no_sub "FAILWITH" ~arities:(1, 0) rule

let mi_never =
  let rule ~tparameter:_ = function
    | {mt = MT0 T_never} :: _ -> Ok Stack_failed
    | _ -> Error "NEVER on empty stack"
  in
  mk_spec_no_sub "NEVER" ~arities:(1, 0) rule

let mi_ticket =
  mk_spec_basic "TICKET" ~arities:(2, 1) (function
      | t :: {mt = MT0 T_nat} :: _ -> Some [mt_ticket t]
      | _ -> None)

let mi_read_ticket =
  mk_spec_basic "READ_TICKET" ~arities:(1, 2) (function
      | {mt = MT1 (T_ticket, t)} :: _ ->
          Some [mt_pair mt_address (mt_pair t mt_nat); mt_ticket t]
      | _ -> None)

let mi_join_tickets =
  mk_spec_basic "JOIN_TICKETS" ~arities:(1, 1) (function
      | { mt =
            MT2
              ( T_pair _
              , ({mt = MT1 (T_ticket, _)} as t1)
              , ({mt = MT1 (T_ticket, _)} as t2) ) }
        :: _
        when unifiable_types t1 t2 ->
          Some [mt_option t1]
      | _ -> None)

let mi_split_ticket =
  mk_spec_basic "SPLIT_TICKET" ~arities:(2, 1) (function
      | ({mt = MT1 (T_ticket, _)} as t)
        :: {mt = MT2 (T_pair _, {mt = MT0 T_nat}, {mt = MT0 T_nat})} :: _ ->
          Some [mt_option (mt_pair t t)]
      | _ -> None)

let mi_pairing_check =
  mk_spec_basic "PAIRING_CHECK" ~arities:(1, 1) (function
      | { mt =
            MT1
              ( T_list
              , { mt =
                    MT2
                      ( T_pair _
                      , {mt = MT0 T_bls12_381_g1}
                      , {mt = MT0 T_bls12_381_g2} ) } ) }
        :: _ ->
          Some [mt_bool]
      | _ -> None)

let cond_aux x y =
  let open Result in
  let* sx = x.stack_out in
  let* sy = y.stack_out in
  Option.cata (error "cannot unify branches") ok (unify_stacks sx sy)

let mi_if =
  let rule ~tparameter:_ stack instr =
    match (stack, instr) with
    | {mt = MT0 T_bool} :: tail, MIif (x, y) ->
        let x = x (Ok (Stack_ok tail)) in
        let y = y (Ok (Stack_ok tail)) in
        Some (MIif (x, y), cond_aux x y)
    | _, MIif _ -> None
    | _ -> assert false
  in
  mk_spec "IF" rule

let mi_if_none =
  let rule ~tparameter:_ stack instr =
    match (stack, instr) with
    | {mt = MT1 (T_option, t)} :: tail, MIif_none (x, y) ->
        let a =
          match t.annot_variable with
          | Some v -> v ^ ".some"
          | None -> "some"
        in
        let t = {t with annot_variable = Some a} in
        let x = x (Ok (Stack_ok tail)) in
        let y = y (Ok (Stack_ok (t :: tail))) in
        Some (MIif_none (x, y), cond_aux x y)
    | _, MIif_none _ -> None
    | _ -> assert false
  in
  mk_spec "IF_NONE" rule

let mi_if_left =
  let rule ~tparameter:_ stack instr =
    match (stack, instr) with
    | ( {mt = MT2 (T_or {annot_left; annot_right}, t, u); annot_variable} :: tail
      , MIif_left (x, y) ) ->
        let open Option in
        let fa v = v ^ "." ^ Option.default "left" annot_left in
        let t = {t with annot_variable = Option.map fa annot_variable} in
        let fa v = v ^ "." ^ Option.default "right" annot_right in
        let u = {u with annot_variable = Option.map fa annot_variable} in
        let x = x (Ok (Stack_ok (t :: tail))) in
        let y = y (Ok (Stack_ok (u :: tail))) in
        return (MIif_left (x, y), cond_aux x y)
    | _, MIif_left _ -> None
    | _ -> assert false
  in
  mk_spec "IF_LEFT" rule

let mi_if_cons =
  let rule ~tparameter:_ stack instr =
    match (stack, instr) with
    | {mt = MT1 (T_list, t)} :: tail, MIif_cons (x, y) ->
        let open Option in
        let x = x (Ok (Stack_ok (t :: mt_list t :: tail))) in
        let y = y (Ok (Stack_ok tail)) in
        return (MIif_cons (x, y), cond_aux x y)
    | _, MIif_cons _ -> None
    | _ -> assert false
  in
  mk_spec "IF_CONS" rule

let mi_dip =
  let rule ~tparameter:_ stack = function
    | MIdip body ->
      ( match stack with
      | [] -> None
      | t :: tail ->
          let body = body (Ok (Stack_ok tail)) in
          let tinstr = MIdip body in
          ( match body.stack_out with
          | Ok (Stack_ok tail') -> Some (tinstr, Ok (Stack_ok (t :: tail')))
          | _ -> Some (tinstr, Error "DIP: body error") ) )
    | _ -> assert false
  in
  mk_spec "DIP" rule

let mi_dipn =
  let rule ~tparameter:_ stack = function
    | MIdipn (n, body) when n <= List.length stack ->
        assert (n >= 0);
        let body = body (Ok (Stack_ok (List.drop n stack))) in
        let tinstr = MIdipn (n, body) in
        ( match body.stack_out with
        | Ok (Stack_ok stack') ->
            Some (tinstr, Ok (Stack_ok (List.take n stack @ stack')))
        | _ -> Some (tinstr, Error "DIP: body error") )
    | _ -> assert false
  in
  mk_spec "DIPn" rule

let is_hot =
  let open Ternary in
  let is_hot ?annot_type:_ ?annot_variable:_ mt =
    match mt with
    | MT1 (T_ticket, _) -> Yes
    | MT1 (T_contract, _) | MT2 (T_lambda, _, _) -> No
    | MT_var _ -> Maybe
    | t -> fold_mtype_f or_ No t
  in
  cata_mtype is_hot

let is_duppable t = is_hot t <> Yes

let mi_dup ~strict_dup i =
  assert (i >= 1);
  let rec get acc n = function
    | x :: _ when n = 1 ->
        if (not strict_dup) || is_duppable x
        then Some (x :: List.rev (x :: acc))
        else None
    | x :: rest -> get (x :: acc) (n - 1) rest
    | [] -> None
  in
  mk_spec_basic "DUP" ~arities:(i, i + 1) (get [] i)

let mi_dig n =
  let rule ~tparameter:_ stack =
    match List.split_at_opt n stack with
    | None -> Error (sprintf "DIG %i: stack too short" n)
    | Some (hi, lo) ->
      ( match lo with
      | [] -> Error (sprintf "DIG %i: stack too short" n)
      | x :: lo -> Ok (Stack_ok ((x :: hi) @ lo)) )
  in
  mk_spec_no_sub "DIG" rule

let mi_dug n =
  let rule ~tparameter:_ = function
    | x :: tail ->
        if n > List.length tail
        then Error (sprintf "DUG %i: stack too short" n)
        else
          let hi, lo = List.split_at n tail in
          Ok (Stack_ok (hi @ (x :: lo)))
    | [] -> Error "DUG: empty stack"
  in
  mk_spec_no_sub "DUG" rule

let mi_swap =
  mk_spec_basic "SWAP" ~arities:(2, 2) (function
      | a :: b :: _ -> Some [b; a]
      | _ -> None)

let mi_drop =
  mk_spec_basic "DROP" ~arities:(1, 0) (function
      | _ :: _ -> Some []
      | [] -> None)

let mi_dropn n =
  mk_spec_basic "DROP" ~arities:(n, 0) (function
      | _ :: _ -> Some []
      | [] -> None)

let unpair_size = List.fold_left (fun acc x -> acc + if x then 1 else 0) 0

let unpair_arg xs =
  if List.for_all id xs
  then string_of_int (List.length xs)
  else List.show String.pp (List.map string_of_bool xs)

let mi_unpair fields =
  assert (List.length fields >= 2);
  let rec aux acc fields stack =
    match (stack, fields) with
    | _, [] -> Some (List.rev acc)
    | _ :: _, [false] -> Some (List.rev acc)
    | se :: _, [true] -> Some (List.rev (se :: acc))
    | {mt = MT2 (T_pair _, fst, snd)} :: rest, select :: fields ->
        let acc = if select then fst :: acc else acc in
        aux acc fields (snd :: rest)
    | _ -> None
  in
  mk_spec_basic "UNPAIR" ~arities:(1, unpair_size fields) (aux [] fields)

let mi_pairn n =
  let rec aux acc n stack =
    if n = 0
    then
      let rec fold acc = function
        | x :: rest -> fold (mt_pair x acc) rest
        | [] -> acc
      in
      match acc with
      | [] -> None
      | x :: rest -> Some [fold x rest]
    else
      match stack with
      | se :: rest -> aux (se :: acc) (n - 1) rest
      | _ -> None
  in
  mk_spec_basic "PAIR" ~arities:(n, 1) (aux [] n)

let mi_iter =
  let rule ~tparameter:_ stack instr =
    match (stack, instr) with
    | c :: tail, MIiter body ->
        let el =
          match c.mt with
          | MT2 (T_map, k, v) -> Some (mt_pair k v)
          | MT1 ((T_list | T_set), t) -> Some t
          | _ -> None
        in
        ( match el with
        | None -> None
        | Some el ->
            let body = body (Ok (Stack_ok (el :: tail))) in
            let tinstr = MIiter body in
            ( match body.stack_out with
            | Ok (Stack_ok stack') when unifiable_ok_stacks tail stack' ->
                Some (tinstr, Ok (Stack_ok tail))
            | _ -> Some (tinstr, Error "ITER: incompatible body") ) )
    | [], MIiter _ -> None
    | _ -> assert false
  in
  mk_spec "ITER" rule

let mi_loop =
  let rule ~tparameter:_ stack = function
    | MIloop body ->
      ( match stack with
      | {mt = MT0 T_bool} :: tail ->
          let body = body (Ok (Stack_ok tail)) in
          let tinstr = MIloop body in
          ( match body.stack_out with
          | Ok (Stack_ok ({mt = MT0 T_bool} :: tail'))
            when unifiable_ok_stacks tail tail' ->
              Some (tinstr, Ok (Stack_ok tail))
          | _ -> Some (tinstr, Error "LOOP: incompatible body") )
      | _ -> None )
    | _ -> assert false
  in
  mk_spec "LOOP" rule

let mi_loop_left =
  let rule ~tparameter:_ stack = function
    | MIloop_left body ->
      ( match stack with
      | {mt = MT2 (T_or _, a, b)} :: tail ->
          let body = body (Ok (Stack_ok (a :: tail))) in
          let tinstr = MIloop_left body in
          ( match body.stack_out with
          | Ok (Stack_ok ({mt = MT2 (T_pair _, a', b')} :: tail'))
            when unifiable_types a a'
                 && unifiable_types b b'
                 && unifiable_ok_stacks tail tail' ->
              Some (tinstr, Ok (Stack_ok (b :: tail)))
          | _ -> Some (tinstr, Error "LOOP_LEFT: incompatible body") )
      | _ -> None )
    | _ -> assert false
  in
  mk_spec "LOOP_LEFT" rule

let mi_lambda =
  let rule ~tparameter:_ stack = function
    | MIlambda (t_in, t_out, body) ->
        let body = body (Ok (Stack_ok [t_in])) in
        let tinstr = MIlambda (t_in, t_out, body) in
        let stack =
          let ok = Ok (Stack_ok (mt_lambda t_in t_out :: stack)) in
          match body.stack_out with
          | Ok (Stack_ok [t_out']) when unifiable_types t_out t_out' -> ok
          | Ok (Stack_ok _) -> Error "LAMBDA: non-singleton result stack"
          | Ok Stack_failed -> ok
          | Error s -> Error (Printf.sprintf "lambda stack error %s" s)
        in
        (tinstr, stack)
    | _ -> assert false
  in
  mk_spec_raw "LAMBDA" rule

let mi_map =
  let rule ~tparameter:_ stack instr =
    match (stack, instr) with
    | c :: tail, MImap body ->
        let wrap_v =
          match c.mt with
          | MT2 (T_map, k, v) -> Some (mt_map k, mt_pair k v)
          | MT1 (T_list, v) -> Some (mt_list, v)
          | _ -> None
        in
        ( match wrap_v with
        | None -> None
        | Some (wrap, v) ->
            let body = body (Ok (Stack_ok (v :: tail))) in
            let tinstr = MImap body in
            ( match body.stack_out with
            | Ok (Stack_ok (v' :: tail')) when unifiable_ok_stacks tail tail' ->
                Some (tinstr, Ok (Stack_ok (wrap v' :: tail)))
            | _ -> Some (tinstr, Error "MAP: incompatible body") ) )
    | [], MImap _ -> None
    | _ -> assert false
  in
  mk_spec "MAP" rule

let mi_pair ?annot_fst ?annot_snd () =
  mk_spec_basic "PAIR" ~arities:(2, 1) (function
      | a :: b :: _ -> Some [mt_pair ?annot_fst ?annot_snd a b]
      | _ -> None)

let mi_cons =
  mk_spec_basic "CONS" ~arities:(2, 1) (function
      | a :: {mt = MT1 (T_list, a')} :: _ ->
        ( match unify_types a a' with
        | Ok t -> Some [mt_list (strip_annots t)]
        | Error _ -> None )
      | _ -> None)

let mi_get =
  mk_spec_basic "GET" ~arities:(2, 1) (function
      | key :: {mt = MT2 (T_map, key', value)} :: _
        when unifiable_types key key' ->
          Some [mt_option value]
      | key :: {mt = MT2 (T_big_map, key', value)} :: _
        when unifiable_types key key' ->
          Some [mt_option value]
      | _ -> None)

let rec get_comb_type n = function
  | t when n = 0 -> Some t
  | {mt = MT2 (T_pair _, fst, _)} when n = 1 -> Some fst
  | {mt = MT2 (T_pair _, _, snd)} -> get_comb_type (n - 2) snd
  | _ -> None

let mi_getn n =
  mk_spec_basic "GET" ~arities:(1, 1) (function
      | t :: _ -> Option.map (fun t -> [t]) (get_comb_type n t)
      | [] -> None)

let mi_updaten n =
  mk_spec_basic "UPDATE" ~arities:(2, 1) (function
      | t1 :: t :: _ ->
        begin
          match get_comb_type n t with
          | None -> None
          | Some t2 -> if unifiable_types t1 t2 then Some [t] else None
        end
      | _ :: _ | [] -> None)

let mi_eq =
  mk_spec_basic "EQ" ~commutative:() ~arities:(1, 1) (function
      | {mt = MT0 T_int} :: _ -> Some [mt_bool]
      | _ -> None)

let mi_neq = {mi_eq with name = "NEQ"}

let mi_lt = {mi_neq with name = "LT"; commutative = false}

let mi_le = {mi_lt with name = "LE"}

let mi_gt = {mi_lt with name = "GT"}

let mi_ge = {mi_lt with name = "GE"}

let mi_neg =
  mk_spec_basic "NEG" ~arities:(1, 1) (function
      | {mt = MT0 T_nat} :: _ -> Some [mt_int]
      | {mt = MT0 T_int} :: _ -> Some [mt_int]
      | {mt = MT0 T_bls12_381_g1} :: _ -> Some [mt_bls12_381_g1]
      | {mt = MT0 T_bls12_381_g2} :: _ -> Some [mt_bls12_381_g2]
      | {mt = MT0 T_bls12_381_fr} :: _ -> Some [mt_bls12_381_fr]
      | _ -> None)

let mi_int =
  mk_spec_basic "INT" ~arities:(1, 1) (function
      | {mt = MT0 T_nat} :: _ -> Some [mt_int]
      | {mt = MT0 T_bls12_381_fr} :: _ -> Some [mt_int]
      | _ -> None)

let rec is_comparable mtype =
  match mtype.mt with
  | MT0
      ( T_unit | T_bool | T_nat | T_int | T_mutez | T_string | T_bytes
      | T_chain_id | T_timestamp | T_address | T_key | T_key_hash | T_signature
      | T_never ) ->
      true
  | MT1 (T_option, t) -> is_comparable t
  | MT2 (T_pair _, t1, t2) | MT2 (T_or _, t1, t2) ->
      is_comparable t1 && is_comparable t2
  | MT0
      ( T_operation | T_sapling_state _ | T_sapling_transaction _
      | T_bls12_381_g1 | T_bls12_381_g2 | T_bls12_381_fr | T_chest_key | T_chest
        )
   |MT1 ((T_list | T_set | T_contract | T_ticket), _)
   |MT2 ((T_lambda | T_map | T_big_map), _, _)
   |MT_var _ ->
      false

let mi_compare =
  mk_spec_basic "COMPARE" ~arities:(2, 1) (function
      | a :: b :: _
        when is_comparable a && is_comparable b && unifiable_types a b ->
          Some [mt_int]
      | _ -> None)

let mi_add =
  mk_spec_basic "ADD" ~commutative:() ~arities:(2, 1) (function
      | {mt = MT0 T_nat} :: {mt = MT0 T_nat} :: _ -> Some [mt_nat]
      | {mt = MT0 T_nat} :: {mt = MT0 T_int} :: _
       |{mt = MT0 T_int} :: {mt = MT0 T_nat} :: _
       |{mt = MT0 T_int} :: {mt = MT0 T_int} :: _ ->
          Some [mt_int]
      | {mt = MT0 T_timestamp} :: {mt = MT0 T_int} :: _
       |{mt = MT0 T_int} :: {mt = MT0 T_timestamp} :: _ ->
          Some [mt_timestamp]
      | {mt = MT0 T_mutez} :: {mt = MT0 T_mutez} :: _ -> Some [mt_mutez]
      | {mt = MT0 T_bls12_381_g1} :: {mt = MT0 T_bls12_381_g1} :: _ ->
          Some [mt_bls12_381_g1]
      | {mt = MT0 T_bls12_381_g2} :: {mt = MT0 T_bls12_381_g2} :: _ ->
          Some [mt_bls12_381_g2]
      | {mt = MT0 T_bls12_381_fr} :: {mt = MT0 T_bls12_381_fr} :: _ ->
          Some [mt_bls12_381_fr]
      | _ -> None)

let mi_sub =
  mk_spec_basic "SUB" ~arities:(2, 1) (function
      | {mt = MT0 T_int} :: {mt = MT0 T_int} :: _ -> Some [mt_int]
      | {mt = MT0 T_int} :: {mt = MT0 T_nat} :: _ -> Some [mt_int]
      | {mt = MT0 T_nat} :: {mt = MT0 T_int} :: _ -> Some [mt_int]
      | {mt = MT0 T_nat} :: {mt = MT0 T_nat} :: _ -> Some [mt_int]
      | {mt = MT0 T_mutez} :: {mt = MT0 T_mutez} :: _ -> Some [mt_mutez]
      | {mt = MT0 T_timestamp} :: {mt = MT0 T_int} :: _ -> Some [mt_timestamp]
      | {mt = MT0 T_timestamp} :: {mt = MT0 T_timestamp} :: _ -> Some [mt_int]
      | _ -> None)

let mi_mul =
  mk_spec_basic "MUL" ~arities:(2, 1) (function
      | {mt = MT0 T_int} :: {mt = MT0 T_int} :: _ -> Some [mt_int]
      | {mt = MT0 T_int} :: {mt = MT0 T_nat} :: _ -> Some [mt_int]
      | {mt = MT0 T_nat} :: {mt = MT0 T_int} :: _ -> Some [mt_int]
      | {mt = MT0 T_nat} :: {mt = MT0 T_nat} :: _ -> Some [mt_nat]
      | {mt = MT0 T_mutez} :: {mt = MT0 T_nat} :: _ -> Some [mt_mutez]
      | {mt = MT0 T_nat} :: {mt = MT0 T_mutez} :: _ -> Some [mt_mutez]
      | {mt = MT0 T_bls12_381_g1} :: {mt = MT0 T_bls12_381_fr} :: _ ->
          Some [mt_bls12_381_g1]
      | {mt = MT0 T_bls12_381_g2} :: {mt = MT0 T_bls12_381_fr} :: _ ->
          Some [mt_bls12_381_g2]
      | {mt = MT0 T_bls12_381_fr} :: {mt = MT0 T_bls12_381_fr} :: _ ->
          Some [mt_bls12_381_fr]
      | {mt = MT0 T_nat} :: {mt = MT0 T_bls12_381_fr} :: _ ->
          Some [mt_bls12_381_fr]
      | {mt = MT0 T_int} :: {mt = MT0 T_bls12_381_fr} :: _ ->
          Some [mt_bls12_381_fr]
      | {mt = MT0 T_bls12_381_fr} :: {mt = MT0 T_nat} :: _ ->
          Some [mt_bls12_381_fr]
      | {mt = MT0 T_bls12_381_fr} :: {mt = MT0 T_int} :: _ ->
          Some [mt_bls12_381_fr]
      | _ -> None)

let mi_ediv =
  mk_spec_basic "EDIV" ~arities:(2, 1) (function
      | {mt = MT0 T_int} :: {mt = MT0 T_int} :: _ ->
          Some [mt_option (mt_pair mt_int mt_nat)]
      | {mt = MT0 T_int} :: {mt = MT0 T_nat} :: _ ->
          Some [mt_option (mt_pair mt_int mt_nat)]
      | {mt = MT0 T_nat} :: {mt = MT0 T_int} :: _ ->
          Some [mt_option (mt_pair mt_int mt_nat)]
      | {mt = MT0 T_nat} :: {mt = MT0 T_nat} :: _ ->
          Some [mt_option (mt_pair mt_nat mt_nat)]
      | {mt = MT0 T_mutez} :: {mt = MT0 T_nat} :: _ ->
          Some [mt_option (mt_pair mt_mutez mt_mutez)]
      | {mt = MT0 T_mutez} :: {mt = MT0 T_mutez} :: _ ->
          Some [mt_option (mt_pair mt_nat mt_mutez)]
      | _ -> None)

let mi_and =
  mk_spec_basic "AND" ~commutative:() ~arities:(2, 1) (function
      | {mt = MT0 T_bool} :: {mt = MT0 T_bool} :: _ -> Some [mt_bool]
      | {mt = MT0 T_nat} :: {mt = MT0 T_nat} :: _ -> Some [mt_nat]
      | _ -> None)

let mi_or = {mi_and with name = "OR"}

let mi_xor = {mi_or with name = "XOR"}

let mi_unit = mk_spec_const "UNIT" mt_unit

let mi_nil t = mk_spec_const "NIL" (mt_list t)

let mi_empty_set t = mk_spec_const "EMPTY_SET" (mt_set t)

let mi_empty_map k v = mk_spec_const "EMPTY_MAP" (mt_map k v)

let mi_empty_big_map k v = mk_spec_const "EMPTY_BIG_MAP" (mt_big_map k v)

let mi_none t = mk_spec_const "NONE" (mt_option t)

let mi_push =
  let rule ~tparameter:_ stack = function
    | MIpush (t, l) ->
        let l = l t in
        let tinstr = MIpush (t, l) in
        let stack =
          match l.t with
          | Ok _ -> Ok (Stack_ok (t :: stack))
          | Error e -> Error e
        in
        (tinstr, stack)
    | _ -> assert false
  in
  mk_spec_raw "PUSH" ~arities:(0, 1) rule

let mi_some =
  mk_spec_basic "SOME" ~arities:(1, 1) (function
      | t :: _ -> Some [mt_option (strip_annots t)]
      | [] -> None)

let mi_left ?annot_left ?annot_right b =
  mk_spec_basic "LEFT" ~arities:(1, 1) (function
      | a :: _ -> Some [mt_or ?annot_left ?annot_right (strip_annots a) b]
      | [] -> None)

let mi_right ?annot_left ?annot_right a =
  mk_spec_basic "RIGHT" ~arities:(1, 1) (function
      | b :: _ -> Some [mt_or ?annot_left ?annot_right a (strip_annots b)]
      | [] -> None)

(** Select the part of the type designated by the ad_path. *)
let rec ad_path_in_type ops t =
  match (ops, t) with
  | [], _ -> Some t
  | A :: p, {mt = MT2 (T_pair _, fst, _)} -> ad_path_in_type p fst
  | D :: p, {mt = MT2 (T_pair _, _, snd)} -> ad_path_in_type p snd
  | _ :: _, _ -> None

let mi_field steps =
  mk_spec_basic
    ~arities:(1, 1)
    (sprintf "C%sR" (string_of_ad_path steps))
    (function
      | t :: _ ->
        ( match ad_path_in_type steps t with
        | None -> None
        | Some t -> Some [t] )
      | [] -> None)

let mi_set_field steps =
  mk_spec_basic
    ~arities:(2, 1)
    (sprintf "SET_C%sR" (string_of_ad_path steps))
    (function
      | t :: x :: _ ->
        ( match ad_path_in_type steps t with
        | Some x' when unifiable_types x x' -> Some [t]
        | _ -> None )
      | _ -> None)

let mi_update =
  mk_spec_basic "UPDATE" ~arities:(3, 1) (function
      | k :: {mt = MT0 T_bool} :: {mt = MT1 (T_set, k')} :: _ ->
          Base.Option.map
            ~f:(fun k -> [mt_set k])
            (Base.Result.ok (unify_types k k'))
      | k :: {mt = MT1 (T_option, v)} :: {mt = MT2 (T_map, k', v')} :: _ ->
          Option.map2
            (fun k v -> [mt_map k v])
            (Base.Result.ok (unify_types k k'))
            (Base.Result.ok (unify_types v v'))
      | k :: {mt = MT1 (T_option, v)} :: {mt = MT2 (T_big_map, k', v')} :: _ ->
          Option.map2
            (fun k v -> [mt_big_map k v])
            (Base.Result.ok (unify_types k k'))
            (Base.Result.ok (unify_types v v'))
      | _ -> None)

let mi_get_and_update =
  mk_spec_basic "GET_AND_UPDATE" ~arities:(3, 2) (function
      | k :: {mt = MT1 (T_option, v)} :: {mt = MT2 (T_map, k', v')} :: _ ->
          Option.map2
            (fun k v -> [mt_option v; mt_map k v])
            (Base.Result.ok (unify_types k k'))
            (Base.Result.ok (unify_types v v'))
      | k :: {mt = MT1 (T_option, v)} :: {mt = MT2 (T_big_map, k', v')} :: _ ->
          Option.map2
            (fun k v -> [mt_option v; mt_big_map k v])
            (Base.Result.ok (unify_types k k'))
            (Base.Result.ok (unify_types v v'))
      | _ -> None)

let mi_open_chest =
  mk_spec_basic "OPEN_CHEST" ~arities:(3, 1) (function
      | {mt = MT0 T_chest_key} :: {mt = MT0 T_chest} :: {mt = MT0 T_nat} :: _ ->
          Some [mt_or mt_bytes mt_bool]
      | _ -> None)

let mi_mem =
  mk_spec_basic "MEM" ~arities:(2, 1) (function
      | k :: {mt = MT1 (T_set, k')} :: _ when unifiable_types k k' ->
          Some [mt_bool]
      | k :: {mt = MT2 (T_map, k', _)} :: _ when unifiable_types k k' ->
          Some [mt_bool]
      | k :: {mt = MT2 (T_big_map, k', _)} :: _ when unifiable_types k k' ->
          Some [mt_bool]
      | _ -> None)

let mi_exec =
  mk_spec_basic "EXEC" ~arities:(2, 1) (function
      | k :: {mt = MT2 (T_lambda, k', v)} :: _ ->
          if unifiable_types k k' then Some [v] else None
      | _ -> None)

let mi_apply =
  mk_spec_basic "APPLY" ~arities:(2, 1) (function
      | k :: {mt = MT2 (T_lambda, {mt = MT2 (T_pair _, k', k'')}, v)} :: _
        when unifiable_types k k' ->
          Some [mt_lambda k'' v]
      | _ -> None)

let mi_contract t =
  mk_spec_basic "CONTRACT" ~arities:(1, 1) (function
      | {mt = MT0 T_address} :: _ -> Some [mt_option (mt_contract t)]
      | _ -> None)

let mi_view t =
  mk_spec_basic "VIEW" ~arities:(2, 1) (function
      | _ :: {mt = MT0 T_address} :: _ -> Some [mt_option t]
      | _ -> None)

let mi_cast t =
  mk_spec_basic "CAST" ~arities:(1, 1) (function
      | t' :: _ when unifiable_types t t' -> Some [t]
      | _ -> None)

let mi_rename annot_variable =
  mk_spec_basic "RENAME" ~arities:(1, 1) (function
      | t :: _ -> Some [{t with annot_variable}]
      | _ -> None)

let mi_transfer_tokens =
  mk_spec_basic "TRANSFER_TOKENS" ~arities:(3, 1) (function
      | p :: {mt = MT0 T_mutez} :: {mt = MT1 (T_contract, p')} :: _
        when unifiable_types p p' ->
          Some [mt_operation]
      | _ -> None)

let mi_set_delegate =
  mk_spec_basic "SET_DELEGATE" ~arities:(1, 1) (function
      | {mt = MT1 (T_option, {mt = MT0 T_key_hash})} :: _ -> Some [mt_operation]
      | _ -> None)

let mi_sapling_verify_update =
  mk_spec_basic "SAPLING_VERIFY_UPDATE" ~arities:(2, 1) (function
      | {mt = MT0 (T_sapling_transaction {memo = m1})}
        :: {mt = MT0 (T_sapling_state {memo = m2})} :: _
        when m1 = m2 ->
          Some [mt_option (mt_pair mt_int (mt_sapling_state m1))]
      | _ -> None)

let mi_concat1 =
  mk_spec_basic "CONCAT" ~arities:(1, 1) (function
      | {mt = MT1 (T_list, {mt = MT0 (T_string | T_bytes) as mt})} :: _ ->
          Some [mk_mtype mt]
      | _ -> None)

let mi_concat2 =
  mk_spec_basic "CONCAT" ~arities:(2, 1) (function
      | {mt = MT0 T_string as mt} :: {mt = MT0 T_string} :: _
       |{mt = MT0 T_bytes as mt} :: {mt = MT0 T_bytes} :: _ ->
          Some [mk_mtype mt]
      | _ -> None)

let mi_concat_unresolved =
  let rule ~tparameter stack instr =
    let _instr1, r1 = mi_concat1.rule ~tparameter stack instr in
    match r1 with
    | Ok _ -> (MI1 Concat1, r1)
    | Error _ ->
        let _instr2, r2 = mi_concat2.rule ~tparameter stack instr in
        (MI2 Concat2, r2)
  in
  {name = "CONCAT"; rule; commutative = false; arities = None}

let mi_pack =
  mk_spec_basic "PACK" ~arities:(1, 1) (function
      | _ :: _ -> Some [mt_bytes]
      | [] -> None)

let mi_unpack t =
  mk_spec_basic "UNPACK" ~arities:(1, 1) (function
      | {mt = MT0 T_bytes} :: _ -> Some [mt_option t]
      | _ -> None)

let mi_slice =
  mk_spec_basic "SLICE" ~arities:(3, 1) (function
      | {mt = MT0 T_nat} :: {mt = MT0 T_nat} :: {mt = MT0 T_string} :: _ ->
          Some [mt_option mt_string]
      | {mt = MT0 T_nat} :: {mt = MT0 T_nat} :: {mt = MT0 T_bytes} :: _ ->
          Some [mt_option mt_bytes]
      | _ -> None)

let mi_size =
  mk_spec_basic "SIZE" ~arities:(1, 1) (function
      | { mt =
            ( MT0 T_string
            | MT0 T_bytes
            | MT1 ((T_set | T_list), _)
            | MT2 ((T_map | T_big_map), _, _) ) }
        :: _ ->
          Some [mt_nat]
      | _ -> None)

let mi_mich ~name ~types_in ~types_out =
  mk_spec_basic
    name
    ~arities:(List.length types_in, List.length types_out)
    (fun stack ->
      if Base.List.is_prefix ~prefix:types_in stack ~equal:equal_mtype
      then Some types_out
      else None)

let mi_self =
  let rule ~tparameter stack = function
    | MI0 (Self ep_name) ->
        let tinstr = MI0 (Self ep_name) in
        let mt_contract t =
          {(mt_contract t) with annot_variable = Some "self"}
        in
        ( match ep_name with
        | None ->
            Some (tinstr, Ok (Stack_ok (mt_contract (fst tparameter) :: stack)))
        | Some ep_name ->
            let rec find_ep (t, annot) =
              match (t.mt, annot) with
              | _, Some a when a = ep_name -> Some t
              | MT2 (T_or {annot_left; annot_right}, left, right), None ->
                ( match find_ep (left, annot_left) with
                | Some t -> Some t
                | None -> find_ep (right, annot_right) )
              | _ -> None
            in
            ( match find_ep tparameter with
            | None -> None
            | Some t -> Some (tinstr, Ok (Stack_ok (mt_contract t :: stack))) )
        )
    | _ -> assert false
  in
  mk_spec "SELF" ~arities:(0, 1) rule

let mi_address =
  mk_spec_basic "ADDRESS" ~arities:(1, 1) (function
      | {mt = MT1 (T_contract, _)} :: _ -> Some [mt_address]
      | _ -> None)

let mi_implicit_account =
  mk_spec_basic "IMPLICIT_ACCOUNT" ~arities:(1, 1) (function
      | {mt = MT0 T_key_hash} :: _ -> Some [mt_contract mt_unit]
      | _ -> None)

let mi_voting_power =
  mk_spec_basic "VOTING_POWER" ~arities:(1, 1) (function
      | {mt = MT0 T_key_hash} :: _ -> Some [mt_nat]
      | _ -> None)

let mi_create_contract =
  let rule ~tparameter:_ stack = function
    | MIcreate_contract {tparameter; tstorage; code} ->
      ( match stack with
      | {mt = MT1 (T_option, {mt = MT0 T_key_hash})}
        :: {mt = MT0 T_mutez} :: storage :: tail ->
          let code =
            code (Ok (initial_stack ~tparameter:(fst tparameter) ~tstorage))
          in
          if unifiable_types storage tstorage
          then
            let tinstr = MIcreate_contract {tparameter; tstorage; code} in
            let stack = Ok (Stack_ok (mt_operation :: mt_address :: tail)) in
            Some (tinstr, stack)
          else None
      | _ -> None )
    | _ -> assert false
  in
  mk_spec "CREATE_CONTRACT" ~arities:(3, 2) rule

let spec_of_prim0 p =
  let mk name =
    let t = Michelson_base.Typing.type_prim0 p in
    mk_spec_const name t
  in
  match p with
  | Sender -> mk "SENDER"
  | Source -> mk "SOURCE"
  | Amount -> mk "AMOUNT"
  | Balance -> mk "BALANCE"
  | Chain_id -> mk "CHAIN_ID"
  | Level -> mk "LEVEL"
  | Now -> mk "NOW"
  | Total_voting_power -> mk "TOTAL_VOTING_POWER"
  | Self_address -> mk "SELF_ADDRESS"
  | Sapling_empty_state _ -> mk "SAPLING_EMPTY_STATE"
  | Nil t -> mi_nil t
  | Empty_set t -> mi_empty_set t
  | Empty_bigmap (k, v) -> mi_empty_big_map k v
  | Empty_map (k, v) -> mi_empty_map k v
  | None_ t -> mi_none t
  | Unit_ -> mi_unit
  | Self _ -> mi_self

let spec_of_prim1 p =
  let mk name =
    let t1, t = Michelson_base.Typing.type_prim1 p in
    let f = function
      | x :: _ when unifiable_types x t1 -> Some [t]
      | _ -> None
    in
    mk_spec_basic name ~arities:(1, 1) f
  in
  match p with
  | Hash_key -> mk "HASH_KEY"
  | IsNat -> mk "ISNAT"
  | Blake2b -> mk "BLAKE2B"
  | Sha256 -> mk "SHA256"
  | Sha512 -> mk "SHA512"
  | Keccak -> mk "KECCAK"
  | Sha3 -> mk "SHA3"
  | Abs -> mk "ABS"
  | Not -> mk "NOT"
  | Contract (_, t) -> mi_contract t
  | Cast t -> mi_cast t
  | Rename a -> mi_rename a
  | Concat1 -> mi_concat1
  | Set_delegate -> mi_set_delegate
  | Read_ticket -> mi_read_ticket
  | Join_tickets -> mi_join_tickets
  | Pairing_check -> mi_pairing_check
  | Eq -> mi_eq
  | Neq -> mi_neq
  | Lt -> mi_lt
  | Le -> mi_le
  | Gt -> mi_gt
  | Ge -> mi_ge
  | Neg -> mi_neg
  | Int -> mi_int
  | Some_ -> mi_some
  | Left (annot_left, annot_right, t) -> mi_left ?annot_left ?annot_right t
  | Right (annot_left, annot_right, t) -> mi_right ?annot_left ?annot_right t
  | Pack -> mi_pack
  | Unpack t -> mi_unpack t
  | Getn n -> mi_getn n
  | Address -> mi_address
  | Implicit_account -> mi_implicit_account
  | Voting_power -> mi_voting_power
  | Size -> mi_size
  | Car | Cdr -> assert false

let spec_of_prim2 p =
  let mk name =
    let t1, t2, t = Michelson_base.Typing.type_prim2 p in
    let f = function
      | x1 :: x2 :: _ when unifiable_types x1 t1 && unifiable_types x2 t2 ->
          Some [t]
      | _ -> None
    in
    mk_spec_basic name ~arities:(2, 1) f
  in
  match p with
  | Lsl -> mk "LSL"
  | Lsr -> mk "LSR"
  | Add -> mi_add
  | Sub -> mi_sub
  | Mul -> mi_mul
  | Ediv -> mi_ediv
  | Concat2 -> mi_concat2
  | Sapling_verify_update -> mi_sapling_verify_update
  | Ticket -> mi_ticket
  | Split_ticket -> mi_split_ticket
  | Compare -> mi_compare
  | Pair (annot_fst, annot_snd) -> mi_pair ?annot_fst ?annot_snd ()
  | Cons -> mi_cons
  | And -> mi_and
  | Or -> mi_or
  | Xor -> mi_xor
  | Get -> mi_get
  | Updaten n -> mi_updaten n
  | Mem -> mi_mem
  | Exec -> mi_exec
  | Apply -> mi_apply
  | View (_, t) -> mi_view t

let spec_of_prim3 p =
  let mk name =
    let t1, t2, t3, t = Michelson_base.Typing.type_prim3 p in
    let f = function
      | x1 :: x2 :: x3 :: _
        when unifiable_types x1 t1
             && unifiable_types x2 t2
             && unifiable_types x3 t3 ->
          Some [t]
      | _ -> None
    in
    mk_spec_basic name ~arities:(3, 1) f
  in
  match p with
  | Check_signature -> mk "CHECK_SIGNATURE"
  | Slice -> mi_slice
  | Transfer_tokens -> mi_transfer_tokens
  | Update -> mi_update
  | Get_and_update -> mi_get_and_update
  | Open_chest -> mi_open_chest

let spec_of_instr ~strict_dup = function
  | MI0 p -> spec_of_prim0 p
  | MI1 p -> spec_of_prim1 p
  | MI2 p -> spec_of_prim2 p
  | MI3 p -> spec_of_prim3 p
  | MI1_fail Failwith -> mi_failwith
  | MI1_fail Never -> mi_never
  | MIpush _ -> mi_push
  | MIconcat_unresolved -> mi_concat_unresolved
  | MIswap -> mi_swap
  | MIdrop -> mi_drop
  | MIdropn n -> mi_dropn n
  | MIunpair n -> mi_unpair n
  | MIpairn n -> mi_pairn n
  | MIfield steps -> mi_field steps
  | MIsetField steps -> mi_set_field steps
  | MIdup i -> mi_dup ~strict_dup i
  | MIcreate_contract _ -> mi_create_contract
  | MImich {name; typesIn; typesOut} ->
      mi_mich ~name ~types_in:typesIn ~types_out:typesOut
  | MIdip _ -> mi_dip
  | MIdipn _ -> mi_dipn
  | MIiter _ -> mi_iter
  | MImap _ -> mi_map
  | MIloop _ -> mi_loop
  | MIloop_left _ -> mi_loop_left
  | MIif _ -> mi_if
  | MIif_none _ -> mi_if_none
  | MIif_left _ -> mi_if_left
  | MIif_cons _ -> mi_if_cons
  | MIdig n -> mi_dig n
  | MIdug n -> mi_dug n
  | MIseq _ -> mi_seq
  | MIlambda _ -> mi_lambda
  | MIcomment _ -> mi_comment
  | MIerror s -> mi_error s

let is_commutative instr = (spec_of_instr ~strict_dup:true instr).commutative

let name_of_instr instr = (spec_of_instr ~strict_dup:true instr).name

(** {1 Type checking} *)

(* Recognize lists of Elts and lists of Instrs. *)
let sanitize_instr, sanitize_literal =
  let f_literal : _ literal_f -> _ = function
    | Seq ({literal = Elt _} :: _ as xs) ->
        let f = function
          | {literal = Elt (k, v)} -> (k, v)
          | _ -> failwith "sanitize: Elt followed by non-Elt"
        in
        {literal = AnyMap (List.map f xs)}
    | Seq ({literal = Instr _} :: _ as xs) ->
        let f = function
          | {literal = Instr i} -> i
          | _ -> failwith "sanitize: instruction followed by non-instruction"
        in
        {literal = Instr {instr = MIseq (List.map f xs)}}
    | literal -> {literal}
  in
  let f_instr instr = {instr} in
  cata {f_instr; f_literal}

(* Match a comb type against the given tuple. *)
let rec match_comb t xs =
  match (t.mt, xs) with
  | _, [x] -> [(t, x)]
  | MT2 (T_pair _, fst, snd), x :: xs -> (fst, x) :: match_comb snd xs
  | _ -> failwith "match_comb"

(* Roll a list into a right comb. *)
let rec comb_literal = function
  | [x1; x2] -> {tliteral = Pair (x1, x2); t = Result.map2 mt_pair x1.t x2.t}
  | x :: xs ->
      let xs = comb_literal xs in
      {tliteral = Pair (x, xs); t = Result.map2 mt_pair x.t xs.t}
  | _ -> assert false

let typecheck_literal_f
    ~strict_dup:_ ~tparameter:_ (literal : _ literal_f) (t : mtype Result.t) =
  let tliteral' :
      (stack Result.t -> tinstr, mtype Result.t -> tliteral) literal_f =
    map_literal_f snd snd literal
  in
  let r =
    let open Result in
    let* t = map_error (fun _ -> "type error") t in
    match (t.mt, tliteral') with
    | _, (Constant _ as l) -> return l
    | MT0 T_unit, (Unit as l)
     |MT0 T_bool, (Bool _ as l)
     |MT0 T_nat, (Int _ as l)
     |MT0 T_int, (Int _ as l)
     |MT0 T_mutez, (Int _ as l)
     |MT0 T_string, (String _ as l)
     |MT0 T_bytes, (Bytes _ as l)
     |MT0 T_chain_id, (Bytes _ as l)
     |MT0 T_timestamp, ((Int _ | String _) as l)
     |MT0 T_address, ((Bytes _ | String _) as l)
     |MT0 T_key, ((Bytes _ | String _) as l)
     |MT0 T_key_hash, ((Bytes _ | String _) as l)
     |MT0 T_signature, ((Bytes _ | String _) as l)
     |MT0 (T_sapling_state _), (Seq [] as l)
     |MT0 (T_sapling_transaction _), (String "FAKE_SAPLING_TRANSACTION" as l)
     |MT0 T_bls12_381_g1, (Bytes _ as l)
     |MT0 T_bls12_381_g2, (Bytes _ as l)
     |MT0 T_bls12_381_fr, (Bytes _ as l)
     |MT0 T_chest_key, (Bytes _ as l)
     |MT0 T_chest, (Bytes _ as l) ->
        return l
    | MT1 (T_contract, _), (String _ as l) ->
        (* needed for scenarios: "Contract_*" *) return l
    | MT2 (T_pair _, fst, snd), Pair (x1, x2) ->
        return (Pair (x1 (Ok fst), x2 (Ok snd)))
    | MT2 (T_pair _, _, _), Seq xs ->
        let xs = List.map (fun (t, x) -> x (Ok t)) (match_comb t xs) in
        return (comb_literal xs).tliteral
    | MT2 (T_or _, left, _), Left x -> return (Left (x (Ok left)))
    | MT2 (T_or _, _, right), Right x -> return (Right (x (Ok right)))
    | MT1 (T_option, t), Some_ x -> return (Some_ (x (Ok t)))
    | MT1 (T_option, _), None_ -> return None_
    | MT1 ((T_set | T_list), t), Seq xs ->
        return (Seq (List.map (fun x -> x (Ok t)) xs))
    | MT2 ((T_map | T_big_map), _tk, _tv), Seq [] -> return (AnyMap [])
    | MT2 ((T_map | T_big_map), _tk, _tv), Seq (_ :: _) ->
        assert false (* eliminated by 'sanitize' *)
    | MT2 ((T_map | T_big_map), tk, tv), AnyMap xs ->
        let f (k, v) = (k (Ok tk), v (Ok tv)) in
        return (AnyMap (List.map f xs))
    | MT2 (T_lambda, t_in, _), Instr i ->
        return (Instr (i (Ok (Stack_ok [t_in]))))
    | MT2 (T_lambda, _t1, _), Seq (_ :: _) ->
        assert false (* eliminated by 'sanitize' *)
    | ( ( MT0
            ( T_unit | T_bool | T_nat | T_int | T_mutez | T_string | T_bytes
            | T_chain_id | T_timestamp | T_address | T_key | T_key_hash
            | T_signature | T_operation | T_sapling_state _
            | T_sapling_transaction _ | T_never | T_bls12_381_g1
            | T_bls12_381_g2 | T_bls12_381_fr | T_chest_key | T_chest )
        | MT1 ((T_option | T_list | T_set | T_contract | T_ticket), _)
        | MT2 ((T_pair _ | T_or _ | T_lambda | T_map | T_big_map), _, _)
        | MT_var _ )
      , ( Int _ | Bool _ | String _ | Bytes _ | Unit | Pair _ | None_ | Left _
        | Right _ | Some_ _ | Seq _ | Elt _ | AnyMap _ | Instr _ ) ) ->
        let msg =
          let literal = map_literal_f fst fst literal in
          let l = MLiteral.to_michelson_string show_instr {literal} in
          let t = string_of_mtype ~html:false t in
          sprintf "Literal %s does not have type %s." l t
        in
        error msg
  in
  match r with
  | Ok tliteral ->
      let r =
        sequence_literal_f
          (map_literal_f (fun {stack_out} -> stack_out) (fun {t} -> t) tliteral)
      in
      let t =
        match Result.get_error r with
        | Some error -> Error (Printf.sprintf "type error in literal %s" error)
        | None -> t
      in
      {tliteral; t}
  | Error msg ->
      let err = Error "type error in literal" in
      let tliteral =
        map_literal_f (fun f -> f err) (fun f -> f err) tliteral'
      in
      {tliteral; t = Error msg}

let typecheck_instr_f ~strict_dup ~tparameter i (stack_in : stack Result.t) :
    tinstr =
  let i : (stack Result.t -> tinstr, mtype Result.t -> tliteral) instr_f =
    map_instr_f snd snd i
  in
  let on_error_stack stack_in msg =
    let err = Error "outer error" in
    let tinstr = map_instr_f (fun x -> x err) (fun x -> x err) i in
    {tinstr; stack_in; stack_out = Error msg}
  in
  match (stack_in, i) with
  | _, (MIcomment _ as tinstr) -> {tinstr; stack_in; stack_out = stack_in}
  | Ok (Stack_ok stack), _ ->
      let {rule} = spec_of_instr ~strict_dup i in
      let okify f x = f (Ok x) in
      let tinstr, stack_out = rule ~tparameter stack (map_instr_f id okify i) in
      {tinstr; stack_in; stack_out}
  | Ok Stack_failed, _ -> on_error_stack stack_in "instruction on failed stack"
  | Error _, _ -> on_error_stack stack_in "previous error"

let typecheck_alg ~strict_dup ~tparameter =
  let p_instr = typecheck_instr_f ~strict_dup ~tparameter in
  let p_literal = typecheck_literal_f ~strict_dup ~tparameter in
  para_alg ~p_instr ~p_literal

let typecheck_instr ~strict_dup ~tparameter stack i =
  let i = sanitize_instr i in
  snd (cata_instr (typecheck_alg ~strict_dup ~tparameter) i) (Ok stack)

let typecheck_literal ~strict_dup ~tparameter t l =
  let l = sanitize_literal l in
  snd (cata_literal (typecheck_alg ~strict_dup ~tparameter) l) (Ok t)

let has_error ~accept_missings =
  let has_missing_type t = if accept_missings then [] else has_missing_type t in
  let f_tinstr ~stack_in:_ ~stack_out instr =
    match stack_out with
    | Error s -> [s]
    | Ok _ ->
      ( match instr with
      | MIerror s -> [s]
      | MI0 (Nil t)
       |MI0 (Empty_set t)
       |MI0 (None_ t)
       |MI1 (Left (_, _, t))
       |MI1 (Right (_, _, t))
       |MI1 (Contract (_, t))
       |MI1 (Unpack t) ->
          has_missing_type t
      | MI0 (Empty_bigmap (k, v) | Empty_map (k, v)) ->
          has_missing_type k @ has_missing_type v
      | MIpush (t, _) -> has_missing_type t
      | MImich {typesIn; typesOut} ->
          Base.List.concat_map ~f:has_missing_type typesIn
          @ Base.List.concat_map ~f:has_missing_type typesOut
      | MIlambda (t1, t2, _i) as x ->
          has_missing_type t1
          @ has_missing_type t2
          @ fold_instr_f ( @ ) (curry fst) [] x
      | x -> fold_instr_f ( @ ) (curry fst) [] x )
  in
  let f_tliteral ~t _ =
    match t with
    | Error msg -> [msg]
    | Ok t -> has_missing_type t
  in
  cata_tinstr {f_tinstr; f_tliteral}

let name_of_instr_exn = function
  | ( MI0
        ( Sender | Source | Amount | Balance | Level | Now | Self _
        | Self_address | Chain_id | Total_voting_power | Sapling_empty_state _
        | Unit_ | None_ _ | Nil _ | Empty_set _ | Empty_map _ | Empty_bigmap _
          )
    | MI1
        ( Car | Cdr | Left _ | Right _ | Some_ | Eq | Abs | Neg | Int | IsNat
        | Neq | Le | Lt | Ge | Gt | Not | Concat1 | Size | Address
        | Implicit_account | Contract _ | Pack | Unpack _ | Hash_key | Blake2b
        | Sha256 | Sha512 | Keccak | Sha3 | Set_delegate | Read_ticket
        | Join_tickets | Pairing_check | Voting_power | Getn _ | Cast _
        | Rename _ )
    | MI1_fail (Failwith | Never)
    | MI2
        ( Pair _ | Add | Mul | Sub | Lsr | Lsl | Xor | Ediv | And | Or | Cons
        | Compare | Concat2 | Get | Mem | Exec | Apply | Sapling_verify_update
        | Ticket | Split_ticket | Updaten _ | View _ )
    | MIdrop | MIswap
    | MI3
        ( Slice | Update | Get_and_update | Transfer_tokens | Check_signature
        | Open_chest )
    | MImich _ | MIdropn _ | MIdup _ | MIpush _ | MIunpair _ | MIpairn _
    | MIfield _ | MIsetField _ | MIconcat_unresolved ) as instr ->
      name_of_instr instr
  | MIdip _ -> "DIP"
  | MIdipn _ -> "DIPN"
  | MIloop _ -> "LOOP"
  | MIloop_left _ -> "LOOP_LEFT"
  | MIiter _ -> "ITER"
  | MImap _ -> "MAP"
  | MIif_left _ -> "IF_LEFT"
  | MIif_none _ -> "IF_NONE"
  | MIif_cons _ -> "IF_CONS"
  | MIif _ -> "IF"
  | MIdig _ -> "DIG"
  | MIdug _ -> "DUG"
  | MIlambda _ -> "LAMBDA"
  | MIerror _ -> failwith "name_of_instr_exn: MIerror"
  | MIcomment _ -> failwith "name_of_instr_exn: MIcomment"
  | MIseq _ -> failwith "name_of_instr_exn: MIseq"
  | MIcreate_contract _ -> "CREATE_CONTRACT"

let two_field_annots = function
  | Some a1, Some a2 -> ["%" ^ a1; "%" ^ a2]
  | Some a1, None -> ["%" ^ a1]
  | None, Some a2 -> ["%"; "%" ^ a2]
  | None, None -> []

let buffer_f ~html ~show_stack ~new_line b =
  let new_line = if new_line then "\n" else "" in
  let f_tliteral ~t:_ x indent protect =
    let prot = Misc.buffer_protect b protect "(" ")" in
    let elt (k, v) =
      Buffer.add_string b "Elt ";
      k indent true;
      Buffer.add_string b " ";
      v indent true
    in
    let sub1 name x =
      prot (fun () ->
          Buffer.add_string b name;
          Buffer.add_string b " ";
          x indent true)
    in
    match x with
    | Int i -> Buffer.add_string b (Big_int.string_of_big_int i)
    | Unit -> Buffer.add_string b "Unit"
    | String s -> bprintf b "%S" s
    | Bool true -> Buffer.add_string b "True"
    | Bool false -> Buffer.add_string b "False"
    | Pair (l, r) ->
        prot (fun () ->
            Buffer.add_string b "Pair ";
            l indent true;
            Buffer.add_string b " ";
            r indent true)
    | None_ -> Buffer.add_string b "None"
    | Left x -> sub1 "Left" x
    | Right x -> sub1 "Right" x
    | Some_ x -> sub1 "Some" x
    | Bytes string_bytes ->
        Buffer.add_string b "0x";
        Buffer.add_string b (Misc.Hex.hexcape string_bytes)
    | Seq xs ->
        Misc.buffer_protect b true "{" "}" (fun () ->
            Misc.buffer_concat b "; " (fun x -> x indent false) xs)
    | Elt (k, v) -> elt (k, v)
    | AnyMap xs ->
        Misc.buffer_protect b true "{" "}" (fun () ->
            Misc.buffer_concat b "; " elt xs)
    | Instr i -> i ~sub:false (indent + 2)
    | Constant hash -> bprintf b "(constant %S)" hash
  in
  let f_tinstr ~stack_in:_ ~stack_out x ~sub indent =
    let spaces n =
      if html
      then String.concat "" (List.init n (fun _ -> "&nbsp;"))
      else String.make n ' '
    in
    let out x = kbprintf (fun _ -> ()) b x in
    let out_str x = Buffer.add_string b x in
    let do_indent () =
      Buffer.add_string b (spaces (if new_line = "" then 1 else indent))
    in
    let line x =
      do_indent ();
      kbprintf (fun _ -> out_str new_line) b x
    in
    let line_str x =
      do_indent ();
      out_str x;
      out_str new_line
    in
    let with_stack s =
      do_indent ();
      let l1 = Buffer.length b in
      s ();
      let l2 = Buffer.length b in
      out ";";
      if show_stack then out_str (spaces (max 0 (10 - (l2 - l1))))
    in
    let span className text =
      if html then sprintf "<span class='%s'>%s</span>" className text else text
    in
    let sub1 name code =
      line_str name;
      code ~sub:true (indent + 2);
      out_str ";"
    in
    let sub2 name l r =
      line_str name;
      l ~sub:true (indent + 2);
      out_str new_line;
      r ~sub:true (indent + 2);
      out_str ";"
    in
    ( match x with
    | MIseq [] ->
        do_indent ();
        out_str "{}"
    | MIseq l ->
        do_indent ();
        out_str "{";
        List.iter
          (fun i ->
            out_str new_line;
            i ~sub:false (indent + 2))
          l;
        out_str new_line;
        do_indent ();
        out_str "}";
        if not sub then out_str ";"
    | MIdip code -> sub1 "DIP" code
    | MIdipn (n, code) -> sub1 (sprintf "DIP %i" n) code
    | MIloop code -> sub1 "LOOP" code
    | MIloop_left code -> sub1 "LOOP_LEFT" code
    | MIiter code -> sub1 "ITER" code
    | MImap code -> sub1 "MAP" code
    | MIif_left (l, r) -> sub2 "IF_LEFT" l r
    | MIif_none (l, r) -> sub2 "IF_NONE" l r
    | MIif_cons (l, r) -> sub2 "IF_CONS" l r
    | MIif (l, r) -> sub2 "IF" l r
    | MIcomment comments ->
        let lines =
          List.concat (List.map (String.split_on_char '\n') comments)
        in
        List.iteri
          (fun i line ->
            if i <> 0 then out_str new_line;
            do_indent ();
            out_str (span "comment" (sprintf "# %s" line)))
          lines
    | MIdig n -> with_stack (fun () -> out "DIG %d" n)
    | MIdug n -> with_stack (fun () -> out "DUG %d" n)
    | MIdropn n -> with_stack (fun () -> out "DROP %d" n)
    | MIdup 1 -> with_stack (fun () -> out_str "DUP")
    | MIunpair [true; true] -> with_stack (fun () -> out_str "UNPAIR")
    | MI0 (Sapling_empty_state {memo}) ->
        with_stack (fun () -> out "SAPLING_EMPTY_STATE %d" memo)
    | MIdup n -> with_stack (fun () -> out "DUP %d" n)
    | MIunpair n -> with_stack (fun () -> out "UNPAIR %s" (unpair_arg n))
    | MIpairn n -> with_stack (fun () -> out "PAIR %d" n)
    | MI1 (Getn n) -> with_stack (fun () -> out "GET %d" n)
    | MI2 (Updaten n) -> with_stack (fun () -> out "UPDATE %d" n)
    | MI2 (View (name, t)) ->
        with_stack (fun () ->
            out "VIEW %S %s" name (string_of_mtype ~protect:() ~html t))
    | MIerror error ->
        do_indent ();
        out_str (span "partialType" (sprintf "MIerror: %s" error))
    | MI0 (Empty_set t)
     |MI0 (Nil t)
     |MI0 (None_ t)
     |MI1 (Contract (None, t))
     |MI1 (Unpack t)
     |MI1 (Cast t) ->
        with_stack (fun () ->
            out_str (name_of_instr_exn x);
            out_str " ";
            buffer_mtype ~html ~protect:() b t)
    | MI1 (Rename a) ->
        with_stack (fun () ->
            out_str (name_of_instr_exn x);
            match a with
            | None -> ()
            | Some a ->
                out_str " @";
                out_str a)
    | MI0 (Empty_bigmap (k, v) | Empty_map (k, v)) ->
        with_stack (fun () ->
            out_str (name_of_instr_exn x);
            out_str " ";
            out_str
              (String.concat
                 " "
                 [ string_of_mtype ~protect:() ~html k
                 ; string_of_mtype ~protect:() ~html v ]))
    | MI1 (Left (a1, a2, t) | Right (a1, a2, t)) ->
        with_stack (fun () ->
            out_str
              (String.concat
                 " "
                 (name_of_instr_exn x :: two_field_annots (a1, a2)));
            out_str " ";
            out_str (string_of_mtype ~protect:() ~html t))
    | MI2 (Pair (a1, a2)) ->
        with_stack (fun () ->
            out_str
              (String.concat
                 " "
                 (name_of_instr_exn x :: two_field_annots (a1, a2))))
    | MI1 (Contract (Some ep, t)) ->
        with_stack (fun () ->
            out "CONTRACT %%%s %s" ep (string_of_mtype ~protect:() ~html t))
    | MIpush (t, l) ->
        with_stack (fun () ->
            out "PUSH %s " (string_of_mtype ~protect:() ~html t);
            l indent true)
    | MI0 (Self (Some entrypoint)) ->
        with_stack (fun () -> out "SELF %%%s" entrypoint)
    | MIlambda (t1, t2, l) ->
        line "LAMBDA";
        line "  %s" (string_of_mtype ~protect:() ~html t1);
        line "  %s" (string_of_mtype ~protect:() ~html t2);
        l ~sub:true (indent + 2);
        out_str ";"
    | MIcreate_contract {tparameter = t, annot; tstorage; code} ->
        line "CREATE_CONTRACT";
        line " { parameter %s;" (string_of_tparameter ~html (t, annot));
        line "   storage   %s;" (string_of_mtype ~protect:() ~html tstorage);
        line "   code";
        code ~sub:true (indent + 5);
        out "};"
    | ( MI0
          ( Self None
          | Sender | Source | Amount | Balance | Level | Now | Self_address
          | Chain_id | Total_voting_power | Unit_ )
      | MI1
          ( Car | Cdr | Some_ | Eq | Abs | Neg | Int | IsNat | Neq | Le | Lt
          | Ge | Gt | Not | Concat1 | Size | Address | Implicit_account | Pack
          | Hash_key | Blake2b | Sha256 | Sha512 | Keccak | Sha3 | Set_delegate
          | Read_ticket | Join_tickets | Pairing_check | Voting_power )
      | MI1_fail (Failwith | Never)
      | MI2
          ( Add | Mul | Sub | Lsr | Lsl | Xor | Ediv | And | Or | Cons | Compare
          | Concat2 | Get | Mem | Exec | Apply | Sapling_verify_update | Ticket
          | Split_ticket )
      | MI3
          ( Slice | Update | Get_and_update | Transfer_tokens | Check_signature
          | Open_chest )
      | MIdrop | MIswap | MImich _ | MIfield _ | MIsetField _
      | MIconcat_unresolved ) as simple ->
        with_stack (fun () -> out_str (name_of_instr_exn simple)) );
    let full =
      match x with
      | MIerror _ -> Some ()
      | _ when full_types_and_tags -> Some ()
      | _ -> None
    in
    if show_stack && not sub
    then
      match stack_out with
      | Ok inst ->
          out
            " %s %s"
            (span "comment" "#")
            (span "stack" (sprintf "%s" (string_of_stack ?full inst)))
      | Error msg -> out " # Error: %s" (span "partialType" msg)
  in
  {f_tinstr; f_tliteral}

let _buffer_tliteral ~html ~show_stack b indent protect x =
  cata_tliteral (buffer_f ~html ~show_stack ~new_line:true b) x indent protect

let buffer_tinstr ~html ~show_stack ~sub ~new_line b indent x =
  cata_tinstr (buffer_f ~html ~show_stack ~new_line b) x ~sub indent

let seq_snoc xs x =
  match xs with
  | {tinstr = MIseq xs; stack_in} ->
      {tinstr = MIseq (xs @ [x]); stack_in; stack_out = x.stack_out}
  | xs ->
      {tinstr = MIseq [xs; x]; stack_in = xs.stack_in; stack_out = x.stack_out}

let wrap_in_seq = function
  | {tinstr = MIseq _} as i -> i
  | i -> {tinstr = MIseq [i]; stack_in = i.stack_in; stack_out = i.stack_out}

let insert_subsequences =
  let f_tinstr ~stack_in ~stack_out = function
    | MIseq _ as tinstr -> {tinstr; stack_in; stack_out}
    | tinstr -> {tinstr = map_instr_f wrap_in_seq id tinstr; stack_in; stack_out}
  in
  cata_tinstr {f_tinstr; f_tliteral = (fun ~t tliteral -> {tliteral; t})}

let display_tinstr ~show_stack ~new_line indent inst =
  let inst = insert_subsequences inst in
  Misc.with_buffer (fun b ->
      buffer_tinstr ~html:false ~show_stack ~sub:true ~new_line b indent inst)

let render_tinstr ~show_stack indent inst =
  let inst = insert_subsequences inst in
  Misc.with_buffer (fun b ->
      buffer_tinstr
        ~html:true
        ~show_stack
        ~sub:true
        ~new_line:true
        b
        indent
        inst)

let pretty_literal_f literal wrap ppf =
  let open Format in
  let wrap x = if wrap then Format.fprintf ppf "(%t)" x else x ppf in
  match (literal : _ literal_f) with
  | Int i -> fprintf ppf "%s" (Big_int.string_of_big_int i)
  | Unit -> fprintf ppf "Unit"
  | String s -> fprintf ppf "%S" s
  | Bool true -> fprintf ppf "True"
  | Bool false -> fprintf ppf "False"
  | Pair (l, r) -> wrap (fun ppf -> fprintf ppf "Pair %t %t" (l true) (r true))
  | None_ -> fprintf ppf "None"
  | Left x -> wrap (fun ppf -> fprintf ppf "Left %t" (x true))
  | Right x -> wrap (fun ppf -> fprintf ppf "Right %t" (x true))
  | Some_ x -> wrap (fun ppf -> fprintf ppf "Some %t" (x true))
  | Bytes s -> fprintf ppf "0x%s" (Misc.Hex.hexcape s)
  | Seq xs ->
      let f i x =
        let x = x false in
        let s = Format.asprintf "%t" x in
        if i = 0 || s = "" then fprintf ppf "%s" s else fprintf ppf "; %s" s
      in
      fprintf ppf "{";
      List.iteri f xs;
      fprintf ppf "}"
  | Elt (k, v) -> wrap (fun ppf -> fprintf ppf "Elt %t %t" (k true) (v true))
  | Instr i -> i true ppf
  | AnyMap xs ->
      let f i (k, v) =
        if i = 0
        then fprintf ppf "Elt %t %t" (k true) (v true)
        else fprintf ppf "; Elt %t %t" (k true) (v true)
      in
      fprintf ppf "{";
      List.iteri f xs;
      fprintf ppf "}"
  | Constant hash -> wrap (fun ppf -> fprintf ppf "constant %S" hash)

let pretty_instr_f i wrap ppf =
  let open Format in
  let name ppf = fprintf ppf "%s" (name_of_instr_exn i) in
  let wrap x = if wrap then Format.fprintf ppf "{ %t }" x else x ppf in
  match i with
  | MIseq [] -> fprintf ppf "{}"
  | MIseq (x :: xs) ->
      fprintf ppf "{ %t" (x false);
      List.iter (fun x -> fprintf ppf "; %t" (x false)) xs;
      fprintf ppf " }"
  | MIdipn (n, i) -> wrap (fun ppf -> fprintf ppf "DIP %i %t" n (i true))
  | MIdip i | MIloop i | MIloop_left i | MIiter i | MImap i ->
      fprintf ppf "%t %t" name (i true)
  | MIif_left (i1, i2) | MIif_none (i1, i2) | MIif_cons (i1, i2) | MIif (i1, i2)
    ->
      wrap (fun ppf -> fprintf ppf "%t %t %t" name (i1 true) (i2 true))
  | MIdup 1 -> wrap (fun ppf -> fprintf ppf "%t" name)
  | MIunpair [true; true] -> wrap (fun ppf -> fprintf ppf "%t" name)
  | MIunpair n -> wrap (fun ppf -> fprintf ppf "%t %s" name (unpair_arg n))
  | MIpairn n -> wrap (fun ppf -> fprintf ppf "%t %d" name n)
  | MIdig n
   |MIdug n
   |MIdropn n
   |MI1 (Getn n)
   |MI2 (Updaten n)
   |MIdup n
   |MI0 (Sapling_empty_state {memo = n}) ->
      wrap (fun ppf -> fprintf ppf "%t %d" name n)
  | MIcomment _xs -> wrap (fun _ppf -> ())
  | MIerror msg -> wrap (fun ppf -> fprintf ppf "ERROR %s" msg)
  | MI1 (Cast t)
   |MI0 (Nil t | Empty_set t | None_ t)
   |MI1 (Contract (None, t) | Unpack t | Left (_, _, t) | Right (_, _, t)) ->
      wrap (fun ppf ->
          fprintf ppf "%t %s" name (string_of_mtype ~protect:() ~html:false t))
  | MI1 (Rename a) ->
      let a = Option.cata "" (( ^ ) "@") a in
      wrap (fun ppf -> fprintf ppf "%t%s" name a)
  | MI0 (Empty_bigmap (k, v) | Empty_map (k, v)) ->
      wrap (fun ppf ->
          fprintf
            ppf
            "%t %s %s"
            name
            (string_of_mtype ~protect:() ~html:false k)
            (string_of_mtype ~protect:() ~html:false v))
  | MI1 (Contract (Some ep, t)) ->
      wrap (fun ppf ->
          fprintf
            ppf
            "CONTRACT %%%s %s"
            ep
            (string_of_mtype ~protect:() ~html:false t))
  | MI2 (View (name, t)) ->
      wrap (fun ppf ->
          fprintf
            ppf
            "VIEW %S %s"
            name
            (string_of_mtype ~protect:() ~html:false t))
  | MIlambda (t1, t2, c) ->
      wrap (fun ppf ->
          let t1 = string_of_mtype ~protect:() ~html:false t1 in
          let t2 = string_of_mtype ~protect:() ~html:false t2 in
          fprintf ppf "LAMBDA %s %s %t" t1 t2 (c true))
  | MIpush (t, l) ->
      wrap (fun ppf ->
          let t = string_of_mtype ~protect:() ~html:false t in
          let l = l true in
          fprintf ppf "PUSH %s %t" t l)
  | MIcreate_contract {tparameter = tparameter, annot; tstorage; code} ->
      wrap (fun ppf ->
          fprintf
            ppf
            "CREATE_CONTRACT { parameter %s; storage %s; code %t}"
            (string_of_tparameter ~html:false (tparameter, annot))
            (string_of_mtype ~protect:() ~html:false tstorage)
            (code true))
  | MI0
      ( Sender | Source | Amount | Balance | Level | Now | Self_address
      | Chain_id | Total_voting_power | Unit_ | Self _ )
   |MI1
      ( Car | Cdr | Some_ | Eq | Abs | Neg | Int | IsNat | Neq | Le | Lt | Ge
      | Gt | Not | Concat1 | Size | Address | Implicit_account | Pack | Hash_key
      | Blake2b | Sha256 | Sha512 | Keccak | Sha3 | Set_delegate | Read_ticket
      | Join_tickets | Pairing_check | Voting_power )
   |MI1_fail (Failwith | Never)
   |MI2
      ( Pair _ | Add | Mul | Sub | Lsr | Lsl | Xor | Ediv | And | Or | Cons
      | Compare | Concat2 | Get | Mem | Exec | Apply | Sapling_verify_update
      | Ticket | Split_ticket )
   |MI3
      ( Slice | Update | Get_and_update | Transfer_tokens | Check_signature
      | Open_chest )
   |MIdrop | MIswap | MImich _ | MIfield _ | MIsetField _ | MIconcat_unresolved
    ->
      wrap (fun ppf -> fprintf ppf "%t" name)

let size_instr, _size_literal =
  let f_instr = fold_instr_f ( + ) ( + ) 1 in
  let f_literal = fold_literal_f ( + ) ( + ) 1 in
  cata {f_instr; f_literal}

let pretty_alg = {f_instr = pretty_instr_f; f_literal = pretty_literal_f}

let _pretty_instr = cata_instr pretty_alg

let pretty_literal = cata_literal pretty_alg

let string_of_literal m = Format.asprintf "%t" (pretty_literal m true)

module Of_micheline = struct
  open Micheline

  let rec mtype x = fst (mtype_annotated x)

  and mtype_annotated = function
    | Basics.Primitive {name; annotations; arguments} as p ->
        let mt =
          match (name, arguments) with
          | "pair", [t1; t2] ->
              let fst, annot_fst = mtype_annotated t1 in
              let snd, annot_snd = mtype_annotated t2 in
              mt_pair ?annot_fst ?annot_snd fst snd
          | "pair", l ->
            begin
              match List.rev_map mtype_annotated l with
              | [] -> assert false
              | (last, annot) :: rest ->
                  fst
                    (List.fold_left
                       (fun (snd, annot_snd) (fst, annot_fst) ->
                         (mt_pair ?annot_fst ?annot_snd fst snd, None))
                       (last, annot)
                       rest)
            end
          | "or", [t1; t2] ->
              let left, annot_left = mtype_annotated t1 in
              let right, annot_right = mtype_annotated t2 in
              mt_or ?annot_left ?annot_right left right
          | "unit", [] -> mt_unit
          | "bool", [] -> mt_bool
          | "mutez", [] -> mt_mutez
          | "timestamp", [] -> mt_timestamp
          | "nat", [] -> mt_nat
          | "int", [] -> mt_int
          | "string", [] -> mt_string
          | "key", [] -> mt_key
          | "signature", [] -> mt_signature
          | "bytes", [] -> mt_bytes
          | "chain_id", [] -> mt_chain_id
          | "key_hash", [] -> mt_key_hash
          | "contract", [t] -> mt_contract (mtype t)
          | "address", [] -> mt_address
          | "list", [t] -> mt_list (mtype t)
          | "option", [t] -> mt_option (mtype t)
          | "set", [t] -> mt_set (mtype t)
          | "map", [t1; t2] -> mt_map (mtype t1) (mtype t2)
          | "big_map", [t1; t2] -> mt_big_map (mtype t1) (mtype t2)
          | "lambda", [t1; t2] -> mt_lambda (mtype t1) (mtype t2)
          | "operation", [] -> mt_operation
          | "sapling_state", [Int memo] -> mt_sapling_state (int_of_string memo)
          | "sapling_transaction", [Int memo] ->
              mt_sapling_transaction (int_of_string memo)
          | "never", [] -> mt_never
          | "ticket", [t] -> mt_ticket (mtype t)
          | "bls12_381_g1", [] -> mt_bls12_381_g1
          | "bls12_381_g2", [] -> mt_bls12_381_g2
          | "bls12_381_fr", [] -> mt_bls12_381_fr
          | "chest_key", [] -> mt_chest_key
          | "chest", [] -> mt_chest
          | _ -> mk_mtype (MT_var (sprintf "Parse type error %S" (pretty "" p)))
        in
        List.fold_left
          (fun (mt, fa) a ->
            let update = function
              | Some _ -> failwith "duplicate annotation"
              | None -> Some (String.sub a 1 (String.length a - 1))
            in
            match a.[0] with
            | ':' -> ({mt with annot_type = update mt.annot_type}, fa)
            | '@' -> ({mt with annot_variable = update mt.annot_variable}, fa)
            | '%' -> (mt, update fa)
            | _ -> failwith "cannot parse annotation")
          (mt, None)
          annotations
    | p -> failwith ("Parse type error " ^ pretty "" p)

  let rec literal x : literal =
    match (x : Basics.micheline) with
    | Int i -> MLiteral.int (Big_int.big_int_of_string i)
    | Bytes s -> MLiteral.bytes s
    | String s -> MLiteral.string s
    | Primitive {name; annotations = _; arguments} ->
      ( match (name, arguments) with
      | "Unit", [] -> MLiteral.unit
      | "False", [] -> MLiteral.bool false
      | "True", [] -> MLiteral.bool true
      | "Pair", l ->
        begin
          match List.rev l with
          | [] -> assert false
          | a :: l ->
              List.fold_left
                (fun x y -> MLiteral.pair (literal y) x)
                (literal a)
                l
        end
      | "None", [] -> MLiteral.none
      | "Some", [x] -> MLiteral.some (literal x)
      | "Left", [x] -> MLiteral.left (literal x)
      | "Right", [x] -> MLiteral.right (literal x)
      | "Elt", [k; v] -> MLiteral.elt (literal k) (literal v)
      | _ -> MLiteral.instr (instruction x) )
    | Sequence xs -> MLiteral.seq (List.map literal xs)

  and instruction x =
    let err () =
      MIerror (sprintf "Cannot parse instruction %S" (Basics.show_micheline x))
    in
    let cmp instr = MIseq [{instr = MI2 Compare}; {instr}] in
    let fail =
      MIseq
        [{instr = MIpush (mt_unit, MLiteral.unit)}; {instr = MI1_fail Failwith}]
    in
    let if_op instr x y =
      MIseq [{instr}; {instr = MIif (instruction x, instruction y)}]
    in
    let ifcmp_op instr x y =
      MIseq
        [ {instr = MI2 Compare}
        ; {instr}
        ; {instr = MIif (instruction x, instruction y)} ]
    in
    let assert_op instr =
      MIseq [{instr}; {instr = MIif ({instr = MIseq []}, {instr = fail})}]
    in
    let assert_cmp_op instr =
      MIseq
        [ {instr = MI2 Compare}
        ; {instr}
        ; {instr = MIif ({instr = MIseq []}, {instr = fail})} ]
    in
    let parse_simple_macro = function
      | "FAIL" -> fail
      | "ASSERT" -> MIif ({instr = MIseq []}, {instr = fail})
      | "CMPEQ" -> cmp (MI1 Eq)
      | "CMPNEQ" -> cmp (MI1 Eq)
      | "CMPLT" -> cmp (MI1 Lt)
      | "CMPGT" -> cmp (MI1 Gt)
      | "CMPLE" -> cmp (MI1 Le)
      | "CMPGE" -> cmp (MI1 Ge)
      | "ASSERTEQ" -> assert_op (MI1 Eq)
      | "ASSERTNEQ" -> assert_op (MI1 Eq)
      | "ASSERTLT" -> assert_op (MI1 Lt)
      | "ASSERTGT" -> assert_op (MI1 Gt)
      | "ASSERTLE" -> assert_op (MI1 Le)
      | "ASSERTGE" -> assert_op (MI1 Ge)
      | "ASSERT_CMPEQ" -> assert_cmp_op (MI1 Eq)
      | "ASSERT_CMPNEQ" -> assert_cmp_op (MI1 Eq)
      | "ASSERT_CMPLT" -> assert_cmp_op (MI1 Lt)
      | "ASSERT_CMPGT" -> assert_cmp_op (MI1 Gt)
      | "ASSERT_CMPLE" -> assert_cmp_op (MI1 Le)
      | "ASSERT_CMPGE" -> assert_cmp_op (MI1 Ge)
      | "ASSERT_SOME" -> MIif_none ({instr = fail}, {instr = MIseq []})
      | "ASSERT_NONE" -> MIif_none ({instr = MIseq []}, {instr = fail})
      | prim
        when String.index_opt prim 'D' = Some 0
             && String.index_opt prim 'P' = Some (String.length prim - 1)
             && 2 < String.length prim
             && Base.String.count prim ~f:(fun c -> c = 'U')
                = String.length prim - 2 ->
          let n = String.length prim - 3 in
          MIseq [{instr = MIdig n}; {instr = MIdup 1}; {instr = MIdug (n + 1)}]
      | prim
        when String.index_opt prim 'C' = Some 0
             && String.index_opt prim 'R' = Some (String.length prim - 1) ->
          let l =
            Base.String.fold
              (String.sub prim 1 (String.length prim - 2))
              ~init:(Some [])
              ~f:(fun acc c ->
                match (acc, c) with
                | Some acc, 'A' -> Some (A :: acc)
                | Some acc, 'D' -> Some (D :: acc)
                | _ -> None)
          in
          ( match l with
          | Some l -> MIfield (List.rev l)
          | None -> err () )
      | _ -> err ()
    in
    let instr =
      match x with
      | Sequence [x] -> (instruction x).instr
      | Sequence xs -> MIseq (List.map instruction xs)
      | Primitive {name; annotations; arguments} ->
        ( match (name, arguments) with
        | "RENAME", [] ->
            let a =
              match annotations with
              | [] -> None
              | [a] -> Base.String.chop_prefix ~prefix:"@" a
              | _ -> assert false
            in
            MI1 (Rename a)
        | "UNIT", [] -> MI0 Unit_
        | "EMPTY_MAP", [k; v] ->
            MIpush (mt_map (mtype k) (mtype v), MLiteral.mk_map [])
        | "EMPTY_SET", [t] -> MIpush (mt_set (mtype t), MLiteral.set [])
        | "EMPTY_BIG_MAP", [k; v] -> MI0 (Empty_bigmap (mtype k, mtype v))
        | "DIP", [x] -> MIdip (instruction x)
        | "DIP", [Int i; x] -> MIdipn (int_of_string i, instruction x)
        | prim, [x]
          when String.index_opt prim 'D' = Some 0
               && String.index_opt prim 'P' = Some (String.length prim - 1)
               && 2 < String.length prim
               && Base.String.count prim ~f:(fun c -> c = 'I')
                  = String.length prim - 2 ->
            MIdipn (String.length prim - 2, instruction x)
        | "LOOP", [x] -> MIloop (instruction x)
        | "LOOP_LEFT", [x] -> MIloop_left (instruction x)
        | "ITER", [x] -> MIiter (instruction x)
        | "MAP", [x] -> MImap (instruction x)
        | "DROP", [] -> MIdrop
        | "DROP", [Int n] -> MIdropn (int_of_string n)
        | "DUP", [] -> MIdup 1
        | "DUP", [Int n] -> MIdup (int_of_string n)
        | "DIG", [Int i] -> MIdig (int_of_string i)
        | "DUG", [Int i] -> MIdug (int_of_string i)
        | "FAILWITH", [] -> MI1_fail Failwith
        | "IF", [x; y] -> MIif (instruction x, instruction y)
        | "IF_LEFT", [x; y] -> MIif_left (instruction x, instruction y)
        | "IF_RIGHT", [x; y] -> MIif_left (instruction y, instruction x)
        | "IF_SOME", [x; y] -> MIif_none (instruction y, instruction x)
        | "IF_NONE", [x; y] -> MIif_none (instruction x, instruction y)
        | "IF_CONS", [x; y] -> MIif_cons (instruction x, instruction y)
        | "NIL", [t] -> MI0 (Nil (mtype t))
        | "CONS", [] -> MI2 Cons
        | "NONE", [t] -> MI0 (None_ (mtype t))
        | "SOME", [] -> MI1 Some_
        | "PAIR", [] -> MI2 (Pair (None, None))
        | "PAIR", [Int n] -> MIpairn (int_of_string n)
        | "LEFT", [t] -> MI1 (Left (None, None, mtype t))
        | "RIGHT", [t] -> MI1 (Right (None, None, mtype t))
        | "PUSH", [t; l] -> MIpush (mtype t, literal l)
        | "SWAP", [] -> MIswap
        | "UNPAIR", [] -> MIunpair [true; true]
        | "UNPAIR", [Int n] -> MIunpair (List.replicate (int_of_string n) true)
        | "CAR", [] -> MIfield [A]
        | "CDR", [] -> MIfield [D]
        | "CONTRACT", [t] ->
            let entry_point =
              match annotations with
              | [] -> None
              | entry_point :: _ ->
                  Base.String.chop_prefix ~prefix:"%" entry_point
            in
            MI1 (Contract (entry_point, mtype t))
        | "VIEW", [String name; t] -> MI2 (View (name, mtype t))
        | "CAST", [t] ->
            let t = mtype t in
            MI1 (Cast t)
        | "EXEC", [] -> MI2 Exec
        | "APPLY", [] -> MI2 Apply
        | "LAMBDA", [t; u; x] -> MIlambda (mtype t, mtype u, instruction x)
        | "CREATE_CONTRACT", [x] ->
            let tparameter, tstorage, code =
              if false then failwith (Basics.show_micheline x);
              match x with
              | Sequence
                  [ Primitive {name = "parameter"; arguments = [tparameter]}
                  ; Primitive {name = "storage"; arguments = [tstorage]}
                  ; Primitive {name = "code"; arguments = [code]} ] ->
                  ( ( mtype tparameter
                    , None (* TODO single entry point annotation *) )
                  , mtype tstorage
                  , instruction code )
              | _ -> assert false
            in
            MIcreate_contract {tparameter; tstorage; code}
        | "SELF", [] ->
            let entry_point =
              match annotations with
              | [] -> None
              | entry_point :: _ ->
                  Base.String.chop_prefix ~prefix:"%" entry_point
            in
            MI0 (Self entry_point)
        | "ADDRESS", [] -> MI1 Address
        | "SELF_ADDRESS", [] -> MI0 Self_address
        | "IMPLICIT_ACCOUNT", [] -> MI1 Implicit_account
        | "TRANSFER_TOKENS", [] -> MI3 Transfer_tokens
        | "CHECK_SIGNATURE", [] -> MI3 Check_signature
        | "SET_DELEGATE", [] -> MI1 Set_delegate
        | "SAPLING_EMPTY_STATE", [Int memo] ->
            MI0 (Sapling_empty_state {memo = int_of_string memo})
        | "SAPLING_VERIFY_UPDATE", [] -> MI2 Sapling_verify_update
        | "NEVER", [] -> MI1_fail Never
        | "READ_TICKET", [] -> MI1 Read_ticket
        | "TICKET", [] -> MI2 Ticket
        | "SPLIT_TICKET", [] -> MI2 Split_ticket
        | "JOIN_TICKETS", [] -> MI1 Join_tickets
        | "PAIRING_CHECK", [] -> MI1 Pairing_check
        | "TOTAL_VOTING_POWER", [] -> MI0 Total_voting_power
        | "VOTING_POWER", [] -> MI1 Voting_power
        | "EQ", [] -> MI1 Eq
        | "NEQ", [] -> MI1 Neq
        | "LE", [] -> MI1 Le
        | "LT", [] -> MI1 Lt
        | "GE", [] -> MI1 Ge
        | "GT", [] -> MI1 Gt
        | "COMPARE", [] -> MI2 Compare
        | "MUL", [] -> MI2 Mul
        | "ADD", [] -> MI2 Add
        | "SUB", [] -> MI2 Sub
        | "EDIV", [] -> MI2 Ediv
        | "NOT", [] -> MI1 Not
        | "AND", [] -> MI2 And
        | "OR", [] -> MI2 Or
        | "LSL", [] -> MI2 Lsl
        | "LSR", [] -> MI2 Lsr
        | "XOR", [] -> MI2 Xor
        | "CONCAT", [] -> MIconcat_unresolved
        | "SLICE", [] -> MI3 Slice
        | "SIZE", [] -> MI1 Size
        | "GET", [] -> MI2 Get
        | "GET", [Int x] -> MI1 (Getn (int_of_string x))
        | "UPDATE", [] -> MI3 Update
        | "UPDATE", [Int x] -> MI2 (Updaten (int_of_string x))
        | "GET_AND_UPDATE", [] -> MI3 Get_and_update
        | "OPEN_CHEST", [] -> MI3 Open_chest
        | "SENDER", [] -> MI0 Sender
        | "SOURCE", [] -> MI0 Source
        | "AMOUNT", [] -> MI0 Amount
        | "BALANCE", [] -> MI0 Balance
        | "NOW", [] -> MI0 Now
        | "LEVEL", [] -> MI0 Level
        | "CHAIN_ID", [] -> MI0 Chain_id
        | "MEM", [] -> MI2 Mem
        | "HASH_KEY", [] -> MI1 Hash_key
        | "BLAKE2B", [] -> MI1 Blake2b
        | "SHA256", [] -> MI1 Sha256
        | "SHA512", [] -> MI1 Sha512
        | "KECCAK", [] -> MI1 Keccak
        | "SHA3", [] -> MI1 Sha3
        | "ABS", [] -> MI1 Abs
        | "NEG", [] -> MI1 Neg
        | "INT", [] -> MI1 Int
        | "ISNAT", [] -> MI1 IsNat
        | "PACK", [] -> MI1 Pack
        | "UNPACK", [t] -> MI1 (Unpack (mtype t))
        | prim, [] -> parse_simple_macro prim
        | "IFEQ", [x; y] -> if_op (MI1 Eq) x y
        | "IFNEQ", [x; y] -> if_op (MI1 Eq) x y
        | "IFLT", [x; y] -> if_op (MI1 Lt) x y
        | "IFGT", [x; y] -> if_op (MI1 Gt) x y
        | "IFLE", [x; y] -> if_op (MI1 Le) x y
        | "IFGE", [x; y] -> if_op (MI1 Ge) x y
        | "IFCMPEQ", [x; y] -> ifcmp_op (MI1 Eq) x y
        | "IFCMPNEQ", [x; y] -> ifcmp_op (MI1 Eq) x y
        | "IFCMPLT", [x; y] -> ifcmp_op (MI1 Lt) x y
        | "IFCMPGT", [x; y] -> ifcmp_op (MI1 Gt) x y
        | "IFCMPLE", [x; y] -> ifcmp_op (MI1 Le) x y
        | "IFCMPGE", [x; y] -> ifcmp_op (MI1 Ge) x y
        (* TODO Macros: ASSERT_SOME, ASSERT_LEFT, ASSERT_RIGHT *)
        | _ -> err () )
      | _ -> err ()
    in
    {instr}
end

module To_micheline = struct
  open Micheline

  let mtype ?annot_field =
    cata_mtype ~annot_field (fun ?annot_type ?annot_variable t ~annot_field ->
        let annotations =
          let get pref = Option.map (( ^ ) pref) in
          Base.List.filter_opt
            [get "%" annot_field; get ":" annot_type; get "@" annot_variable]
        in
        let prim = primitive ~annotations in
        match t with
        | MT0 t ->
            let t, memo = string_of_type0 t in
            Option.cata (prim t []) (fun x -> prim t [Int x]) memo
        | MT1 (t, t1) -> prim (string_of_type1 t) [t1 ~annot_field:None]
        | MT2 (t, t1, t2) ->
            let t, a1, a2 = string_of_type2 t in
            prim t [t1 ~annot_field:a1; t2 ~annot_field:a2]
        | MT_var msg ->
            primitive
              "ERROR"
              [Format.kasprintf string "Cannot compile missing type: %s" msg])

  let dip_seq i = primitive "DIP" [sequence i]

  let rec c_ad_r s =
    (*
         See http://tezos.gitlab.io/mainnet/whitedoc/michelson.html#syntactic-conveniences
         > CA(\rest=[AD]+)R / S  =>  CAR ; C(\rest)R / S
         > CD(\rest=[AD]+)R / S  =>  CDR ; C(\rest)R / S
      *)
    match s.[0] with
    | 'A' -> primitive "CAR" [] :: c_ad_r (Base.String.drop_prefix s 1)
    | 'D' -> primitive "CDR" [] :: c_ad_r (Base.String.drop_prefix s 1)
    | exception _ -> []
    | other ->
        Format.kasprintf
          failwith
          "c_ad_r macro: wrong char: '%c' (of %S)"
          other
          s

  let rec set_c_ad_r s =
    (*
         See http://tezos.gitlab.io/mainnet/whitedoc/michelson.html#syntactic-conveniences
         > SET_CA(\rest=[AD]+)R / S   =>
             { DUP ; DIP { CAR ; SET_C(\rest)R } ; CDR ; SWAP ; PAIR } / S
         > SET_CD(\rest=[AD]+)R / S   =>
             { DUP ; DIP { CDR ; SET_C(\rest)R } ; CAR ; PAIR } / S
         Then,
         > SET_CAR  =>  CDR ; SWAP ; PAIR
         > SET_CDR  =>  CAR ; PAIR
      *)
    match s.[0] with
    | 'A' when String.length s > 1 ->
        [ primitive "DUP" []
        ; dip_seq
            (primitive "CAR" [] :: set_c_ad_r (Base.String.drop_prefix s 1))
        ; primitive "CDR" []
        ; primitive "SWAP" []
        ; primitive "PAIR" [] ]
    | 'A' -> [primitive "CDR" []; primitive "SWAP" []; primitive "PAIR" []]
    | 'D' when String.length s > 1 ->
        [ primitive "DUP" []
        ; dip_seq
            (primitive "CDR" [] :: set_c_ad_r (Base.String.drop_prefix s 1))
        ; primitive "CAR" []
        ; primitive "PAIR" [] ]
    | 'D' -> [primitive "CAR" []; primitive "PAIR" []]
    | exception _ ->
        Format.kasprintf failwith "set_c_r_macro: called with no chars: S" s
    | other ->
        Format.kasprintf
          failwith
          "set_c_r_macro: wrong char: '%c' (of %S)"
          other
          s

  let rec literal {literal = l} =
    match l with
    | Int bi -> int (Big_int.string_of_big_int bi)
    | Bool false -> primitive "False" []
    | Bool true -> primitive "True" []
    | String s -> string s
    | Unit -> primitive "Unit" []
    | Bytes b -> bytes b
    | Pair (left, right) -> primitive "Pair" [literal left; literal right]
    | None_ -> primitive "None" []
    | Some_ l -> primitive "Some" [literal l]
    | Left e -> primitive "Left" [literal e]
    | Right e -> primitive "Right" [literal e]
    | Seq xs -> xs |> Base.List.map ~f:literal |> sequence
    | Elt (k, v) -> primitive "Elt" [literal k; literal v]
    | Instr x -> sequence (instruction x)
    | AnyMap xs ->
        let xs = List.map (fun (k, v) -> {literal = Elt (k, v)}) xs in
        literal {literal = Seq xs}
    | Constant hash -> primitive "constant" [string hash]

  and instruction (the_instruction : instr) =
    let prim0 ?annotations n = [primitive ?annotations n []] in
    let primn ?annotations n l = [primitive ?annotations n l] in
    let rec_instruction instr = Micheline.sequence (instruction instr) in
    match the_instruction.instr with
    | MIerror s -> primn "ERROR" [string s]
    | MIcomment _comment ->
        []
        (*
          [ primitive "PUSH" [primitive "string" []; string comment]
          ; primitive "DROP" [] ] *)
    | MIdip instr -> primn "DIP" [rec_instruction instr]
    | MIdipn (n, instr) ->
        primn "DIP" [int (Base.Int.to_string n); rec_instruction instr]
    | MIdig n -> primn "DIG" [int (Base.Int.to_string n)]
    | MIdug n -> primn "DUG" [int (Base.Int.to_string n)]
    | MIdup 1 -> primn "DUP" []
    | MIdup n -> primn "DUP" [int (Base.Int.to_string n)]
    | MIunpair [true; true] -> primn "UNPAIR" []
    | MIunpair n -> primn "UNPAIR" [int (unpair_arg n)]
    | MIpairn n -> primn "PAIR" [int (Base.Int.to_string n)]
    | MI1 (Getn n) -> primn "GET" [int (Base.Int.to_string n)]
    | MI2 (Updaten n) -> primn "UPDATE" [int (Base.Int.to_string n)]
    | MIdropn n -> primn "DROP" [int (Base.Int.to_string n)]
    | MIloop instr -> primn "LOOP" [rec_instruction instr]
    | MIloop_left instr -> primn "LOOP_LEFT" [rec_instruction instr]
    | MIiter instr -> primn "ITER" [rec_instruction instr]
    | MImap instr -> primn "MAP" [rec_instruction instr]
    | MIseq ils -> Base.List.concat_map ils ~f:instruction
    | MIif (t, e) -> primn "IF" [rec_instruction t; rec_instruction e]
    | MIif_left (t, e) -> primn "IF_LEFT" [rec_instruction t; rec_instruction e]
    | MIif_none (t, e) -> primn "IF_NONE" [rec_instruction t; rec_instruction e]
    | MIif_cons (t, e) -> primn "IF_CONS" [rec_instruction t; rec_instruction e]
    | MIpush (mt, lit) -> primn "PUSH" [mtype mt; literal lit]
    | MI0 (Self (Some entry_point)) ->
        primn ~annotations:["%" ^ entry_point] "SELF" []
    | MI2 (Pair (a1, a2)) ->
        primn ~annotations:(two_field_annots (a1, a2)) "PAIR" []
    | MI1 (Right (a1, a2, mty)) ->
        primn ~annotations:(two_field_annots (a1, a2)) "RIGHT" [mtype mty]
    | MI1 (Left (a1, a2, mty)) ->
        primn ~annotations:(two_field_annots (a1, a2)) "LEFT" [mtype mty]
    | MI0 (None_ mty) -> primn "NONE" [mtype mty]
    | MI0 (Nil mty) -> primn "NIL" [mtype mty]
    | MI0 (Empty_set mty) -> primn "EMPTY_SET" [mtype mty]
    | MI0 (Empty_map (k, v)) -> primn "EMPTY_MAP" [mtype k; mtype v]
    | MI0 (Empty_bigmap (k, v)) -> primn "EMPTY_BIG_MAP" [mtype k; mtype v]
    | MI1 (Contract (None, mty)) -> primn "CONTRACT" [mtype mty]
    | MI1 (Contract (Some entry_point, mty)) ->
        primn ~annotations:["%" ^ entry_point] "CONTRACT" [mtype mty]
    | MI2 (View (name, mty)) -> primn "VIEW" [string name; mtype mty]
    | MI1 (Cast t) -> primn "CAST" [mtype t]
    | MI1 (Rename None) -> primn "RENAME" []
    | MI1 (Rename (Some a)) -> primn "RENAME" ~annotations:["@" ^ a] []
    | MIlambda (t1, t2, b) ->
        primn "LAMBDA" [mtype t1; mtype t2; rec_instruction b]
    | MI1 (Unpack mty) -> primn "UNPACK" [mtype mty]
    | MIfield op -> c_ad_r (string_of_ad_path op)
    | MIsetField op -> set_c_ad_r (string_of_ad_path op)
    | MIcreate_contract {tparameter = tparameter, annot_field; tstorage; code}
      ->
        primn
          "CREATE_CONTRACT"
          [ sequence
              ( primn "parameter" [mtype ?annot_field tparameter]
              @ primn "storage" [mtype tstorage]
              @ primn "code" [rec_instruction code] ) ]
    | MI0 (Sapling_empty_state {memo}) ->
        primn "SAPLING_EMPTY_STATE" [int (string_of_int memo)]
    | ( MI0
          ( Sender | Source | Amount | Balance | Level | Now | Self _
          | Self_address | Chain_id | Total_voting_power | Unit_ )
      | MI1
          ( Car | Cdr | Some_ | Eq | Abs | Neg | Int | IsNat | Neq | Le | Lt
          | Ge | Gt | Not | Concat1 | Size | Address | Implicit_account | Pack
          | Hash_key | Blake2b | Sha256 | Sha512 | Keccak | Sha3 | Set_delegate
          | Read_ticket | Join_tickets | Pairing_check | Voting_power )
      | MI1_fail (Failwith | Never)
      | MI2
          ( Add | Mul | Sub | Lsr | Lsl | Xor | Ediv | And | Or | Cons | Compare
          | Concat2 | Get | Mem | Exec | Apply | Sapling_verify_update | Ticket
          | Split_ticket )
      | MI3
          ( Slice | Update | Get_and_update | Transfer_tokens | Check_signature
          | Open_chest )
      | MIdrop | MIswap | MImich _ | MIconcat_unresolved ) as simple ->
      ( try prim0 (name_of_instr_exn simple) with
      | _ -> [sequence [primitive "ERROR-NOT-SIMPLE" []]] )
end

let count_bigmaps =
  let f_tinstr ~stack_in:_ ~stack_out:_ _instr = 0 in
  let f_tliteral ~t literal =
    let i =
      match t with
      | Ok {mt = MT2 (T_big_map, _, _)} -> 1
      | _ -> 0
    in
    fold_literal_f ( + ) ( + ) i literal
  in
  cata_tliteral {f_tinstr; f_tliteral}

type 'instr view =
  { name : string
  ; pure : bool
  ; doc : string
  ; tparameter : mtype option
  ; treturn : mtype
  ; onchain_code : 'instr option
  ; offchain_code : 'instr }
[@@deriving show {with_path = false}, map]

type contract =
  { tparameter : mtype * string option
  ; tstorage : mtype
  ; code : instr
  ; lazy_entry_points : literal option
  ; storage : literal option
  ; views : instr view list }
[@@deriving show {with_path = false}]

type tcontract =
  { tparameter : mtype * string option
  ; tstorage : mtype
  ; code : tinstr
  ; lazy_entry_points : tliteral option
  ; storage : tliteral option
  ; views : tinstr view list }
[@@deriving show {with_path = false}]

let unexpected_final_stack_error = "Unexpected final stack"

let erase_types_contract
    {tparameter; tstorage; code; lazy_entry_points; storage; views} =
  let lazy_entry_points = Option.map erase_types_literal lazy_entry_points in
  let code = erase_types_instr code in
  let storage = Option.map erase_types_literal storage in
  let views = List.map (map_view erase_types_instr) views in
  ({tparameter; tstorage; code; lazy_entry_points; storage; views} : contract)

let string_of_tliteral x = string_of_literal (erase_types_literal x)

let typecheck_contract
    ~strict_dup
    ({tparameter; tstorage; code; lazy_entry_points; storage; views} : contract)
    =
  let storage =
    Option.map (typecheck_literal ~strict_dup ~tparameter tstorage) storage
  in
  let code =
    typecheck_instr
      ~strict_dup
      ~tparameter
      (initial_stack ~tparameter:(fst tparameter) ~tstorage)
      code
  in
  let typecheck_view ({tparameter; onchain_code; offchain_code} as v) =
    let check offchain code =
      let tparameter, stack =
        match tparameter with
        | None ->
            let stack =
              if offchain
              then Stack_ok [{tstorage with annot_variable = Some "storage"}]
              else initial_stack ~tparameter:mt_unit ~tstorage
            in
            (mt_unit, stack)
        | Some tparameter -> (tparameter, initial_stack ~tparameter ~tstorage)
      in
      typecheck_instr ~strict_dup:true ~tparameter:(tparameter, None) stack code
    in
    { v with
      onchain_code = Option.map (check false) onchain_code
    ; offchain_code = (check true) offchain_code }
  in
  let views = List.map typecheck_view views in
  let code =
    match code.stack_out with
    | Ok (Stack_ok [{mt = MT2 (T_pair _, fst, snd)}])
      when unifiable_types fst (mt_list mt_operation)
           && unifiable_types snd tstorage ->
        code
    | Ok Stack_failed -> code
    | Ok _ ->
        let msg = unexpected_final_stack_error in
        let err =
          { tinstr = MIerror msg
          ; stack_in = code.stack_in
          ; stack_out = code.stack_out }
        in
        seq_snoc code err
    | Error _ -> code
  in
  let lazy_entry_points =
    match (tstorage.mt, lazy_entry_points) with
    | MT2 (T_pair _, _, snd), Some x ->
        Some (typecheck_literal ~strict_dup ~tparameter snd x)
    | _, Some _ -> failwith "lazy entry points, but storage not a pair"
    | _, None -> None
  in
  {tparameter; tstorage; code; lazy_entry_points; storage; views}

let has_error_tcontract ~accept_missings {tparameter; tstorage; code; views} =
  let errors =
    let has_error_view {onchain_code; offchain_code} =
      Option.cata [] (has_error ~accept_missings) onchain_code
      @ has_error ~accept_missings offchain_code
    in
    let e =
      has_error ~accept_missings code @ List.concat_map has_error_view views
    in
    if accept_missings
    then e
    else e @ has_missing_type tstorage @ has_missing_type (fst tparameter)
  in
  let rec clean acc = function
    | e1 :: e2 :: rest when e1 = e2 -> clean acc (e1 :: rest)
    | e :: rest -> clean (e :: acc) rest
    | [] -> List.rev acc
  in
  clean [] errors

let to_micheline_tcontract
    {tstorage; tparameter = tparameter, annot_field; code; views} =
  let open Micheline in
  let erase_types_from_instr code =
    match To_micheline.instruction (erase_types_instr code) with
    | [Sequence _] as l -> l
    | l -> [sequence l]
  in
  sequence
    ( [ primitive "storage" [To_micheline.mtype tstorage]
      ; primitive "parameter" [To_micheline.mtype ?annot_field tparameter]
      ; primitive "code" (erase_types_from_instr code) ]
    @ List.fold_left
        (fun acc {name; tparameter; treturn; onchain_code} ->
          acc
          @
          match onchain_code with
          | None -> []
          | Some code ->
              [ primitive
                  "view"
                  [ To_micheline.literal {literal = String name}
                  ; To_micheline.mtype (Option.default mt_unit tparameter)
                  ; To_micheline.mtype treturn
                  ; sequence (To_micheline.instruction (erase_types_instr code))
                  ] ])
        []
        views )

let display_tcontract {tstorage; tparameter; code; views} =
  sprintf
    "parameter %s;\nstorage   %s;\ncode\n%s;%s"
    (string_of_tparameter ~html:false tparameter)
    (string_of_mtype ~protect:() ~html:false tstorage)
    (display_tinstr ~show_stack:true ~new_line:true 2 code)
    (List.fold_left
       (fun acc {name; tparameter; treturn; onchain_code} ->
         match onchain_code with
         | None -> acc
         | Some code ->
             sprintf
               "%s\nview\n  %S %s %s\n%s;"
               acc
               name
               (string_of_mtype
                  ~protect:()
                  ~html:false
                  (Option.default mt_unit tparameter))
               (string_of_mtype ~protect:() ~html:false treturn)
               (display_tinstr ~show_stack:true ~new_line:true 2 code))
       ""
       views)

let render_tcontract {tstorage; tparameter; code; views} =
  sprintf
    "parameter %s;<br>storage &nbsp;&nbsp;%s;<br><div \
     class='michelsonLine'>code<br>%s;</div>%s"
    (string_of_tparameter ~html:true tparameter)
    (string_of_mtype ~protect:() ~html:true tstorage)
    (render_tinstr ~show_stack:true 2 code)
    (List.fold_left
       (fun acc {name; tparameter; treturn; onchain_code} ->
         match onchain_code with
         | None -> acc
         | Some code ->
             sprintf
               "%s<br/><div class='michelsonLine'>view<br/>  \"<span \
                class=\"type\">%s</span>\" %s %s<br/>%s;</div>"
               acc
               name
               (string_of_mtype
                  ~protect:()
                  ~html:true
                  (Option.default mt_unit tparameter))
               (string_of_mtype ~protect:() ~html:true treturn)
               (render_tinstr ~show_stack:true 2 code))
       ""
       views)

let render_tcontract_no_types {tstorage; tparameter; code; views} =
  sprintf
    "parameter %s;<br>storage &nbsp;&nbsp;%s;<br><div \
     class='michelsonLine'>code<br>%s;</div>%s"
    (string_of_tparameter ~html:true tparameter)
    (string_of_mtype ~protect:() ~html:true tstorage)
    (render_tinstr ~show_stack:false 2 code)
    (List.fold_left
       (fun acc {name; tparameter; treturn; onchain_code} ->
         match onchain_code with
         | None -> acc
         | Some code ->
             sprintf
               "%s<br/><div class='michelsonLine'>view<br/>  \"<span \
                class=\"type\">%s</span>\" %s %s<br/>%s;</div>"
               acc
               name
               (string_of_mtype
                  ~protect:()
                  ~html:true
                  (Option.default mt_unit tparameter))
               (string_of_mtype ~protect:() ~html:true treturn)
               (render_tinstr ~show_stack:false 2 code))
       ""
       views)

let profile_of_arity (m, n) = (m, Some (n - m))

let arity_of_profile = function
  | m, None -> (m, None)
  | m, Some d -> (m, Some (m + d))

let profile =
  let open Result in
  let ( =?= ) x y =
    match x with
    | Some x when x <> y -> error "profile: unequal p"
    | _ -> return ()
  in
  let if_some d x = Option.map (fun _ -> x) d in
  let same x y =
    match (x, y) with
    | Some x, Some y ->
        if x = y then return (Some x) else error "profile: unequal d"
    | None, Some y -> return (Some y)
    | Some x, None -> return (Some x)
    | None, None -> return None
  in
  let pred = Option.map (fun x -> x - 1) in
  let succ = Option.map (fun x -> x + 1) in
  let f_instr i =
    let* i = sequence_instr_f i in
    match i with
    | MI1_fail _ -> return (1, None)
    | MIdip (p, d) -> return (p + 1, d)
    | MIdipn (i, (p, d)) -> return (p + i, d)
    | MIloop (p, d) ->
        let* () = d =?= 1 in
        return (p + 1, if_some d (-1))
    | MIloop_left (p, d) ->
        let* () = d =?= 1 in
        return (max 1 p, if_some d 0)
    | MIiter (p, d) ->
        let* () = d =?= -1 in
        return (p, Some (-1))
    | MImap (p, d) ->
        let* () = d =?= 0 in
        return (p, if_some d 0)
    | MIdig n | MIdug n -> return (n + 1, Some 0)
    | MIif ((p1, d1), (p2, d2)) ->
        let* d = same d1 d2 in
        return (max p1 p2 + 1, pred d)
    | MIif_none ((p1, d1), (p2, d2)) ->
        let* d = same (pred d1) d2 in
        return (max (p1 + 1) p2, d)
    | MIif_left ((p1, d1), (p2, d2)) ->
        let* d = same d1 d2 in
        return (max p1 p2, d)
    | MIif_cons ((p1, d1), (p2, d2)) ->
        let* d = same (succ d1) (pred d2) in
        return (max (p1 + 1) (p2 - 1), d)
    | MIseq xs ->
        let f = function
          | _, Error e -> Error e
          | (p1, None), _ -> return (p1, None)
          (* TODO Fix the compiler/rewriter such that they never emit
             instructions after FAILWITH and assert false here. *)
          | (p1, Some d1), Ok (p2, Some d2) ->
              return (max p1 (p2 - d1), Some (d1 + d2))
          | (p1, Some d1), Ok (p2, None) -> return (max p1 (p2 - d1), None)
        in
        List.fold_right (curry f) xs (return (0, Some 0))
    | MIcomment _ -> return (0, Some 0)
    | MIlambda _ -> return (0, Some 1)
    | MIconcat_unresolved -> failwith "profile: CONCAT arity undetermined"
    | MIerror _ -> return (0, Some 0)
    | i ->
      ( match spec_of_instr ~strict_dup:false i with
      | {arities = Some a} -> return (profile_of_arity a)
      | _ -> assert false )
  in
  cata_instr {f_instr; f_literal = (fun _ -> return ())}

let has_profile pr instr = Ok pr = profile {instr}

let has_arity a instr = has_profile (profile_of_arity a) instr

let arity instr = Result.map arity_of_profile (profile instr)

let rec mtype_examples t =
  match t.mt with
  | MT0 T_unit -> [MLiteral.unit]
  | MT0 T_bool -> [MLiteral.bool false; MLiteral.bool true]
  | MT0 T_nat ->
      [ MLiteral.small_int 0
      ; MLiteral.small_int 2
      ; MLiteral.small_int 4
      ; MLiteral.small_int 8 ]
  | MT0 T_int ->
      [ MLiteral.small_int (-2)
      ; MLiteral.small_int 10
      ; MLiteral.small_int 5
      ; MLiteral.small_int 3 ]
  | MT0 T_mutez ->
      [ MLiteral.small_int 0
      ; MLiteral.small_int 1000
      ; MLiteral.small_int 2000000
      ; MLiteral.small_int 3000000 ]
  | MT0 T_string ->
      [ MLiteral.string ""
      ; MLiteral.string "foo"
      ; MLiteral.string "bar"
      ; MLiteral.string "SmartPy" ]
  | MT0 T_chain_id
   |MT0 T_bytes
   |MT0 T_bls12_381_g1
   |MT0 T_bls12_381_g2
   |MT0 T_bls12_381_fr
   |MT0 T_chest_key
   |MT0 T_chest ->
      [ MLiteral.bytes ""
      ; MLiteral.bytes (Misc.Hex.unhex "00")
      ; MLiteral.bytes (Misc.Hex.unhex "010203")
      ; MLiteral.bytes (Misc.Hex.unhex "0FFF") ]
  | MT0 T_timestamp ->
      [ MLiteral.small_int 0
      ; MLiteral.small_int 1000
      ; MLiteral.small_int 2000000
      ; MLiteral.small_int 3000000 ]
  | MT0 T_address ->
      [ MLiteral.string "tz1..."
      ; MLiteral.string "tz2..."
      ; MLiteral.string "tz3..."
      ; MLiteral.string "KT1..." ]
  | MT0 T_key_hash ->
      [ MLiteral.string "tz1..."
      ; MLiteral.string "tz2..."
      ; MLiteral.string "tz3..." ]
  | MT0 T_signature -> [MLiteral.string "edsigt..."; MLiteral.string "edsigu..."]
  | MT0 T_key ->
      [ MLiteral.string "edpkuvNy6TuQ2z8o9wnoaTtTXkzQk7nhegCHfxBc4ecsd4qG71KYNG"
      ; MLiteral.string "edpkvThfdv8Efh1MuqSTUk5EnUFCTjqN6kXDCNXpQ8udN3cKRhNDr2"
      ]
  | MT1 (T_option, t) ->
      List.map MLiteral.some (mtype_examples t) @ [MLiteral.none]
  | MT1 (T_list, t) -> [MLiteral.seq (mtype_examples t); MLiteral.seq []]
  | MT1 (T_set, t) -> [MLiteral.seq (mtype_examples t)]
  | MT1 (T_contract, _t) ->
      [ MLiteral.string "KT1a..."
      ; MLiteral.string "KT1b..."
      ; MLiteral.string "KT1c..."
      ; MLiteral.string "KT1d..." ]
  | MT2 (T_pair _, fst, snd) ->
      let l1 = mtype_examples fst in
      let l2 = mtype_examples snd in
      begin
        match (l1, l2) with
        | a1 :: a2 :: _, b1 :: b2 :: _ ->
            [ MLiteral.pair a1 b1
            ; MLiteral.pair a2 b2
            ; MLiteral.pair a1 b2
            ; MLiteral.pair a2 b1 ]
        | _ ->
            List.fold_left
              (fun acc b ->
                List.fold_left (fun acc a -> MLiteral.pair a b :: acc) acc l1)
              []
              l2
      end
  | MT2 (T_or _, left, right) ->
      let l1 = mtype_examples left in
      let l2 = mtype_examples right in
      begin
        match (l1, l2) with
        | a1 :: a2 :: _, b1 :: b2 :: _ ->
            [ MLiteral.left a1
            ; MLiteral.left a2
            ; MLiteral.right b1
            ; MLiteral.right b2 ]
        | _ ->
            List.fold_left
              (fun acc b ->
                List.fold_left (fun acc a -> MLiteral.pair a b :: acc) acc l1)
              []
              l2
      end
  | MT2 (T_lambda, _, _) -> [MLiteral.seq []]
  | MT2 ((T_map | T_big_map), k, v) ->
      let l1 = mtype_examples k in
      let l2 = mtype_examples v in
      let rec map2 f acc l1 l2 =
        match (l1, l2) with
        | a1 :: l1, a2 :: l2 -> map2 f (f a1 a2 :: acc) l1 l2
        | _ -> List.rev acc
      in
      let l = map2 MLiteral.elt [] l1 l2 in
      [MLiteral.seq l]
  | MT_var s -> [MLiteral.string (sprintf "no value for %S" s)]
  | MT0 T_operation -> [MLiteral.string "operation"]
  | MT0 (T_sapling_state _) -> [MLiteral.seq []]
  | MT0 T_never -> [MLiteral.string "no value in type never"]
  | MT0 (T_sapling_transaction _) -> [MLiteral.string "sapling_transaction"]
  | MT1 (T_ticket, _) -> [MLiteral.string "no example for tickets"]

let on_instrs f =
  let f_instr instr = f {instr} in
  let f_literal literal = {literal} in
  cata_literal {f_instr; f_literal}

let display_instr t1 instr =
  let tinstr =
    typecheck_instr
      ~strict_dup:false
      ~tparameter:(mt_unit, None)
      (Stack_ok [t1])
      instr
  in
  display_tinstr ~show_stack:false ~new_line:false 0 tinstr
