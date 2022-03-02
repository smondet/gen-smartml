(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Control
open Basics
open Typed
open Michelson
open Utils.Misc
open Has_operations
open Printf

let replace subst =
  let f x = Option.default x (List.assoc_opt x subst) in
  List.map f

type scenario_vars = value String.Map.t

module Printer = (val Printer.get_by_language Config.SmartPy : Printer.Printer)

open Printer

let debug_tags = false

let debug_adapt = false

let debug_michel = false

let is_ok c = has_error_tcontract ~accept_missings:true c = []

let pipeline ~config =
  let open Michelson_rewriter in
  (if config.Config.erase_comments then remove_comments else [])
  @ if config.Config.simplify then simplify ~config else []

let simplify ~config c =
  Michelson_rewriter.run_on_tcontract (pipeline ~config) c

let simplify_via_michel ~config c =
  let st = Michel.Transformer.{var_counter = ref 0} in
  let ({storage; lazy_entry_points} : contract) = erase_types_contract c in
  let views = c.views in
  let c = Michel_decompiler.decompile_contract st c in
  let c = Michel.Transformer.michelsonify st c in
  let c' = Result.get_ok_exn (Michel.Typing.typecheck_precontract c) in
  let c' =
    Michel_compiler.compile_contract ?storage ?lazy_entry_points c' ~views
  in
  let c' =
    if is_ok c' && config.Config.simplify then simplify ~config c' else c'
  in
  (c, c')

(* NB This does not invoke simplification via Michel. *)
let optimize_instr ~config = Michelson_rewriter.run (pipeline ~config)

let optimize_literal ~config l =
  (* Comments are not properly pretty-printed in lambdas so we need to
     remove them. *)
  let config = {config with Config.erase_comments = true} in
  on_instrs (optimize_instr ~config) l

let optimize_tcontract ~config =
  let optimize_storage c =
    let c = erase_types_contract c in
    let f = Option.map (optimize_literal ~config) in
    let c =
      {c with storage = f c.storage; lazy_entry_points = f c.lazy_entry_points}
    in
    typecheck_contract ~strict_dup:true c
  in
  fun c ->
    if is_ok c
    then
      let c = optimize_storage c in
      if is_ok c
      then
        let c = simplify ~config c in
        if is_ok c && config.Config.simplify_via_michel
        then snd (simplify_via_michel ~config c)
        else c
      else c
    else c

(** {1 Actions for code construction} *)
type target =
  | T_var          of string
  | T_match_cons   of string * bool (* head *)
  | T_entry_points
  | T_self_address
[@@deriving eq, show {with_path = false}]

let tgt_parameter = T_var "__parameter__"

let tgt_storage = T_var "__storage__"

let tgt_operations = T_var "__operations__"

type stack_tag =
  | ST_none   of int
  | ST_target of target
[@@deriving eq, show]

let st_none =
  let c = ref 0 in
  fun () ->
    incr c;
    ST_none !c

let tag_operations = ST_target tgt_operations

let mk_instr instr = {instr}

let display_target = function
  | T_var "__parameter__" -> "params"
  | T_var "__storage__" -> "storage"
  | T_var "__operations__" -> "operations"
  | T_var n -> Printf.sprintf "var(%s)" n
  | T_match_cons (n, b) ->
      Printf.sprintf "match_cons(%s).%s" n (if b then "head" else "tail")
  | T_entry_points -> "entry_points"
  | T_self_address -> "self_address"

let display_stack_tag = function
  | ST_none i -> "#" ^ string_of_int i
  | ST_target t -> display_target t

let tags_for_instr instr tags =
  let auto (a_in, a_out) =
    Some List.(map st_none (replicate a_out ()) @ drop a_in tags)
  in
  match (tags, instr.instr) with
  | _, MIdup _ -> auto (0, 1)
  | _, MIdig n ->
    ( match List.split_at_opt n tags with
    | Some (hi, x :: lo) -> Some ((x :: hi) @ lo)
    | _ -> assert false )
  | _, MIdug n ->
    ( match List.split_at_opt (n + 1) tags with
    | Some (x :: hi, lo) -> Some (hi @ (x :: lo))
    | _ -> assert false )
  | _ ->
    ( match arity instr with
    | Ok (_, None) -> None
    | Ok (m, Some n) -> auto (m, n)
    | Error e -> failwith ("arity: " ^ e) )

let find_stack_tag t =
  let f offset t' = if equal_stack_tag t t' then Some offset else None in
  Base.List.find_mapi ~f

let move_to_bottom x xs =
  let xs = List.rev xs in
  let last = List.length xs - 1 in
  match find_stack_tag x xs with
  | None -> None
  | Some i when i = last -> Some ([], List.rev xs)
  | Some i ->
    ( match List.nth_rem i xs with
    | Some (x', xs) when equal_stack_tag x x' ->
        Some ([MIdig i; MIdug last], List.rev (xs @ [x]))
    | _ -> assert false )

let move_to_top x xs =
  match find_stack_tag x xs with
  | None -> assert false
  | Some 0 -> ([], Some xs)
  | Some i ->
    ( match List.nth_rem i xs with
    | Some (x', xs) when equal_stack_tag x x' -> ([MIdig i], Some ([x] @ xs))
    | _ -> assert false )

let rec adapt_branches r rx ry xs ys =
  let last_x = List.length xs - 1 in
  let last_y = List.length ys - 1 in
  let drop i = [MIdig i; MIdrop] in
  match (xs, ys) with
  | [], [] -> (Some r, rx, ry)
  | [], _ :: ys -> adapt_branches r rx (ry @ drop last_y) xs ys
  | _ :: xs, [] -> adapt_branches r (rx @ drop last_x) ry xs ys
  | x :: xs, y :: ys when equal_stack_tag x y ->
      adapt_branches (x :: r) rx ry xs ys
  | (x :: xs' as xs), (y :: ys' as ys) ->
    ( match (move_to_bottom y xs, move_to_bottom x ys) with
    | Some (ix, xs), _ -> adapt_branches r (rx @ ix) ry xs ys
    | _, Some (iy, ys) -> adapt_branches r rx (ry @ iy) xs ys
    | _ -> adapt_branches r (rx @ drop last_x) (ry @ drop last_y) xs' ys' )

let mark_top = function
  | None -> None
  | Some [] -> failwith "mark_top: empty stack"
  | Some (_ :: xs) -> Some (ST_target (T_var "__top__") :: xs)

(** Finds the offset of the first occurence of the target. *)
let find_target target =
  let f offset = function
    | ST_none _ -> None
    | ST_target t -> if target = t then Some offset else None
  in
  Base.List.find_mapi ~f

(** Does the stack have the given target? *)
let has_target target tags = Option.is_some (find_target target tags)

let adapt_branches xs ys =
  let has_ops = has_target tgt_operations in
  match (xs, ys) with
  | None, None -> (None, [], [])
  | Some xs, None | None, Some xs -> (Some xs, [], [])
  | Some xs, Some ys ->
      let instrs_xs, xs, instrs_ys, ys =
        match (has_ops xs, has_ops ys) with
        | true, true | false, false -> ([], xs, [], ys)
        | true, false -> ([], xs, [MI0 (Nil mt_operation)], tag_operations :: ys)
        | false, true -> ([MI0 (Nil mt_operation)], tag_operations :: xs, [], ys)
      in
      let tags, instrs_xs, instrs_ys =
        adapt_branches [] instrs_xs instrs_ys (List.rev xs) (List.rev ys)
      in
      (tags, List.map mk_instr instrs_xs, List.map mk_instr instrs_ys)

let mk_seq block =
  match block with
  | [i] -> i
  | _ -> {instr = MIseq block}

module type ACTION = sig
  include MONAD_CORE

  open Michelson

  type state = {tags : stack_tag list option}

  val run :
       config:Config.t
    -> scenario_vars:scenario_vars
    -> lazy_ids:(string * literal) list
    -> ?tstorage:Type.t
    -> state
    -> 'a t
    -> 'a option * state * instr list

  val get_config : Config.t t

  val get_scenario_vars : scenario_vars t

  val get_lazy_id : string -> literal t

  val get_tstorage : Type.t option t

  val with_lazy_ids : (string * literal) list -> 'a t -> 'a t

  val abort : string -> 'a t

  val instr : (instr, literal) instr_f -> unit t

  val composite : ?protect_top:unit -> (unit t, literal) instr_f -> unit t

  val block : state -> unit t -> (instr list * state) t

  val set_tags : stack_tag list option -> unit t

  val get_tags : stack_tag list option t
end

(** A monad that can read the (immutable) parameter and storage types,
   as well as modify the state: (1) the instructions emitted within
   the current block and (2) the current stack. *)
module Action : ACTION = struct
  type state = {tags : stack_tag list option}

  type const =
    { config : Config.t
    ; scenario_vars : scenario_vars
    ; lazy_ids : (string * literal) list
    ; tstorage : Type.t option }

  type 'a t = const -> state -> 'a option * state * instr list

  let run ~config ~scenario_vars ~lazy_ids ?tstorage tags x =
    x {config; scenario_vars; lazy_ids; tstorage} tags

  let return (x : 'a) : 'a t = fun _c s -> (Some x, s, [])

  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
   fun c s ->
    match x c s with
    | Some x', s, i ->
        let r, s, i' = (f x') c s in
        (r, s, i @ i')
    | None, si, i -> (None, si, i)

  let block tags body c s =
    let _r, s', i = body c tags in
    (Some (i, s'), s, [])

  let set_tags tags _c _s = (Some (), {tags}, [])

  let get_tags _c s = (Some s.tags, s, [])

  let map f x = bind x (fun x -> return (f x))

  let apply f x = bind f (fun f -> bind x (fun x -> return (f x)))

  let mk_cond
      ?protect_top cond (x_result, x_state, x_block) (y_result, y_state, y_block)
      =
    let result =
      match (x_result, y_result) with
      | Some (), Some () -> Some ()
      | _, _ -> None
    in
    let x_tags, y_tags =
      if protect_top = Some ()
      then (mark_top x_state.tags, mark_top y_state.tags)
      else (x_state.tags, y_state.tags)
    in
    let tags, x_post, y_post = adapt_branches x_tags y_tags in
    let x = {instr = MIseq (x_block @ x_post)} in
    let y = {instr = MIseq (y_block @ y_post)} in
    match tags with
    | Some tags when protect_top = Some () ->
        let xs, tags = move_to_top (ST_target (T_var "__top__")) tags in
        (result, {tags}, List.map mk_instr (cond x y :: xs))
    | _ -> (result, {tags}, [mk_instr (cond x y)])

  let get_config : Config.t t = fun c s -> (Some c.config, s, [])

  let get_scenario_vars c s = (Some c.scenario_vars, s, [])

  let get_tstorage c s = (Some c.tstorage, s, [])

  let get_lazy_id name c s =
    match List.assoc_opt name c.lazy_ids with
    | Some id -> (Some id, s, [])
    | None -> failwith ("get_lazy_id: " ^ name)

  let with_lazy_ids lazy_ids x c s = x {c with lazy_ids} s

  let abort msg _c s = (None, s, [{instr = MIerror msg}])

  let instr instr : unit t =
   fun _c s ->
    let i = {instr} in
    match s.tags with
    | None -> (Some (), s, [])
    | Some tags -> (Some (), {tags = tags_for_instr i tags}, [i])

  let rec composite ?protect_top i : unit t =
   fun c s ->
    let err msg = (None, s, [{instr = MIerror msg}]) in
    match (s.tags, i) with
    | _, MIseq (x :: xs) ->
        let f () = composite ?protect_top (MIseq xs) in
        bind x f c s
    | Some (_ :: tail), MIif (x, y) ->
        let x = x c {tags = Some tail} in
        let y = y c {tags = Some tail} in
        mk_cond ?protect_top (fun l r -> MIif (l, r)) x y
    | _, MIif _ -> err "IF"
    | Some (_ :: tail), MIif_none (x, y) ->
        let x = x c {tags = Some tail} in
        let y = y c {tags = Some (st_none () :: tail)} in
        mk_cond ?protect_top (fun l r -> MIif_none (l, r)) x y
    | _, MIif_none _ -> err "IF_NONE"
    | Some (_ :: tail), MIif_left (x, y) ->
        let x = x c {tags = Some (st_none () :: tail)} in
        let y = y c {tags = Some (st_none () :: tail)} in
        mk_cond ?protect_top (fun l r -> MIif_left (l, r)) x y
    | _, MIif_left _ -> err "IF_LEFT"
    | Some (_ :: tail), MIif_cons (x, y) ->
        let x = x c {tags = Some (st_none () :: st_none () :: tail)} in
        let y = y c {tags = Some tail} in
        mk_cond ?protect_top (fun l r -> MIif_cons (l, r)) x y
    | _, MIif_cons _ -> err "IF_CONS"
    | Some (_ :: tail), MImap body ->
        let result, _state, block = body c {tags = Some (st_none () :: tail)} in
        ( result
        , {tags = Some (st_none () :: tail)}
        , [mk_instr (MImap (mk_seq block))] )
    | _, MImap _ -> err "MAP"
    | Some (_ :: tail), MIiter body ->
        let result, _state, block = body c {tags = Some (st_none () :: tail)} in
        (result, {tags = Some tail}, [mk_instr (MIiter (mk_seq block))])
    | _, MIiter _ -> err "ITER"
    | Some (_ :: tail), MIloop body ->
        let result, _state, block = body c {tags = Some tail} in
        (result, {tags = Some tail}, [mk_instr (MIloop (mk_seq block))])
    | Some tags, MIlambda (t_in, t_out, body) ->
        let result, _state, block = body c {tags = Some [st_none ()]} in
        (* TODO raise error if output stack is not a singleton *)
        ( result
        , {tags = Some (st_none () :: tags)}
        , [mk_instr (MIlambda (t_in, t_out, mk_seq block))] )
    | _, MIlambda _ -> err "LAMBDA"
    | None, _ -> (Some (), s, [])
    | Some tags, i ->
        let dummy ppf _ = Format.fprintf ppf "..." in
        let pp = pp_instr_f dummy dummy in
        let f _ = failwith (Format.asprintf "emit_instr: %a" pp i) in
        let instr = map_instr_f f id i in
        (Some (), {tags = tags_for_instr {instr} tags}, [{instr}])
end

module ActionM = struct
  include Action
  include Monad (Action)
end

open ActionM
module T = Binary_tree.Traversable (ActionM)

let mi_seq = iter_list id

let display_ok_tags s =
  let tags = List.map display_stack_tag s in
  sprintf "[ %s ]" (String.concat " : " tags)

let display_tags = function
  | None -> sprintf "FAILED"
  | Some s -> display_ok_tags s

let instrs = iter_list instr

let comment msg = instr (MIcomment [msg])

let error msg = abort msg

let commentf fmt = ksprintf comment fmt

let errorf fmt = ksprintf error fmt

let comment_if c fmt =
  let comment msg = if c then comment msg else return () in
  ksprintf comment fmt

let not_implemented =
  ksprintf (fun x -> failwith ("[compiler] Not implemented: " ^ x))

let _debug msg =
  let* tags = get_tags in
  if false then Printf.printf "%s %s\n" msg (display_tags tags);
  commentf "%s %s" msg (display_tags tags)

let get_ok_stack lbl =
  let* tags = get_tags in
  match tags with
  | Some tags -> return tags
  | _ -> error ("get_ok_stack: " ^ lbl)

let compile_michel e =
  let open Michel_compiler in
  let of_target = function
    | T_var x -> x
    | T_match_cons _ -> not_implemented "michel T_match_cons"
    | T_entry_points -> "__entry_points__"
    | T_self_address -> (* unused *) assert false
  in
  let of_tag = function
    | ST_none i -> "#" ^ string_of_int i
    | ST_target tgt -> of_target tgt
  in
  let to_target = function
    | "__entry_points__" -> T_entry_points
    | x -> T_var x
  in
  let to_tag x =
    if String.sub x 0 1 = "#"
    then ST_none (int_of_string (String.drop 1 x))
    else ST_target (to_target x)
  in
  let to_stack = function
    | Stack_failed -> None
    | Stack_ok stack ->
        Some
          (List.map
             (function
               | Some x -> to_tag x
               | None -> st_none ())
             stack)
  in
  get_tags
  >>= function
  | None -> return ()
  | Some tags ->
      let names = List.map of_tag tags in
      let stack = Stack_ok (List.map Option.some names) in
      let e = e names in
      if debug_michel
      then Format.printf "Michel:@.%a@." Michel.Expr.print_expr e;
      let xs, stack = Michel_compiler.compile_expr stack e in
      let* () = instrs (List.map (fun {instr} -> instr) xs) in
      set_tags (to_stack stack)

(** {1 Michelson helpers} *)

let mi_car = instr (MIfield [A])

let mi_cdr = instr (MIfield [D])

(* Equivalent to DUP n, but with zero-based index. *)
let mi_copy = function
  | 0 -> instr (MIdup 1)
  | n ->
      let* tags = get_tags in
      ( match tags with
      | None -> assert false
      | Some tags ->
          let* () = instrs [MIdig n; MIdup 1; MIdug (n + 1)] in
          set_tags (Some (st_none () :: tags)) )

let push_unit = instr (MIpush (mt_unit, MLiteral.unit))

let mi_failwith ?verify ~default ~line_no ~context ?(args = push_unit) s =
  let* {exceptions} = get_config in
  let line_no = head_line_no line_no in
  let i =
    match exceptions with
    | Unit -> push_unit
    | DefaultUnit -> if default then push_unit else args
    | Line -> instr (MIpush (mt_int, MLiteral.small_int line_no))
    | VerifyOrLine ->
        if (not default) || verify = Some ()
        then args
        else instr (MIpush (mt_int, MLiteral.small_int line_no))
    | DefaultLine ->
        if not default
        then args
        else instr (MIpush (mt_int, MLiteral.small_int line_no))
    | Message ->
        instrs
          [ MIpush (mt_int, MLiteral.small_int line_no)
          ; MIpush (mt_string, MLiteral.string s)
          ; MI2 (Pair (None, None)) ]
    | FullDebug ->
        let* () = args in
        instrs
          [ MIpush (mt_string, MLiteral.string context)
          ; MI2 (Pair (None, None))
          ; MIpush (mt_int, MLiteral.small_int line_no)
          ; MI2 (Pair (None, None))
          ; MIpush (mt_string, MLiteral.string s)
          ; MI2 (Pair (None, None)) ]
  in
  mi_seq [comment s; i; instr (MI1_fail Failwith)]

(** {1 Stack tags and targets} *)

let tag_top tags =
  let* ts = get_ok_stack "tag_top" in
  if List.length tags <= List.length ts
  then
    let* () = set_tags (Some (tags @ List.(drop (length tags) ts))) in
    comment_if debug_tags "tag_top"
  else error "tag_top: stack too short"

let fetch_target ~dup target =
  let* tags = get_ok_stack "fetch_target" in
  match find_target target tags with
  | None ->
      errorf
        "fetch_target %s in [%s]"
        (show_target target)
        (String.concat "; " (List.map show_stack_tag tags))
  | Some offset when dup -> mi_copy offset
  | Some offset -> instr (MIdig offset)

let rec equal_stack_tags xs ys =
  match (xs, ys) with
  | [], [] -> true
  | x :: xs, y :: ys -> equal_stack_tag x y && equal_stack_tags xs ys
  | _ -> false

let compatible_stack_tags xs ys =
  match (xs, ys) with
  | Some xs, Some ys -> equal_stack_tags xs ys
  | _ -> (* FAILED is compatible with anything *) true

let rec adapt_stack offset desired =
  let* current = get_tags in
  let* () = comment_if debug_adapt "adapt_stack %d" offset in
  match current with
  | None -> return ()
  | Some current ->
      let current = List.rdrop offset current in
      ( match (current, desired) with
      | [], [] -> return ()
      | [], _ -> assert false
      | _, [] ->
          let* () = comment_if debug_adapt "dropping" in
          let* () = instr MIdrop in
          adapt_stack offset desired
      | _ :: _, _ :: _ ->
          let ts, t = List.unsnoc desired in
          let f i c = if equal_stack_tag c t then Some i else None in
          let n = List.length current - 1 in
          let i = Base.List.find_mapi ~f current in
          let* () =
            comment_if
              debug_adapt
              "%s at %s (looking till %d)"
              (display_stack_tag t)
              (Option.cata "NA" string_of_int i)
              n
          in
          let* () =
            match i with
            | Some i when i = n -> return ()
            | Some i when i < n ->
                let* () = when_ (i <> 0) (instr (MIdig i)) in
                instr (MIdug n)
            | _ -> assert false
          in
          adapt_stack (offset + 1) ts )

let adapt_stack desired =
  let* current = get_tags in
  let* () = comment_if debug_adapt "current    : %s" (display_tags current) in
  let* () = comment_if debug_adapt "desired     : %s" (display_tags desired) in
  match (current, desired) with
  | Some current, Some desired ->
      let f t = Option.is_some (List.find_opt (equal_stack_tag t) current) in
      let available = List.filter f desired in
      let* () =
        comment_if debug_adapt "available  : %s" (display_ok_tags available)
      in
      let* () = adapt_stack 0 available in
      let* result = get_tags in
      (* TODO instead of printing, assert available = result *)
      let* () =
        comment_if debug_adapt "result     : %s" (display_tags result)
      in
      unless
        (compatible_stack_tags (Some available) result)
        (error "available /= result")
  | None, _ -> comment_if debug_adapt "not adapting failed stack"
  | _, None -> comment_if debug_adapt "desired stack failed"

let if_stack_ok x =
  let* tags = get_tags in
  match tags with
  | Some tags -> x tags
  | None -> return ()

let drop_target target =
  if_stack_ok (fun tags ->
      match find_target target tags with
      | None ->
          comment_if debug_adapt "drop_target skipping %s" (show_target target)
      | Some offset -> instrs [MIdig offset; MIdrop])

let unzip_target ~copy target =
  let err msg =
    let* () = errorf "unzip_target: %s" msg in
    return (false, return ())
  in
  let* tags = get_tags in
  match tags with
  | None -> err "stack failed"
  | Some tags ->
      let* stack_original = get_tags in
      ( match find_target target tags with
      | None -> return (false, tag_top [ST_target target])
      | Some offset when copy ->
          let* () = mi_copy offset in
          return
            ( true
            , let* () = drop_target target in
              let* () = tag_top [ST_target target] in
              adapt_stack stack_original )
      | Some offset ->
          let* () = instr (MIdig offset) in
          return
            ( true
            , let* () = tag_top [ST_target target] in
              adapt_stack stack_original ) )

(** {1 Stack unification with operations} *)

let ops_init =
  let* () = instr (MI0 (Nil mt_operation)) in
  tag_top [ST_target tgt_operations]

let mi_if ?protect_top l r = composite ?protect_top (MIif (l, r))

let mi_if_none ?protect_top l r = composite ?protect_top (MIif_none (l, r))

let mi_if_left ?protect_top l r = composite ?protect_top (MIif_left (l, r))

let mi_if_cons ?protect_top l r = composite ?protect_top (MIif_cons (l, r))

let mi_map body = composite (MImap body)

let mi_iter body = composite (MIiter body)

let mi_loop body = composite (MIloop body)

let tree_of_layout row =
  Binary_tree.map (fun Layout.{source; target} ->
      match List.assoc_opt source row with
      | Some t -> (source, target, t)
      | None ->
          Printf.ksprintf
            failwith
            "Missing layout field %S in [%s]"
            source
            (String.concat "; " (List.map fst row)))

let get_layout row layout =
  match Unknown.get layout with
  | None ->
      Printf.ksprintf
        failwith
        "Missing layout for record %s"
        (String.concat " " (List.map fst row))
  | Some layout -> layout

let rec compile_tuple f = function
  | [] | [_] -> failwith "compile_tuple"
  | [x; y] -> f x y
  | x :: xs -> f x (compile_tuple f xs)

let compile_literal ?for_error (l : Basics.Literal.t) =
  let module L = MLiteral in
  let for_error =
    Option.cata [] (fun (e : texpr) -> [`Expr e; `Line e.line_no]) for_error
  in
  match l with
  | Unit -> L.unit
  | Int {i} -> L.int i
  | Timestamp i ->
      let i = Bigint.int_of_big_int i in
      ( match Ptime.of_span (Ptime.Span.of_int_s i) with
      | None -> failwith (sprintf "timestamp %d out of range" i)
      | Some t -> L.string (Ptime.to_rfc3339 ~tz_offset_s:0 t) )
  | Mutez i -> L.int i
  | String s -> L.string s
  | Bytes s -> L.bytes s
  | Chain_id s -> L.bytes s
  | Key_hash s -> L.string s
  | Signature s -> L.string s
  | Key s -> L.string s
  | Address {address; entry_point} ->
      let entry_point =
        match entry_point with
        | Some "default" -> None
        | _ -> entry_point
      in
      let s =
        sprintf "%s%s" address (Option.cata "" (sprintf "%%%s") entry_point)
      in
      L.string s
  | Bool b -> L.bool b
  | Secret_key _ ->
      raise
        (SmartExcept
           ([`Text "Secret keys are forbidden in contracts"] @ for_error))
  | Sapling_test_state _ -> L.sapling_empty_state
  | Sapling_test_transaction _ -> L.string "FAKE_SAPLING_TRANSACTION"
  | Bls12_381_g1 s -> L.bytes s
  | Bls12_381_g2 s -> L.bytes s
  | Bls12_381_fr s -> L.bytes s
  | Chest_key s -> L.bytes s
  | Chest s -> L.bytes s

let op_of_attr (name : string) t =
  let rec get acc = function
    | Binary_tree.Leaf Layout.{target} ->
        if target = name then Some (List.rev acc) else None
    | Node (fst, snd) -> get (A :: acc) fst <|> fun () -> get (D :: acc) snd
  in
  match Type.getRepr t with
  | TRecord {row; layout} -> get [] (get_layout row layout)
  | _ -> assert false

(** {1 Expressions} *)

let mi_rev_list t =
  let* () =
    instrs [MI0 (Nil (Typing.mtype_of_type ~with_annots:false t)); MIdig 1]
  in
  mi_iter (instr (MI2 Cons))

(** Ensure an operations element is on the stack if the given command
   may output any. *)
let ensure_ops =
  let* tags = get_ok_stack "ensure_ops_if_out" in
  if has_target tgt_operations tags then return () else ops_init

let ensure_ops_if c = if c then ensure_ops else return ()

type lstep =
  | LMapItem of texpr * Type.t
  | LAttr    of string * Type.t

type lroot =
  | LOperations
  | LStorage
  | LVar        of string

type lexpr =
  { lroot : lroot
  ; lsteps : lstep list }

let occurs_lroot_alg root =
  let a = monoid_para_talg ( || ) false in
  let f_texpr ln t e =
    let y = a.f_texpr ln t e in
    match e with
    | EVar (n, _) ->
        ( fst y
        , ( match root with
          | LOperations -> n = "__operations__"
          | LStorage -> n = "__storage__"
          | LVar n' -> n = n' ) )
    | EPrim2 (ECallLambda, _, (lambda, _)) ->
        ( fst y
        , ( match Type.unF lambda.et with
          | TLambda ({with_storage; with_operations}, _, _) ->
              snd y
              ||
              ( match root with
              | LOperations -> Unknown.get_ok "with_operations" with_operations
              | LStorage ->
                begin
                  match Unknown.get_ok "with_storage" with_storage with
                  | None -> false
                  | Some (Read_only | Read_write) -> true
                end
              | LVar _ -> false )
          | _ -> assert false ) )
    | _ -> y
  in
  {a with f_texpr}

let occurs_lroot_expr root = para_texpr (occurs_lroot_alg root)

let occurs_lroot_command root = para_tcommand (occurs_lroot_alg root)

let self_referential_lexpr {lroot; lsteps} =
  let f = function
    | LMapItem (e, _) -> occurs_lroot_expr lroot e
    | _ -> false
  in
  List.exists f lsteps

let extend_lexpr {lroot; lsteps} s = {lroot; lsteps = lsteps @ [s]}

let last_step_is_item {lroot; lsteps} =
  if List.length lsteps > 0
  then
    match List.unsnoc lsteps with
    | lsteps, LMapItem (key, tvalue) -> Some ({lroot; lsteps}, key, tvalue)
    | _ -> None
  else None

let lexpr_of_expr =
  let rec of_expr acc e =
    match e.e with
    | EVar ("__operations__", _) -> Some {lroot = LOperations; lsteps = acc}
    | EVar ("__storage__", _) -> Some {lroot = LStorage; lsteps = acc}
    | EVar (name, _) -> Some {lroot = LVar name; lsteps = acc}
    | EPrim1 (EAttr name, {e = EPrim0 (EMetaLocal loc)}) ->
        Some {lroot = LVar (Printf.sprintf "%s.%s" loc name); lsteps = acc}
    | EItem {items; key; default_value = None; missing_message = None} ->
      ( match items.et with
      | F (T2 ((T_map | T_big_map), _, tvalue)) ->
          of_expr (LMapItem (key, tvalue) :: acc) items
      | _ -> assert false )
    | EPrim1 (EAttr name, expr) -> of_expr (LAttr (name, expr.et) :: acc) expr
    | _ -> None
  in
  of_expr []

(* There are two ways to modify a variable: (1) assign to it directly;
   (2) iterate over it or one of its substructures (nested loop). *)
let rec modifies_var i c =
  let touches lhs =
    match lexpr_of_expr lhs with
    | None ->
        raise
          (SmartExcept
             [ `Text "Forbidden expression in left position of an assignment."
             ; `Expr lhs
             ; `Line c.line_no ])
    | Some {lroot = LVar name} when name = i -> true
    | Some _ -> false
  in
  match c.c with
  | CSetVar (lhs, _) | CDelItem (lhs, _) | CUpdateSet (lhs, _, _) -> touches lhs
  | CModifyProduct (lhs, _, body) -> touches lhs || modifies_var i body
  | CFor (name, container, body) ->
      modifies_var i body
      ||
      ( match lexpr_of_expr container with
      | Some {lroot = LVar cname} when i = cname -> modifies_var name body
      | _ -> false )
  | CIf (_, c1, c2) -> modifies_var i c1 || modifies_var i c2
  | CBind (_, c1, c2) -> modifies_var i c1 || modifies_var i c2
  | CWhile (_, c) -> modifies_var i c
  | CNever _ | CFailwith _
   |CVerify (_, _)
   |CMatch (_, _)
   |CMatchProduct (_, _, _)
   |CMatchCons _ | CDefineLocal _ | CResult _ | CComment _
   |CSetType (_, _)
   |CSetResultType (_, _)
   |CTrace _ | CSetEntryPoint _ ->
      false

let rec un_comb acc = function
  | [] -> Some acc
  | [A] -> Some (acc + 1)
  | A :: _ -> None
  | D :: rest -> un_comb (acc + 2) rest

let mi_unpair k = MIunpair (List.replicate k true)

(** Brings the target into focus, leaves a zipper on the stack. *)
let rec mi_unzip steps comb =
  let* {pairn} = get_config in
  match comb with
  | Some (n, duppable) when n >= 1 && pairn ->
      if duppable
      then instrs [MIdup 1; MI1 (Getn n)]
      else
        let k = (n + 1) / 2 in
        instrs [mi_unpair (k + 1); MIdig (n / 2)]
  | _ ->
    ( match steps with
    | A :: rest -> mi_seq [instr (mi_unpair 2); mi_unzip rest comb]
    | D :: rest ->
        let* () = instrs [mi_unpair 2; MIdig 1] in
        mi_unzip rest comb
    | [] -> return () )

let rec mi_zip steps comb =
  let* {pairn} = get_config in
  match comb with
  | Some (n, duppable) when n >= 1 && pairn ->
      if duppable
      then instr (MI2 (Updaten n))
      else
        let k = (n + 1) / 2 in
        instrs [MIdug (n / 2); MIpairn (k + 1)]
  | _ ->
    ( match steps with
    | A :: rest -> mi_seq [mi_zip rest comb; instr (MI2 (Pair (None, None)))]
    | D :: rest ->
        let* () = mi_zip rest comb in
        instrs [MIdig 1; MI2 (Pair (None, None))]
    | [] -> return () )

let target_of_lexpr = function
  | {lroot = LOperations; lsteps} -> (tgt_operations, lsteps)
  | {lroot = LStorage; lsteps} -> (tgt_storage, lsteps)
  | {lroot = LVar name; lsteps} -> (T_var name, lsteps)

let renaming_of_type ~rev t =
  match
    match Type.getRepr t with
    | TRecord {layout} -> Unknown.get layout
    | _ -> None
  with
  | None -> Hashtbl.create 5
  | Some layout ->
      let renaming = Hashtbl.create 5 in
      let rec aux = function
        | Binary_tree.Leaf Layout.{source; target} ->
            if rev
            then Hashtbl.replace renaming target source
            else Hashtbl.replace renaming source target
        | Binary_tree.Node (l1, l2) ->
            aux l1;
            aux l2
      in
      aux layout;
      renaming

let compile_record ~layout entries =
  layout
  |> tree_of_layout entries
  |> Binary_tree.map (fun (source, target, _) ->
         (target, List.assoc_exn ~msg:"compile_record" source entries))

(** Puts the specified fields on the stack and returns a list
   reflecting their order. *)
let explode_record ?name bs =
  let open Layout in
  let bs = List.map (fun {var; field} -> (field, var)) bs in
  let get_target t = List.assoc_opt t bs in
  let is_target {target} = Option.is_some (get_target target) in
  let build_target t =
    let t =
      match name with
      | None -> t
      | Some name -> Printf.sprintf "%s.%s" name t
    in
    ST_target (T_var t)
  in
  let rec explode (x : t) =
    let* config = get_config in
    match x with
    | Leaf {target} ->
      ( match List.assoc_opt target bs with
      | Some var ->
          let* () = tag_top [build_target var] in
          return [var]
      | None ->
          let* () = instr MIdrop in
          return [] )
    | l
      when ( match config.protocol with
           | Delphi -> false
           | Edo | Florence | Granada | Hangzhou | Ithaca -> true )
           && Binary_tree.(is_right_comb l && for_all is_target l) ->
        let l = Binary_tree.to_list l in
        let* () = instr (MIunpair (List.map (fun _ -> true) l)) in
        let f {target} = Option.get ~msg:"explode" (get_target target) in
        let targets = List.map f l in
        let* () = tag_top (List.map build_target targets) in
        return targets
    | Node (l, r) ->
        let* () = instr (mi_unpair 2) in
        let* l = explode l in
        let* () = instr (MIdig (List.length l)) in
        let* r = explode r in
        return (r @ l)
  in
  explode

let explode_single_pattern v =
  let* () = tag_top [ST_target (T_var v)] in
  return (drop_target (T_var v))

let explode_tuple_pattern ns =
  let* config = get_config in
  let n = List.length ns in
  let open List in
  match config.protocol with
  | Edo | Florence | Granada | Hangzhou | Ithaca ->
      let* () = instr (mi_unpair n) in
      let* () = tag_top (map (fun n -> ST_target (T_var n)) ns) in
      return (iter_list (fun n -> drop_target (T_var n)) (rev ns))
  | Delphi ->
      let* () =
        (* "UNPAIR n" in reverse order: *)
        iter_list
          (fun _ -> instrs [mi_unpair 2; MIdig 1])
          (replicate (n - 1) ())
      in
      let* () = tag_top (rev (map (fun n -> ST_target (T_var n)) ns)) in
      return (iter_list (fun n -> drop_target (T_var n)) ns)

let explode_record_pattern ?name t bs =
  match Type.getRepr t with
  | TRecord {row; layout} ->
      let* targets = explode_record ?name bs (get_layout row layout) in
      return (iter_list (fun t -> drop_target (T_var t)) targets)
  | _ -> assert false

let explode_pattern ?name t = function
  | Pattern_single v -> explode_single_pattern v
  | Pattern_tuple ns -> explode_tuple_pattern ns
  | Pattern_record (_, bs) -> explode_record_pattern ?name t bs

let open_record_pattern name t bs =
  match Type.getRepr t with
  | TRecord {row; layout} ->
      let* _ = explode_record ~name bs (get_layout row layout) in
      let close =
        let l (field, _) =
          let var_name = Printf.sprintf "%s.%s" name field in
          fetch_target ~dup:false (T_var var_name)
        in
        let p x1 x2 = mi_seq [x2; x1; instr (MI2 (Pair (None, None)))] in
        let entries = List.map (fun {field} -> (field, field)) bs in
        let msg = "open_record_pattern" in
        let layout = Option.get ~msg (Unknown.get layout) in
        let r = compile_record ~layout entries in
        Binary_tree.cata l p r
      in
      return close
  | _ -> assert false

let compile_option t name x =
  match name with
  | "None" ->
      instr
        (MIpush
           (mt_option (Typing.mtype_of_type ~with_annots:false t), MLiteral.none))
  | "Some" -> mi_seq [x; instr (MI1 Some_)]
  | _ -> assert false

let compile_or tl tr name x =
  match name with
  | "Left" ->
      mi_seq
        [ x
        ; instr
            (MI1 (Left (None, None, Typing.mtype_of_type ~with_annots:false tr)))
        ]
  | "Right" ->
      mi_seq
        [ x
        ; instr
            (MI1
               (Right (None, None, Typing.mtype_of_type ~with_annots:false tl)))
        ]
  | _ -> failwith (sprintf "Bad variant expecting Left/Right but got %s" name)

let compile_variant ~row ~layout name =
  layout
  |> tree_of_layout row
  |> Binary_tree.find_leaf (fun (_, target, _) -> target = name)
  |> Option.of_some ~msg:"compile_variant"
  |> snd
  |> Binary_tree.map_context (fun (_, _, x) -> x)

let unzip_attr name t =
  let err =
    let* () = error "unzip_attr" in
    return (return ())
  in
  match op_of_attr name t with
  | None -> err
  | Some op ->
      let comb =
        match un_comb 0 op with
        | None -> None
        | Some n ->
            let t = Typing.mtype_of_type ~with_annots:false t in
            Some (n, Michelson.is_duppable t)
      in
      let* () = mi_unzip op comb in
      return (mi_zip op comb)

let unzip_map key tvalue =
  let* () =
    if Type.is_hot tvalue = No
    then
      let* () = instr (MIdup 1) (* x x *) in
      let* () = key (* k x x *) in
      let* () = tag_top [st_none ()] in
      let* () = instr (MIdup 1) (* k k x x *) in
      let* () = instr (MIdug 2) (* k x k x *) in
      let* () = instr (MI2 Get) (* v k x *) in
      return ()
    else
      let* () =
        instr (MI0 (None_ (Typing.mtype_of_type ~with_annots:false tvalue)))
      in
      (* v x x *)
      let* () = key (* k v x *) in
      let* () = instr (MIdup 1) (* k k v x *) in
      let* () = instr (MIdug 3) (* k v x k *) in
      let* () = instr (MI3 Get_and_update) (* v x k *) in
      let* () = instr (MIdig 2) (* k v x *) in
      let* () = instr (MIdig 1) (* v k x *) in
      return ()
  in
  return
    (let* () = instr (MIdig 1) in
     instr (MI3 Update))

let unzip_option ~line_no ~context ~args =
  let* () =
    mi_if_none
      (mi_failwith ~default:true ~line_no ~context ~args "unzip_option")
      (return ())
  in
  return (instr (MI1 Some_))

let pps = List.pp_sep ", " (fun ppf -> Format.fprintf ppf "%s")

let ppS = List.pp_sep ", " (fun ppf -> Format.fprintf ppf "%S")

let print_match_product scrutinee p =
  let s = texpr_to_string scrutinee in
  match p with
  | Pattern_single _ -> assert false
  | Pattern_tuple ns ->
      let f = Format.asprintf "%a = sp.match_tuple(%s, %a)" in
      f pps ns s ppS ns
  | Pattern_record (_, bs) ->
      let vs = List.map (fun {var} -> var) bs in
      let fs = List.map (fun {field} -> field) bs in
      let f = Format.asprintf "%a = sp.match_record(%s, %a)" in
      f pps vs s ppS fs

let print_modify_product lhs p =
  let s = texpr_to_string lhs in
  match p with
  | Pattern_single x ->
      let f = Format.asprintf "%s = sp.modify(%s, %S)" in
      f x s x
  | Pattern_tuple ns ->
      let f = Format.asprintf "%a = sp.modify_tuple(%s, %a)" in
      f pps ns s ppS ns
  | Pattern_record (name, _bs) ->
      let f = Format.asprintf "with sp.modify_record(%s, %S) as %s:" in
      let s = texpr_to_string lhs in
      f s name name

let instr_repaired i =
  let* () = instr i in
  let* {erase_var_annots} = get_config in
  when_ erase_var_annots (instr (MI1 (Rename None)))

let compile_prim0 outer =
  let dup = Type.is_hot outer.et <> Yes in
  function
  | ECst l | EBounded l ->
      instr
        (MIpush
           ( Typing.mtype_of_type ~with_annots:false (Type.type_of_literal l)
           , compile_literal ~for_error:outer l ))
  | ECstContract _ -> assert false
  | EMetaLocal _name -> assert false
  | EMatchCons name ->
      let* () = fetch_target ~dup (T_match_cons (name, false)) in
      let* () = fetch_target ~dup (T_match_cons (name, true)) in
      instr (MI2 (Pair (None, None)))
  | EAccount_of_seed _ ->
      errorf
        "Expression %s cannot be converted to Michelson (missing \
         pre-evaluation)"
        (String.escaped (texpr_to_string outer))
  | EContract_address _ | EContract_balance _ | EContract_data _
   |EContract_baker _ | EContract_typed _ ->
      errorf
        "No conversion for expression %s of type %s"
        (String.escaped (texpr_to_string outer))
        (String.escaped (type_to_string outer.et))
  | EConstant (hash, t) ->
      instr
        (MIpush
           (Typing.mtype_of_type ~with_annots:false t, MLiteral.constant hash))
  | EConstantVar name ->
      let* scenario_vars = get_scenario_vars in
      ( match String.Map.find_opt name scenario_vars with
      | None -> assert false
      | Some {v = Literal (String hash)} ->
          instr
            (MIpush
               ( Typing.mtype_of_type ~with_annots:false outer.et
               , MLiteral.constant hash ))
      | Some v ->
          errorf
            "Value %s is not a string."
            (String.escaped (value_to_string v)) )

let rec compile_prim1 outer arg expr =
  let dup = Type.is_hot outer.et <> Yes in
  let on_arg i =
    let* () = compile_expr arg in
    instr i
  in
  match expr with
  | EToInt -> on_arg (MI1 Int)
  | ENeg -> on_arg (MI1 Neg)
  | ESign ->
      let* () = instr (MIpush (mt_int, MLiteral.small_int 0)) in
      let* () = compile_expr arg in
      instr (MI2 Compare)
  | ESum ->
      let proj, sub =
        match arg.e with
        | EPrim1 (EListValues _, inner2) -> (mi_cdr, inner2)
        | _ -> (return (), arg)
      in
      let* () = instr (MIpush (mt_int, MLiteral.small_int 0)) in
      let* () = compile_expr sub in
      mi_iter (mi_seq [proj; instr (MI2 Add)])
  | EProject 0 -> on_arg (MIfield [A])
  | EProject 1 -> on_arg (MIfield [D])
  | EProject _ -> failwith "compiler: TODO EProject"
  | EPack -> on_arg (MI1 Pack)
  | EUnpack t ->
      on_arg (MI1 (Unpack (Typing.mtype_of_type ~with_annots:false t)))
  | EConcat_list ->
    ( match arg.e with
    | EList [a] -> compile_expr a
    | EList [a; b] ->
        let* () = compile_expr b in
        let* () = compile_expr a in
        instr (MI2 Concat2)
    | _ -> on_arg (MI1 Concat1) )
  | ESize -> on_arg (MI1 Size)
  | EAddress ->
    ( match arg.e with
    | EMPrim0 (Self None) ->
        let* tags = get_ok_stack "self_address" in
        if has_target T_self_address tags
        then fetch_target ~dup T_self_address
        else instr (MI0 Self_address)
    | _ -> on_arg (MI1 Address) )
  | EImplicit_account -> on_arg (MI1 Implicit_account)
  | EListRev ->
    ( match Type.getRepr arg.et with
    | T1 (T_list, t) -> mi_seq [compile_expr arg; mi_rev_list t]
    | _ -> errorf "map.items error: %s" (type_to_string arg.et) )
  | EListItems rev ->
    ( match Type.getRepr arg.et with
    | T2 ((T_map | T_big_map), tkey, tvalue) ->
        compile_container_access_list
          arg
          (return ())
          (Type.key_value tkey tvalue)
          rev
    | _ -> errorf "map.items error: %s" (type_to_string arg.et) )
  | EListValues rev ->
    ( match Type.getRepr arg.et with
    | T2 ((T_map | T_big_map), _, tvalue) ->
        compile_container_access_list arg mi_cdr tvalue rev
    | _ -> errorf "map.values error: %s" (type_to_string arg.et) )
  | EListKeys rev ->
    ( match Type.getRepr arg.et with
    | T2 ((T_map | T_big_map), tkey, _) ->
        compile_container_access_list arg mi_car tkey rev
    | _ -> errorf "map.keys error: %s" (type_to_string arg.et) )
  | EListElements rev ->
    ( match Type.getRepr arg.et with
    | T1 (T_set, telement) ->
        compile_container_access_list arg (return ()) telement rev
    | _ -> errorf "set.elements error: %s" (type_to_string arg.et) )
  | EResolve ->
      errorf
        "Expression %s cannot be converted to Michelson (missing \
         pre-evaluation)"
        (String.escaped (texpr_to_string outer))
  | ESetDelegate -> on_arg (MI1 Set_delegate)
  | EType_annotation _ -> compile_expr arg
  | EAttr name ->
    begin
      match (name, arg) with
      | "head", {e = EPrim0 (EMatchCons name)} ->
          fetch_target ~dup (T_match_cons (name, true))
      | "tail", {e = EPrim0 (EMatchCons name)} ->
          fetch_target ~dup (T_match_cons (name, false))
      | attr, {e = EPrim0 (EMetaLocal name)} ->
          fetch_target ~dup (T_var (name ^ "." ^ attr))
      | name, x ->
          let renaming = renaming_of_type ~rev:false x.et in
          let name =
            match Hashtbl.find_opt renaming name with
            | None -> name
            | Some x -> x
          in
          ( match op_of_attr name x.et with
          | None -> errorf "EAttr '%s', %s" name (Type.show x.et)
          | Some [] -> mi_seq [compile_expr x]
          | Some op -> mi_seq [compile_expr x; instr (MIfield op)] )
    end
  | EVariant name ->
      let x = compile_expr arg in
      ( match Type.getRepr outer.et with
      | T1 (T_option, t) -> compile_option t name x
      | TVariant {row = [("Left", tl); ("Right", tr)]} ->
          compile_or tl tr name x
      | TVariant {row; layout} ->
          let layout =
            Option.get ~msg:"compiler: variant" (Unknown.get layout)
          in
          let ctxt = compile_variant ~row ~layout name in
          let l acc t =
            let t =
              Binary_tree.cata (Typing.mtype_of_type ~with_annots:false) mt_or t
            in
            instr (MI1 (Left (None, None, t))) :: acc
          in
          let r t acc =
            let t =
              Binary_tree.cata (Typing.mtype_of_type ~with_annots:false) mt_or t
            in
            instr (MI1 (Right (None, None, t))) :: acc
          in
          let instrs = Binary_tree.context_cata [x] l r ctxt in
          mi_seq (List.rev instrs)
      | t ->
          let t = Type.show_f Type.pp t in
          errorf "EVariant: not a variant: '%s' '%s'" name t )
  | EIsVariant constructor ->
      compile_match
        arg
        [ ( constructor
          , "dummy"
          , mi_seq [instr (MIpush (mt_bool, MLiteral.bool true))] ) ]
        (mi_seq [instr (MIpush (mt_bool, MLiteral.bool false))])
  | EReadTicket ->
      let* () = compile_expr arg in
      let* () = instr (MI1 Read_ticket) in
      instr (MI2 (Pair (None, None)))
  | EJoinTickets -> on_arg (MI1 Join_tickets)
  | EPairingCheck -> on_arg (MI1 Pairing_check)
  | EVotingPower -> on_arg (MI1 Voting_power)
  | EUnbounded -> compile_expr arg
  | EConvert -> compile_expr arg
  | EStaticView _ ->
      errorf
        "Expression %s cannot be converted to Michelson (missing \
         pre-evaluation)"
        (String.escaped (texpr_to_string outer))

and compile_expr e =
  let dup = Type.is_hot e.et <> Yes in
  match e.e with
  | EVar (name, Scenario) ->
      let* scenario_vars = get_scenario_vars in
      ( match String.Map.find_opt name scenario_vars with
      | None -> assert false
      | Some v ->
          let* config = get_config in
          let* tstorage = get_tstorage in
          let t = Typing.mtype_of_type ~with_annots:false (type_of_value v) in
          let v = compile_value_internal ~config ~scenario_vars ?tstorage v in
          instr (MIpush (t, v)) )
  | EVar (name, _) -> fetch_target ~dup (T_var name)
  | EPrivate name -> fetch_target ~dup (T_var name)
  | EMPrim0 p -> instr_repaired (MI0 p)
  | EMPrim1 (p, x) ->
      let* () = compile_expr x in
      instr_repaired (MI1 p)
  | EMPrim1_fail _ -> assert false
  | EMPrim2 (p, x1, x2) ->
      let* () = compile_expr x2 in
      let* () = compile_expr x1 in
      instr (MI2 p)
  | EMPrim3 (p, x1, x2, x3) ->
      let* () = compile_expr x3 in
      let* () = compile_expr x2 in
      let* () = compile_expr x1 in
      instr (MI3 p)
  | EPrim0 prim -> compile_prim0 e prim
  | EPrim1 (prim, x) -> compile_prim1 e x prim
  | EOpenVariant (constructor, inner, missing_message) ->
      let default, args =
        match missing_message with
        | None ->
            let l = head_line_no e.line_no in
            let l = instr (MIpush (mt_int, MLiteral.small_int l)) in
            (true, l)
        | Some missing_message -> (false, compile_expr missing_message)
      in
      compile_match
        ~no_drop:()
        inner
        [(constructor, "dummy", tag_top [st_none ()])]
        (mi_failwith
           ~default
           ~context:(texpr_to_string e)
           ~line_no:e.line_no
           ~args
           "OpenVariant")
  | EMake_signature _ ->
      errorf
        "Expression %s cannot be converted to Michelson (missing \
         pre-evaluation)"
        (String.escaped (texpr_to_string e))
  | EMichelson ({name; parsed; typesIn; typesOut}, exprs) ->
      let* () = iter_list compile_expr (List.rev exprs) in
      let x = Of_micheline.instruction parsed in
      begin
        match x.instr with
        | MIconcat_unresolved | MIerror _ ->
            let typesIn =
              List.map (Typing.mtype_of_type ~with_annots:false) typesIn
            in
            let typesOut =
              List.map (Typing.mtype_of_type ~with_annots:false) typesOut
            in
            instr (MImich {name; parsed; typesIn; typesOut})
        | _ -> instr x.instr
      end
  | EMapFunction {l; f} ->
    ( match f.e with
    | ELambda {name; body} ->
        let* () = compile_expr l in
        let* tags = get_tags in
        let go_fun () =
          match tags with
          | Some (_ :: ts) ->
              let tag = ST_target (T_var name) in
              let* () = set_tags (Some (tag :: ts)) in
              let* () = compile_command body in
              drop_target (T_var name)
          | _ -> errorf "map compilation error"
        in
        mi_map (go_fun ())
    | _ ->
        let* () = compile_expr f (* f *) in
        let* () = compile_expr l (* l f *) in
        let* tags = get_tags in
        ( match tags with
        | Some (_ :: ts) ->
            let go_fun =
              let* () = set_tags (Some (st_none () :: ts)) in
              let* () = mi_copy 1 (* f x f *) in
              instrs [MIdig 1 (* x f f *); MI2 Exec (* y f *)]
            in
            mi_seq [mi_map go_fun; instr (MIdig 1); instr MIdrop]
        | _ -> error "map compilation error" ) )
  | EItem {items; key; default_value; missing_message} ->
      let line_no = e.line_no in
      let on_option =
        let l = head_line_no e.line_no in
        let missed =
          match default_value with
          | None ->
              let default, args =
                match missing_message with
                | None ->
                    let l = instr (MIpush (mt_int, MLiteral.small_int l)) in
                    (true, l)
                | Some missing_message -> (false, compile_expr missing_message)
              in
              mi_failwith
                ~default
                ~line_no
                ~context:(texpr_to_string e)
                ~args
                "GetItem"
          | Some v -> compile_expr v
        in
        let comment = commentf "of_some: Get-item:%d" l in
        mi_if_none ~protect_top:() missed comment
      in
      mi_seq [compile_expr items; compile_expr key; instr (MI2 Get); on_option]
  | EPrim2 (EGetOpt, k, m) ->
      mi_seq [compile_expr m; compile_expr k; instr (MI2 Get)]
  | EPrim2 (EBinOp op, x, y) ->
      compile_binop
        ~line_no:e.line_no
        ~context:(texpr_to_string e)
        (Some x)
        y
        y.et
        op
  | ERecord entries ->
      let* config = get_config in
      let has_pairn =
        match config.protocol with
        | Delphi -> false
        | Edo | Florence | Granada | Hangzhou | Ithaca -> config.pairn
      in
      let layout =
        match Type.getRepr e.et with
        | TRecord {layout} ->
            Option.get ~msg:"compiler: record" (Unknown.get layout)
        | _ -> assert false
      in
      let entries = List.map (map_snd compile_expr) entries in
      let entries = compile_record ~layout entries in
      let n = Binary_tree.size entries in
      if has_pairn && n > 2 && Binary_tree.is_right_comb entries
      then
        let* () = iter_list snd (List.rev (Binary_tree.to_list entries)) in
        instr (MIpairn n)
      else
        let p x1 x2 = mi_seq [x2; x1; instr (MI2 (Pair (None, None)))] in
        Binary_tree.cata snd p entries
  | ETuple es ->
      let f x y = mi_seq [y; x; instr (MI2 (Pair (None, None)))] in
      compile_tuple f (List.map compile_expr es)
  | EPrim2 (ECons, x, l) ->
      let x = compile_expr x in
      let l = compile_expr l in
      mi_seq [l; x; instr (MI2 Cons)]
  | EPrim2 (EAdd_seconds, t, s) ->
      mi_seq [compile_expr s; compile_expr t; instr (MI2 Add)]
  | EPrim2 (EView (name, return_type), address, param) ->
      let* () = compile_expr address in
      let* () = compile_expr param in
      instr
        (MI2 (View (name, Typing.mtype_of_type ~with_annots:true return_type)))
  | ESlice {offset; length; buffer} ->
      let* () = compile_expr buffer in
      let* () = compile_expr length in
      let* () = compile_expr offset in
      instr (MI3 Slice)
  | EPrim2 (EContains, member, items) ->
    ( match Type.getRepr items.et with
    | T2 ((T_map | T_big_map), _, _) ->
        mi_seq [compile_expr items; compile_expr member; instr (MI2 Mem)]
    | T1 (T_set, _) ->
        mi_seq [compile_expr items; compile_expr member; instr (MI2 Mem)]
    | _ -> error "EContains" )
  | EPrim3 (ESplit_tokens, e, quantity, {e = EPrim0 (ECst (Int {i}))})
    when Bigint.equal i (Bigint.of_int 1) ->
      mi_seq [compile_expr quantity; compile_expr e; instr (MI2 Mul)]
  | EPrim3 (ESplit_tokens, amount, quantity, total) ->
      let* () = compile_expr total in
      let* () = compile_expr quantity in
      let* () = compile_expr amount in
      let* () = instr (MI2 Mul) in
      let* () = instr (MI2 Ediv) in
      let* () = of_Some ~line_no:e.line_no ~context:(texpr_to_string e) in
      mi_car
  | EList elems ->
    ( match Typing.mtype_of_type ~with_annots:false e.et with
    | {mt = MT1 (T_list, t)} ->
        let* () = instr (MIpush (mt_list t, MLiteral.list [])) in
        mi_seq
          (List.rev_map
             (fun value -> mi_seq [compile_expr value; instr (MI2 Cons)])
             elems)
    | _ -> failwith "elems" )
  | EMap (_, entries) ->
      let* () =
        match Typing.mtype_of_type ~with_annots:false e.et with
        | {mt = MT2 (T_big_map, k, v)} -> instr (MI0 (Empty_bigmap (k, v)))
        | mt -> instr (MIpush (mt, MLiteral.mk_map []))
      in
      mi_seq
        (List.map
           (fun (key, value) ->
             let* () = compile_expr value in
             let* () = instr (MI1 Some_) in
             let* () = compile_expr key in
             instr (MI3 Update))
           entries)
  | ESet entries ->
      let* () =
        instr
          (MIpush (Typing.mtype_of_type ~with_annots:false e.et, MLiteral.set []))
      in
      mi_seq
        (List.map
           (fun key ->
             let* () = instr (MIpush (mt_bool, MLiteral.bool true)) in
             let* () = compile_expr key in
             instr (MI3 Update))
           entries)
  | EContract {entry_point; arg_type; address} ->
      let* () = compile_expr address in
      instr
        (MI1
           (Contract
              (entry_point, Typing.mtype_of_type ~with_annots:true arg_type)))
  | EPrim3 (ERange, a, b, step) ->
      let range_result_target = T_var "__range_result" in
      let body =
        if_stack_ok (fun tags ->
            match find_target range_result_target tags with
            | None -> errorf "find_target %s" (show_target range_result_target)
            | Some offset ->
                instrs
                  [MIdup 1; MIdig (offset + 1); MIdig 1; MI2 Cons; MIdug offset])
      in
      let* () = comment (texpr_to_string e) in
      let* () =
        instr (MI0 (Nil (Typing.mtype_of_type ~with_annots:false a.et)))
      in
      let* () = tag_top [ST_target range_result_target] in
      let* () =
        let line_no = e.line_no in
        let name = "RANGE" in
        let has_operations = false in
        compile_range ~line_no ~a ~b ~step ~name ~body ~has_operations
      in
      let* () = mi_rev_list a.et in
      tag_top [st_none ()]
  | EPrim2 (ECallLambda, parameter, lambda) ->
    ( match Type.unF lambda.et with
    | TLambda ({with_storage; with_operations}, _, _) ->
        let with_storage = Unknown.get_ok "with_storage" with_storage in
        let with_operations =
          Unknown.get_ok "with_operations" with_operations
        in
        if Option.is_some with_storage || with_operations
        then
          let* tags = get_tags in
          let* () = compile_expr lambda in
          let* () = compile_expr parameter in
          let f = function
            | x :: f :: stack ->
                let open Michel.Expr in
                let input =
                  tuple
                    ( [var x]
                    @ (if with_operations then [var "__operations__"] else [])
                    @
                    if Option.is_some with_storage
                    then [var "__storage__"]
                    else [] )
                in
                let output =
                  [Some "y"]
                  @ (if with_operations then [Some "o"] else [])
                  @ if Option.is_some with_storage then [Some "s"] else []
                in
                let stack =
                  replace
                    ( (if with_operations then [("__operations__", "o")] else [])
                    @
                    if Option.is_some with_storage
                    then [("__storage__", "s")]
                    else [] )
                    stack
                in
                lets
                  [ (P_var (Some "output"), prim2 Exec input (var f))
                  ; (P_vector output, unpair (List.length output) (var "output"))
                  ]
                  (vector (List.map var ("y" :: stack)))
            | _ -> assert false
          in
          let* () = compile_michel f in
          set_tags (Option.map (fun xs -> st_none () :: xs) tags)
        else
          mi_seq [compile_expr lambda; compile_expr parameter; instr (MI2 Exec)]
    | _ -> assert false )
  | EPrim2 (EApplyLambda, parameter, lambda) ->
      mi_seq [compile_expr lambda; compile_expr parameter; instr (MI2 Apply)]
  | ELambda {name; body; with_storage; with_operations; tParams; tResult} ->
      let* tstorage = get_tstorage in
      let tParams, tResult =
        let with_storage =
          if Option.is_some with_storage
          then (
            assert (Option.is_some tstorage);
            tstorage )
          else None
        in
        Type.rawify_lambda
          ~with_storage
          ~with_operations
          (get_extra tParams)
          (get_extra tResult)
      in
      let body =
        let* () =
          match (Option.is_some with_storage, with_operations) with
          | false, false -> tag_top [ST_target (T_var name)]
          | false, true ->
              let* () = instr (MIunpair [true; true]) in
              let* () =
                tag_top [ST_target (T_var name); ST_target tgt_operations]
              in
              instr (MIdig 1)
          | true, false ->
              let* () = instr (MIunpair [true; true]) in
              tag_top [ST_target (T_var name); ST_target tgt_storage]
          | true, true ->
              let* () = instr (MIunpair [true; true; true]) in
              let* () =
                tag_top
                  [ ST_target (T_var name)
                  ; ST_target tgt_operations
                  ; ST_target tgt_storage ]
              in
              instr (MIdig 1)
        in
        let* () = compile_command body in
        let* () = drop_target (T_var name) in
        match (Option.is_some with_storage, with_operations) with
        | false, false -> return ()
        | true, false -> instr (MI2 (Pair (None, None)))
        | false, true -> instr (MI2 (Pair (None, None)))
        | true, true -> instr (MIpairn 3)
      in
      composite
        (MIlambda
           ( Typing.mtype_of_type ~with_annots:false tParams
           , Typing.mtype_of_type ~with_annots:false tResult
           , body ))
  | ECreate_contract
      { contract_template = {entry_points; private_variables; derived; views}
      ; baker
      ; balance
      ; storage } ->
      let ({tparameter_lazy; tparameter_non_lazy; config; tstorage}
            : _ contract_derived) =
        get_extra derived
      in
      let* scenario_vars = get_scenario_vars in
      let storage = storage in
      let {tparameter; tstorage; code} =
        compile_contract
          ~config
          ~scenario_vars
          ~storage:None
          ~entry_points
          ~tstorage
          ~tparameter_lazy
          ~tparameter_non_lazy
          ~private_variables
          ~views
      in
      let* () = compile_expr storage in
      let* () = compile_expr balance in
      let* () = compile_expr baker in
      let* () =
        instr
          (MIcreate_contract
             {tparameter; tstorage; code = erase_types_instr code})
      in
      instr (MI2 (Pair (None, None)))
  | EPrim3 (EUpdate_map, k, v, m) ->
      let* () = compile_expr m in
      let* () = compile_expr v in
      let* () = compile_expr k in
      instr (MI3 Update)
  | EPrim3 (EGet_and_update, k, v, m) ->
      let* () = compile_expr m in
      let* () = compile_expr v in
      let* () = compile_expr k in
      instrs [MI3 Get_and_update; MI2 (Pair (None, None))]
  | ETransfer {arg; amount; destination} ->
      let* () = compile_expr destination in
      let* () = compile_expr amount in
      let* () = compile_expr arg in
      instr (MI3 Transfer_tokens)
  | EMatch _ -> error "ematch"
  | EPrim3 (ETest_ticket, _, _, _) -> failwith "cannot compile test ticket"
  | ESaplingVerifyUpdate {state; transaction} ->
      let* () = compile_expr state in
      let* () = compile_expr transaction in
      instr (MI2 Sapling_verify_update)
  | EPrim2 (ETicket, content, amount) ->
      mi_seq [compile_expr amount; compile_expr content; instr (MI2 Ticket)]
  | EPrim2 (ESplitTicket, ticket, decomposition) ->
      mi_seq
        [ compile_expr decomposition
        ; compile_expr ticket
        ; instr (MI2 Split_ticket) ]
  | EHasEntryPoint name ->
      let* () = fetch_target ~dup:true T_entry_points in
      let* id = get_lazy_id name in
      let* () = instr (MIpush (mt_nat, id)) in
      instr (MI2 Mem)
  | EIf (c, x, y) ->
      mi_seq
        [compile_expr c; mi_if ~protect_top:() (compile_expr x) (compile_expr y)]
  | EIsFailing _ | ECatchException _ ->
      raise
        (SmartExcept
           [ `Text "sp.is_failing and sp.catch_exception cannot be compiled"
           ; `Expr e
           ; `Line e.line_no ])

(* If x is missing, compile top += y *)
and compile_binop ~line_no ~context x y t =
  let t = Typing.mtype_of_type ~with_annots:false t in
  let strict i =
    let* () = compile_expr y in
    let* () = Option.default (instr (MIdig 1)) (Option.map compile_expr x) in
    i
  in
  let min_max is_min =
    let final_min_max is_min =
      let* () = instrs [MIdup 1; MIdup 3; MI2 Compare; MI1 Le] in
      let x = instr MIdrop in
      let y = mi_seq [instr (MIdig 1); instr MIdrop] in
      if is_min then mi_if ~protect_top:() x y else mi_if ~protect_top:() y x
    in
    strict (final_min_max is_min)
  in
  function
  | BNeq -> strict (instrs [MI2 Compare; MI1 Neq])
  | BEq -> strict (instrs [MI2 Compare; MI1 Eq])
  | BLt -> strict (instrs [MI2 Compare; MI1 Lt])
  | BLe -> strict (instrs [MI2 Compare; MI1 Le])
  | BGt -> strict (instrs [MI2 Compare; MI1 Gt])
  | BGe -> strict (instrs [MI2 Compare; MI1 Ge])
  | BOr when equal_mtype t mt_bool ->
      let* () = Option.default (return ()) (Option.map compile_expr x) in
      mi_if
        ~protect_top:()
        (instr (MIpush (mt_bool, MLiteral.bool true)))
        (compile_expr y)
  | BAnd when equal_mtype t mt_bool ->
      let* () = Option.default (return ()) (Option.map compile_expr x) in
      mi_if
        ~protect_top:()
        (compile_expr y)
        (instr
           (MIpush
              ( Typing.mtype_of_type ~with_annots:false Type.bool
              , MLiteral.bool false )))
  | BAnd -> strict (instr (MI2 And))
  | BOr -> strict (instr (MI2 Or))
  | BAdd when equal_mtype t mt_string -> strict (instr (MI2 Concat2))
  | BAdd when equal_mtype t mt_bytes -> strict (instr (MI2 Concat2))
  | BAdd -> strict (instr (MI2 Add))
  | BSub -> strict (instr (MI2 Sub))
  | BEDiv -> strict (instr (MI2 Ediv))
  | BDiv ->
      strict
        (let* () = instr (MI2 Ediv) in
         mi_if_none
           (mi_failwith ~default:true ~line_no ~context "DivisionByZero")
           mi_car)
  | BMul _ -> strict (instr (MI2 Mul))
  | BMod ->
      strict
        (let* () = instr (MI2 Ediv) in
         mi_if_none
           (mi_failwith ~default:true ~line_no ~context "DivisionByZero")
           mi_cdr)
  | BXor -> strict (instr (MI2 Xor))
  | BMin -> min_max true
  | BMax -> min_max false

and compile_container_access_list container projection t rev =
  let* () = instr (MI0 (Nil (Typing.mtype_of_type ~with_annots:false t))) in
  let* () = compile_expr container in
  let* () = mi_iter (mi_seq [projection; instr (MI2 Cons)]) in
  if rev then return () else mi_rev_list t

and compile_match ?no_drop scrutinee cases (body_else : unit t) =
  let body_tagged arg_name body =
    let tag = T_var arg_name in
    let* () = tag_top [ST_target tag] in
    let* () = body in
    if no_drop = Some () then return () else drop_target tag
  in
  let drop_else = mi_seq [instr MIdrop; body_else] in
  let tree =
    match cases with
    | [("None", _arg_name, body)] -> mi_if_none ~protect_top:() body drop_else
    | [("Some", arg_name, body)] ->
        mi_if_none ~protect_top:() body_else (body_tagged arg_name body)
    | [("Some", arg_name, body1); ("None", _arg_name, body2)]
     |[("None", _arg_name, body2); ("Some", arg_name, body1)] ->
        mi_if_none ~protect_top:() body2 (body_tagged arg_name body1)
    | [("Left", arg_name, body)] ->
        mi_if_left ~protect_top:() (body_tagged arg_name body) drop_else
    | [("Right", arg_name, body)] ->
        mi_if_left ~protect_top:() drop_else (body_tagged arg_name body)
    | _ ->
        let compile_match =
          let leaf Layout.{source} =
            match
              List.find_opt
                (fun (constructor, _, _) -> constructor = source)
                cases
            with
            | None -> drop_else
            | Some (_constructor, arg_name, body) -> body_tagged arg_name body
          in
          Binary_tree.cata leaf (mi_if_left ~protect_top:())
        in
        let layout =
          match Type.getRepr scrutinee.et with
          | TVariant {layout} -> Unknown.get layout
          | _ -> None
        in
        let tree =
          match layout with
          | None -> error "Not a variant type"
          | Some layout -> compile_match layout
        in
        tree
  in
  mi_seq [compile_expr scrutinee; tree]

and compile_match_cons expr id ok_match ko_match =
  let* () = compile_expr expr in
  mi_if_cons
    ~protect_top:()
    (let* () =
       tag_top
         [ ST_target (T_match_cons (id, true))
         ; ST_target (T_match_cons (id, false)) ]
     in
     let* () = ok_match in
     let* () = drop_target (T_match_cons (id, true)) in
     drop_target (T_match_cons (id, false)))
    ko_match

and of_Some ~line_no ~context =
  let comment = comment context in
  mi_if_none (mi_failwith ~default:true ~line_no ~context "OfSome") comment

(** Return an instruction that makes the value of [e] available. The
   instruction is either [dup_tag] or [PUSH], depending on whether [e]
   is a constant or not. Currently only implemented for int and
   nat. *)
and get_target_or_cst e tag =
  let put_tag = tag_top [ST_target tag] in
  match (e.e, Typing.mtype_of_type ~with_annots:false e.et) with
  | EPrim0 (ECst (Int {i})), {mt = MT0 T_int} ->
      return (mi_seq [instr (MIpush (mt_int, MLiteral.int i)); put_tag])
  | EPrim0 (ECst (Int {i})), {mt = MT0 T_nat} ->
      return (mi_seq [instr (MIpush (mt_nat, MLiteral.int i)); put_tag])
  | _ ->
      let* () = compile_expr e in
      let* () = put_tag in
      return (fetch_target ~dup:true tag)

and compile_range ~line_no ~a ~b ~step ~name ~body ~has_operations =
  let i = T_var name in
  let* () = ensure_ops_if has_operations in
  let* get_b = get_target_or_cst b (T_var (name ^ "#b")) in
  let* get_step = get_target_or_cst step (T_var (name ^ "#step")) in
  let cond cmp =
    mi_seq [fetch_target ~dup:true i; get_b; instr (MI2 Compare); cmp]
  in
  let loop cmp =
    let* () = cond cmp in
    mi_loop
      (let* () = body in
       let* () = comment "loop step" in
       let* () = get_step in
       let* () = instr (MI2 Add) in
       let* () = tag_top [ST_target i] in
       cond cmp)
  in
  let* () = compile_expr a in
  let* () = tag_top [ST_target i] (* i := a *) in
  let* () =
    let err =
      mi_failwith ~default:true ~line_no ~context:"range" "ZeroRangeStep"
    in
    match Typing.mtype_of_type ~with_annots:false step.et with
    | {mt = MT0 T_int} ->
        let* () = get_step in
        let* () = instr (MI1 Gt) in
        mi_if (* step > 0 *)
          (loop (instr (MI1 Gt)))
          (mi_seq
             [ get_step
             ; instr (MI1 Lt)
             ; mi_if (* step < 0 *) (loop (instr (MI1 Lt))) err ])
    | {mt = MT0 T_nat} ->
        let* () = get_step in
        let* () = instr (MIpush (mt_nat, MLiteral.small_int 0)) in
        let* () = instr (MI2 Compare) in
        let* () = instr (MI1 Eq) in
        mi_if err (loop (instr (MI1 Gt)))
    | _ -> assert false
  in
  let* () = drop_target i in
  let* () = drop_target (T_var (name ^ "#b")) in
  let* () = drop_target (T_var (name ^ "#step")) in
  return ()

and unzip_lstep ~line_no ~context = function
  | LMapItem (key, tvalue) ->
      let* zip_container = unzip_map (compile_expr key) tvalue in
      let* zip_option =
        unzip_option ~line_no ~context ~args:(instr (MI2 (Pair (None, None))))
      in
      return
        (let* () = zip_option in
         zip_container)
  | LAttr (name, t) ->
      let* zip_attr = unzip_attr name t in
      return zip_attr

and unzip_path ~line_no ~context = function
  | [] -> return (return ())
  | s :: rest ->
      let* zip_lstep = unzip_lstep ~line_no ~context s in
      let* zip_rest = unzip_path ~line_no ~context rest in
      return
        (let* () = zip_rest in
         zip_lstep)

and unzip_lexpr ~line_no ~context ~copy lexpr =
  let target, lsteps = target_of_lexpr lexpr in
  let* found, zip_target = unzip_target ~copy target in
  match (lsteps, found) with
  | [], false -> return (false, zip_target)
  | _, true ->
      let* zip_path = unzip_path ~line_no ~context lsteps in
      return
        ( true
        , let* () = zip_path in
          zip_target )
  | _ ->
      raise
        (SmartExcept
           [ `Text "Missing target (unzip_lexpr)"
           ; `Text context
           ; `Br
           ; `Text "sp.modify_record or similar instructions may be required"
           ; `Line line_no ])

and strict_unzip_lexpr ~line_no ~context ~copy lexpr =
  let* found, zip_target = unzip_lexpr ~line_no ~context ~copy lexpr in
  if not found
  then
    raise
      (SmartExcept
         [ `Text "Missing target (strict_unzip_lexpr)"
         ; `Text context
         ; `Br
         ; `Text "sp.modify_record or similar instructions may be required"
         ; `Line line_no ]);
  return zip_target

and compile_delete ~line_no ~context lexpr =
  match last_step_is_item lexpr with
  | None -> assert false
  | Some (lexpr', key, tvalue) ->
      let copy = self_referential_lexpr lexpr in
      let* zip_lexpr = strict_unzip_lexpr ~line_no ~context ~copy lexpr' in
      let* () =
        instr
          (MIpush
             ( mt_option (Typing.mtype_of_type ~with_annots:false tvalue)
             , MLiteral.none ))
      in
      let* () = compile_expr key in
      let* () = instr (MI3 Update) in
      zip_lexpr

and compile_assign ~line_no ~context ~copy lexpr rhs =
  match last_step_is_item lexpr with
  | None ->
      let* found, zip_lexpr = unzip_lexpr ~line_no ~context ~copy lexpr in
      let* () = when_ found (instr MIdrop) in
      let* () = rhs in
      let* () = zip_lexpr in
      return ()
  | Some (lexpr', key, _) ->
      (* Do not look up the key. *)
      let* zip_lexpr = strict_unzip_lexpr ~line_no ~context ~copy lexpr' in
      let* () = rhs in
      let* () = instr (MI1 Some_) in
      let* () = compile_expr key in
      let* () = instr (MI3 Update) in
      zip_lexpr

and compile_update ~line_no ~context ~copy lexpr rhs =
  let* zip_lexpr = strict_unzip_lexpr ~line_no ~context ~copy lexpr in
  let* () = rhs in
  zip_lexpr

and compile_command x =
  let ops = ensure_ops_if (has_operations x <> HO_none) in
  match x.c with
  | CNever inner -> mi_seq [compile_expr inner; instr (MI1_fail Never)]
  | CDefineLocal {rhs} ->
      let* () = comment (tcommand_to_string x) in
      let* () = ops in
      let* () = compile_expr rhs in
      let* () = instr MIdrop in
      push_unit
  | CBind (x, ({c = CDefineLocal {var; rhs}} as def_local), c) ->
      assert (x = None);
      let* () = comment (tcommand_to_string def_local) in
      let* () = ensure_ops_if (has_operations def_local <> HO_none) in
      let* () = compile_expr rhs in
      let* () = tag_top [ST_target (T_var var)] in
      let* () = compile_command c in
      drop_target (T_var var)
  | CBind (_, c, ({c = CDefineLocal {rhs}} as def_local)) ->
      let* () = compile_command c in
      let* () = instr MIdrop in
      let* () = comment (tcommand_to_string def_local) in
      let* () = ensure_ops_if (has_operations def_local <> HO_none) in
      let* () = compile_expr rhs in
      let* () = instr MIdrop in
      push_unit
  | CBind (None, c1, c2) ->
      let* () = compile_command c1 in
      let* () = instr MIdrop in
      compile_command c2
  | CBind (x, c1, c2) ->
      let* () = compile_command c1 in
      ( match x with
      | None ->
          let* () = instr MIdrop in
          compile_command c2
      | Some x ->
          let* () = tag_top [ST_target (T_var x)] in
          let* () = compile_command c2 in
          drop_target (T_var x) )
  | CResult {e = EPrim0 (ECst Literal.Unit)} -> push_unit
  | CResult e ->
      let* () = commentf "sp.result(%s)" (texpr_to_string e) in
      let* () = compile_expr e in
      tag_top [st_none ()]
  | CIf (c, t, e) ->
      let* () = commentf "if %s:" (texpr_to_string c) in
      let* () = compile_expr c in
      mi_if ~protect_top:() (compile_command t) (compile_command e)
  | CMatch (scrutinee, cases) ->
      let cases =
        List.map
          (fun (constructor, name, body) ->
            (constructor, name, compile_command body))
          cases
      in
      let* () =
        commentf "with %s.match_cases(...):" (texpr_to_string scrutinee)
      in
      compile_match scrutinee cases push_unit
  | CMatchCons {expr; id; ok_match; ko_match} ->
      let* () =
        commentf "with sp.match_cons(%s) as %s:" (texpr_to_string expr) id
      in
      compile_match_cons
        expr
        id
        (compile_command ok_match)
        (compile_command ko_match)
  | CMatchProduct (scrutinee, p, c) ->
      let* () = comment (print_match_product scrutinee p) in
      let* () = compile_expr scrutinee in
      let* drop_vars = explode_pattern scrutinee.et p in
      let* () = compile_command c in
      drop_vars
  | CModifyProduct (lhs, Pattern_record (name, p), body) ->
      let* () = comment (print_modify_product lhs (Pattern_record (name, p))) in
      ( match lexpr_of_expr lhs with
      | None -> error "invalid lexpr: CModifyProduct"
      | Some lexpr ->
          let p =
            match Type.getRepr lhs.et with
            | TRecord {row} -> List.map (fun (var, _) -> {var; field = var}) row
            | _ -> assert false
          in
          let* () = ops in
          let line_no = x.line_no in
          let context = tcommand_to_string x in
          let copy = occurs_lroot_command lexpr.lroot body in
          let* zip_lexpr = strict_unzip_lexpr ~line_no ~context ~copy lexpr in
          let* close_record = open_record_pattern name lhs.et p in
          let* () = compile_command body in
          let* () = instr MIdrop in
          let* () = close_record in
          let* () = zip_lexpr in
          push_unit )
  | CModifyProduct (lhs, p, c) ->
      let* () = comment (print_modify_product lhs p) in
      ( match lexpr_of_expr lhs with
      | None -> error "invalid lexpr: CModifyProduct"
      | Some lexpr ->
          let* () = ops in
          let line_no = x.line_no in
          let context = tcommand_to_string x in
          let copy = occurs_lroot_command lexpr.lroot c in
          let* zip_lexpr = strict_unzip_lexpr ~line_no ~context ~copy lexpr in
          let* drop_vars = explode_pattern lhs.et p in
          let* () = compile_command c in
          let* () = drop_vars in
          let* () = zip_lexpr in
          push_unit )
  | CFailwith message ->
      mi_failwith
        ~default:false
        ~line_no:x.line_no
        ~context:(tcommand_to_string x)
        ~args:(compile_expr message)
        "failwith"
  | CVerify (e, message) ->
      let error =
        let default, message =
          match message with
          | None ->
              ( true
              , instr
                  (MIpush
                     ( mt_string
                     , MLiteral.string (Printer.wrong_condition_string e) )) )
          | Some message -> (false, compile_expr message)
        in
        mi_failwith
          ~default
          ~verify:()
          ~line_no:x.line_no
          ~context:(texpr_to_string e)
          ~args:message
          "verify"
      in
      let* () = comment (tcommand_to_string x) in
      let* () = ops in
      let* () = compile_expr e in
      let* () = mi_if (return ()) error in
      push_unit
  | CSetVar (lhs, e) ->
      let* () = comment (tcommand_to_string x) in
      let* () = ops in
      let* () =
        match lexpr_of_expr lhs with
        | None -> errorf "Invalid lexpr (CSetVar): %s" (texpr_to_string lhs)
        | Some lexpr_lhs ->
            let* () = ensure_ops_if (lexpr_lhs.lroot = LOperations) in
            let line_no = x.line_no in
            let context = tcommand_to_string x in
            ( match e with
            | {e = EPrim2 (EBinOp op, e1, e2)} when equal_texpr lhs e1 ->
                let copy =
                  occurs_lroot_expr lexpr_lhs.lroot e2
                  || self_referential_lexpr lexpr_lhs
                in
                let e = compile_binop ~line_no ~context None e2 e2.et op in
                compile_update ~line_no ~context ~copy lexpr_lhs e
            | _ ->
                let copy =
                  occurs_lroot_expr lexpr_lhs.lroot e
                  || self_referential_lexpr lexpr_lhs
                in
                compile_assign
                  ~line_no
                  ~context
                  ~copy
                  lexpr_lhs
                  (compile_expr e) )
      in
      push_unit
  | CUpdateSet (lhs, key, add) ->
      let* () = comment (tcommand_to_string x) in
      ( match lexpr_of_expr lhs with
      | None -> errorf "Invalid lexpr (CSetVar): %s" (texpr_to_string lhs)
      | Some lhs ->
          let copy =
            occurs_lroot_expr lhs.lroot key || self_referential_lexpr lhs
          in
          let* zip_lexpr =
            strict_unzip_lexpr
              ~line_no:x.line_no
              ~context:(tcommand_to_string x)
              ~copy
              lhs
          in
          let* () = instr (MIpush (mt_bool, MLiteral.bool add)) in
          let* () = compile_expr key in
          let* () = instr (MI3 Update) in
          let* () = zip_lexpr in
          push_unit )
  | CFor (name, ({e = EPrim3 (ERange, a, b, step)} as range), body)
    when List.mem (Typing.mtype_of_type ~with_annots:false a.et) [mt_nat; mt_int]
    ->
      let* () =
        commentf
          "for %s in %s: ... (%s)"
          name
          (texpr_to_string range)
          (type_to_string step.et)
      in
      let line_no = x.line_no in
      let has_operations = has_operations body <> HO_none in
      let body = compile_command body >> instr MIdrop in
      let* () =
        compile_range ~line_no ~a ~b ~step ~name ~body ~has_operations
      in
      push_unit
  | CFor (name, container, body) ->
      let projection, map, onMap =
        match container.e with
        | EPrim1 (EListItems false, map) ->
          ( match Type.getRepr map.et with
          | T2 ((T_map | T_big_map), _, _) -> (return (), map, mi_cdr)
          | _ ->
              ( errorf "map.items error: %s" (type_to_string map.et)
              , map
              , return () ) )
        | EPrim1 (EListValues false, map) -> (mi_cdr, map, return ())
        | EPrim1 (EListKeys false, map) -> (mi_car, map, return ())
        | EPrim1 (EListElements false, set) -> (return (), set, return ())
        | _ -> (return (), container, return ())
      in
      let lcontainer = lexpr_of_expr map in
      let iterName = T_var name in
      let writes = modifies_var name body in
      let* () = commentf "for %s in %s: ..." name (texpr_to_string container) in
      let* () = ops in
      let* () = compile_expr map in
      let* () =
        (if writes then mi_map else mi_iter)
          (let* () = projection in
           let* () = tag_top [ST_target iterName] in
           let* () = compile_command body in
           let* () = instr MIdrop in
           let* () = if writes then onMap else return () in
           if writes then return () else drop_target iterName)
      in
      let* () =
        match lcontainer with
        | None when writes ->
            errorf "Invalid lexpr (CFor): %s" (texpr_to_string map)
        | Some lhs when writes ->
            let t = T_var (name ^ "#iteratee") in
            let* () = tag_top [ST_target t] in
            let* () =
              compile_assign
                ~line_no:x.line_no
                ~context:"for"
                ~copy:true
                lhs
                (fetch_target ~dup:true t)
            in
            drop_target t
        | _ -> return ()
      in
      push_unit
  | CDelItem (map, key) ->
    ( match (lexpr_of_expr map, map.et) with
    | Some lexpr, F (T2 ((T_map | T_big_map), _, tvalue)) ->
        let lexpr = extend_lexpr lexpr (LMapItem (key, tvalue)) in
        let context = tcommand_to_string x in
        let* () = comment (tcommand_to_string x) in
        let* () = compile_delete ~line_no:x.line_no ~context lexpr in
        push_unit
    | _ -> errorf "invalid lexpr" )
  | CWhile (e, c) ->
      let* () = commentf "while %s : ..." (texpr_to_string e) in
      let* () = ops in
      let* () = compile_expr e in
      let* () =
        mi_loop
          (let* () = compile_command c in
           let* () = instr MIdrop in
           let* () = commentf "check for next loop: %s" (texpr_to_string e) in
           compile_expr e)
      in
      push_unit
  | CComment s -> comment s
  | CSetResultType (c, _) -> compile_command c
  | CSetType _ | CTrace _ -> push_unit
  | CSetEntryPoint (n, e) ->
      let* found, zip = unzip_target ~copy:false T_entry_points in
      assert found;
      let* () = compile_expr e in
      let* () = instr (MI1 Some_) in
      let* id = get_lazy_id n in
      let* () = instr (MIpush (mt_nat, id)) in
      let* () = instr (MI3 Update) in
      zip

and compile_entry_point (ep : _ entry_point) =
  let* () = commentf "== %s ==" ep.channel in
  let* () = tag_top [ST_target tgt_parameter; ST_target tgt_storage] in
  let* () = compile_command ep.body in
  let* () = if_stack_ok (fun _ -> instr MIdrop) in
  drop_target tgt_parameter

(** Tidies up the stack at the end of the contract. *)
and pre_finalize_contract has_operations =
  let* tags = get_tags in
  match tags with
  | None -> return ()
  | Some _ ->
    ( match has_operations with
    | Has_operations.HO_none ->
        mi_seq [ops_init; instr (MI2 (Pair (None, None)))]
    | HO_at_most_one -> instr (MI2 (Pair (None, None)))
    | HO_many ->
        mi_seq [mi_rev_list Type.operation; instr (MI2 (Pair (None, None)))] )

and assert_singleton_stack =
  let* tags = get_tags in
  match tags with
  | Some [_] -> return ()
  | None -> return ()
  | _ -> error "assert_singleton_stack"

and lazify_body (id, (path, ep)) =
  let rec proj = function
    | [] -> return ()
    | A :: rest ->
        mi_if_left
          (proj rest)
          (mi_failwith ~default:true ~line_no:[] ~context:"" "")
    | D :: rest ->
        mi_if_left
          (mi_failwith ~default:true ~line_no:[] ~context:"" "")
          (proj rest)
  in
  let* instrs, _ =
    block
      {tags = Some [st_none ()]}
      (let* () = instr (mi_unpair 2) in
       let* () = tag_top [ST_target tgt_parameter; ST_target tgt_storage] in
       let* () = proj path in
       let* () = compile_entry_point ep in
       let* () =
         get_tags
         >>= function
         | Some tags when not (has_target tgt_operations tags) -> ops_init
         | _ -> return ()
       in
       instr (MI2 (Pair (None, None))))
  in
  let body = {instr = MIseq instrs} in
  let entry =
    ( ep.channel
    , Option.default false ep.lazy_no_code
    , MLiteral.small_int id
    , MLiteral.instr body )
  in
  return (ep, id, [entry])

and lazify_call =
  let t = st_none () in
  fun id ->
    let* () = instrs [MIdrop; MIpush (mt_nat, MLiteral.small_int id)] in
    tag_top
      [ t
      ; ST_target tgt_operations
      ; ST_target tgt_storage
      ; ST_target T_entry_points ]

and compile_entry_points eps t =
  let layout =
    match Type.getRepr t with
    | Type.(TVariant {layout}) -> Option.get (Unknown.get layout)
    | _ -> assert false
  in
  let layout =
    Binary_tree.map
      (fun Layout.{source} ->
        List.find (fun ({channel} : _ entry_point) -> channel = source) eps)
      layout
  in
  Binary_tree.cata
    (fun ep -> compile_entry_point ep)
    (fun l1 l2 -> void (mi_if_left l1 l2))
    layout

and compile_lazy_entry_points eps t =
  let layout =
    match t with
    | Type.(F (TVariant {layout})) -> Option.get (Unknown.get layout)
    | _ -> assert false
  in
  let layout =
    Binary_tree.map
      (fun Layout.{source} ->
        List.find (fun ({channel} : _ entry_point) -> channel = source) eps)
      layout
  in
  let layout =
    Binary_tree.(
      cata
        (fun ep path -> Leaf (path, ep))
        (fun l1 l2 path -> Node (l1 (path @ [A]), l2 (path @ [D])))
        layout
        [])
  in
  let layout =
    snd (Binary_tree.map_accum (fun i x -> (i + 1, (i, x))) 0 layout)
  in
  let* layout = T.map lazify_body layout in
  let code =
    let* () = instr (MIdup 1) in
    let* () =
      Binary_tree.cata
        (fun (_, id, _) -> lazify_call id)
        (mi_if_left ~protect_top:())
        layout
    in
    let* () = instrs [MIdig 3; MIdup 1; MIdug 4; MIdig 1; MI2 Get] in
    let* () = of_Some ~line_no:[] ~context:"missing entry point" in
    let* () = instrs [MIdig 2; MIdig 2; MI2 (Pair (None, None))] in
    let* () = instrs [MI2 Exec; mi_unpair 2] in
    tag_top
      [ST_target tgt_operations; ST_target tgt_storage; ST_target T_entry_points]
  in
  let l = Binary_tree.cata (fun (_, _, l) -> l) (fun l1 l2 -> l1 @ l2) layout in
  let ep_map =
    let f (_, no_code, i, ep) = if no_code then None else Some (i, ep) in
    List.filter_map f l
  in
  let lazy_ids = List.map (fun (c, _, i, _) -> (c, i)) l in
  return (code, MLiteral.mk_map ep_map, lazy_ids)

and compile_value_f ~config ~scenario_vars ?tstorage = function
  | Literal l | Bounded (_, l) -> compile_literal l
  | Contract {address; entry_point} ->
      compile_literal (Literal.address address ?entry_point)
  | Record (layout, entries) ->
      Binary_tree.cata snd MLiteral.pair (compile_record ~layout entries)
  | Variant (layout, row, name, x) ->
    ( match name with
    | "None" -> MLiteral.none
    | "Some" -> MLiteral.some x
    | "Left" -> MLiteral.left x
    | "Right" -> MLiteral.right x
    | _ ->
        let ctxt = compile_variant ~row ~layout name in
        let l x _ = MLiteral.left x in
        let r _ x = MLiteral.right x in
        Binary_tree.context_cata x l r ctxt )
  | List (_, xs) -> MLiteral.list xs
  | Ticket _ -> MLiteral.string "(some ticket)"
  | Set (_, xs) -> MLiteral.set xs
  | Map (_, _, _, xs) -> MLiteral.mk_map xs
  | Tuple xs -> compile_tuple MLiteral.pair xs
  | Operation _ -> failwith "compile_value: operation"
  | Closure ({name = name1; body}, args) ->
    ( match args with
    | [] ->
        let tags = Some [ST_target (T_var name1)] in
        let body =
          match body.c with
          | CResult
              { e =
                  EMichelson
                    ( {name; parsed; typesIn; typesOut}
                    , [{e = EVar (name2, Simple)}] ) }
            when name1 = name2 ->
              let x = Of_micheline.instruction parsed in
              begin
                match x.instr with
                | MIconcat_unresolved | MIerror _ ->
                    let typesIn =
                      List.map (Typing.mtype_of_type ~with_annots:false) typesIn
                    in
                    let typesOut =
                      List.map
                        (Typing.mtype_of_type ~with_annots:false)
                        typesOut
                    in
                    instr (MImich {name; parsed; typesIn; typesOut})
                | _ -> instr x.instr
              end
          | _ -> mi_seq [compile_command body; drop_target (T_var name1)]
        in
        let _, _, instrs =
          run ~config ~scenario_vars ~lazy_ids:[] ?tstorage {tags} body
        in
        MLiteral.instr (mk_seq instrs)
    | _ -> failwith "compile_value: closure with arguments" )

and compile_value_internal ~config ~scenario_vars ?tstorage =
  cata_value (compile_value_f ~config ~scenario_vars ?tstorage)

and compile_contract
    ~config
    ~scenario_vars
    ~storage
    ~entry_points
    ~tstorage
    ~tparameter_lazy
    ~tparameter_non_lazy
    ~private_variables
    ~views =
  let tparameter =
    match
      ( Option.map
          (Typing.mtype_of_type_with_single ~with_annots:true)
          tparameter_lazy
      , Option.map
          (Typing.mtype_of_type_with_single ~with_annots:true)
          tparameter_non_lazy )
    with
    | None, None -> mt_unit
    | Some (al, l), Some (anl, nl) -> mt_or ?annot_left:al ?annot_right:anl l nl
    | None, Some (_anl, nl) -> nl
    | Some (_al, l), None -> l
  in
  let tparameter_annot =
    match entry_points with
    | [{channel}] when config.single_entry_point_annotation -> Some channel
    | _ -> None
  in
  let tags = Some [st_none ()] in
  let push_privates =
    private_variables
    |> iter_list (fun (name, expr) ->
           let* () = commentf "Private variable: %s" name in
           let* () = compile_expr expr in
           let* () = tag_top [ST_target (T_var name)] in
           instr (MIdig 1))
  in
  let drop_privates =
    iter_list
      (fun (name, _) -> drop_target (T_var name))
      (List.rev private_variables)
  in
  let has_ops =
    let f (ep : _ entry_point) = has_operations ep.body in
    Has_operations.(List.fold_left or_ HO_none (List.map f entry_points))
  in
  let code =
    let* r =
      match (tparameter_lazy, tparameter_non_lazy) with
      | None, None ->
          let* () = mi_cdr in
          let* () = pre_finalize_contract HO_none in
          return None
      | None, Some t_non_lazy ->
          let* () = push_privates in
          let* () = instr (mi_unpair 2) in
          let* () = compile_entry_points entry_points t_non_lazy in
          let* () = pre_finalize_contract has_ops in
          let* () = drop_privates in
          return None
      | Some t_lazy, _ ->
          let* () = push_privates in
          let* () = instr (mi_unpair 3) in
          let* () =
            tag_top [st_none (); ST_target tgt_storage; ST_target T_entry_points]
          in
          let* code_lazy, ep_map, lazy_ids =
            compile_lazy_entry_points entry_points t_lazy
          in
          let* () =
            match tparameter_non_lazy with
            | None -> code_lazy
            | Some t_non_lazy ->
                let code_non_lazy =
                  let* () =
                    with_lazy_ids
                      lazy_ids
                      (compile_entry_points entry_points t_non_lazy)
                  in
                  ensure_ops
                in
                mi_if_left ~protect_top:() code_lazy code_non_lazy
          in
          let* () = mi_rev_list Type.operation in
          let* () =
            instrs
              [ MIdug 2
              ; MI2 (Pair (None, None))
              ; MIdig 1
              ; MI2 (Pair (None, None)) ]
          in
          let* () = drop_privates in
          return (Some ep_map)
    in
    let* () = assert_singleton_stack in
    return r
  in
  let tstorage' = Typing.mtype_of_type ~with_annots:true tstorage in
  let tlazy_storage =
    match tparameter_lazy with
    | None -> None
    | Some t ->
        let t = Typing.mtype_of_type ~with_annots:true t in
        let tl =
          mt_lambda
            (mt_pair t tstorage')
            (mt_pair (mt_list mt_operation) tstorage')
        in
        Some (mt_big_map mt_nat tl)
  in
  let tstorage_with_entry_points =
    match tlazy_storage with
    | None -> tstorage'
    | Some t -> mt_pair tstorage' (remove_annots t)
  in
  let lazy_entry_points, _, instrs =
    run ~config ~scenario_vars ~lazy_ids:[] ~tstorage {tags} code
  in
  let lazy_entry_points = Option.join lazy_entry_points in
  let code =
    if config.initial_cast
    then
      let tstorage = Option.cata tstorage' (mt_pair tstorage') tlazy_storage in
      let t = remove_annots (mt_pair tparameter tstorage) in
      mk_seq ({instr = MI1 (Cast t)} :: instrs)
    else mk_seq instrs
  in
  let storage =
    match storage with
    | None -> None
    | Some storage ->
        let storage =
          compile_value_internal ~config ~scenario_vars ~tstorage storage
        in
        ( match lazy_entry_points with
        | None -> Some storage
        | Some entry_points -> Some (MLiteral.pair storage entry_points) )
  in
  let views =
    List.map
      (compile_view
         ~has_lazy_entry_points:(lazy_entry_points <> None)
         ~config
         ~scenario_vars)
      views
  in
  let c : contract =
    { tparameter = (tparameter, tparameter_annot)
    ; tstorage = tstorage_with_entry_points
    ; lazy_entry_points
    ; storage
    ; code
    ; views }
  in
  typecheck_contract ~strict_dup:true c

and compile_view
    ~has_lazy_entry_points
    ~config
    ~scenario_vars
    {name; tparameter_derived; pure; body; doc; kind} =
  let tags = Some [st_none ()] in
  let treturn = Typing.mtype_of_type ~with_annots:true body.ct in
  let body offchain =
    match get_extra tparameter_derived with
    | None when offchain ->
        let* () = if has_lazy_entry_points then mi_car else return () in
        let* () = tag_top [ST_target tgt_storage] in
        let* () = compile_command body in
        drop_target tgt_storage
    | _ ->
        let* () = instr (mi_unpair (if has_lazy_entry_points then 3 else 2)) in
        let* () =
          tag_top
            ( [ST_target tgt_parameter; ST_target tgt_storage]
            @ if has_lazy_entry_points then [ST_target T_entry_points] else []
            )
        in
        let* () =
          if has_lazy_entry_points
          then drop_target T_entry_points
          else return ()
        in
        let* () = compile_command body in
        let* () = drop_target tgt_parameter in
        drop_target tgt_storage
  in
  let offchain_code =
    let _, _, instrs =
      run ~config ~scenario_vars ~lazy_ids:[] {tags} (body true)
    in
    let code = mk_seq instrs in
    code
  in
  let onchain_code =
    match kind with
    | Offchain -> None
    | Onchain ->
        let _, _, instrs =
          run ~config ~scenario_vars ~lazy_ids:[] {tags} (body false)
        in
        let code = mk_seq instrs in
        Some code
  in
  let tparameter =
    Option.map
      (Typing.mtype_of_type ~with_annots:true)
      (get_extra tparameter_derived)
  in
  {name; pure; doc; tparameter; treturn; offchain_code; onchain_code}

let _canonicalize_instr, canonicalize_literal =
  let f_tliteral ~t tliteral =
    let t = Result.get_ok_exn t in
    let literal =
      match (t.mt, tliteral) with
      | MT0 T_timestamp, String s ->
        ( match Ptime.of_rfc3339 s with
        | Ok (t, tz, _chars_read) ->
            assert (tz = Some 0);
            let t = Ptime.to_span t in
            ( match Ptime.Span.to_int_s t with
            | None -> assert false
            | Some t -> Int (Bigint.big_int_of_int t) )
        | Error _ -> Int (Bigint.big_int_of_string s) )
      | MT0 T_key_hash, String s ->
          Bytes (Misc.Hex.unhex (Bs58.decode_key_hash s))
      | MT0 T_signature, String s ->
          Bytes (Misc.Hex.unhex (Bs58.decode_signature s))
      | MT0 T_address, String s ->
          Bytes (Misc.Hex.unhex (Bs58.decode_address s))
      | MT0 T_key, String s -> Bytes (Misc.Hex.unhex (Bs58.decode_key s))
      | ( ( MT0
              ( T_unit | T_bool | T_nat | T_int | T_mutez | T_string | T_bytes
              | T_chain_id | T_timestamp | T_address | T_key | T_key_hash
              | T_signature | T_operation | T_sapling_state _
              | T_sapling_transaction _ | T_never | T_bls12_381_g1
              | T_bls12_381_g2 | T_bls12_381_fr | T_chest_key | T_chest )
          | MT1 ((T_option | T_list | T_set | T_contract | T_ticket), _)
          | MT2 ((T_pair _ | T_or _ | T_lambda | T_map | T_big_map), _, _)
          | MT_var _ )
        , ( ( Int _ | Bool _ | String _ | Bytes _ | Unit | Pair _ | None_
            | Left _ | Right _ | Some_ _ | Seq _ | Elt _ | AnyMap _ | Instr _
            | Constant _ ) as l ) ) ->
          l
    in
    {literal}
  in
  let f_tinstr ~stack_in:_ ~stack_out:_ instr = {instr} in
  tcata {f_tinstr; f_tliteral}

let pack_value ~config ~scenario_vars ?tstorage v =
  let t = Typing.mtype_of_type ~with_annots:false (type_of_value v) in
  let tparameter = (mt_unit, None) in
  let v = compile_value_internal ~config ~scenario_vars ?tstorage v in
  let v = optimize_literal ~config v in
  let v = typecheck_literal ~strict_dup:true ~tparameter t v in
  let v = canonicalize_literal v in
  let mich = Michelson.To_micheline.literal v in
  Micheline_encoding.(pack_prefix ^ pack_node_expression (node_of_mich mich))

let unpack_value ~config t b =
  let bytes =
    Base.String.chop_prefix_exn b ~prefix:Micheline_encoding.pack_prefix
  in
  let mich = Micheline_encoding.mich_of_bytes (Misc.Hex.hexcape bytes) in
  let pp_mich t i =
    Michelson.display_instr
      (Typing.mtype_of_type ~with_annots:false t)
      (Michelson.Of_micheline.instruction i)
  in
  let value = Value.of_micheline ~config ~pp_mich t mich in
  value

let compile_value ~config ~scenario_vars ?tstorage v =
  optimize_literal
    ~config
    (compile_value_internal ~config ~scenario_vars ?tstorage v)

let compile_instance
    ~scenario_vars
    { state = {storage}
    ; template = {tcontract = {entry_points; private_variables; derived; views}}
    } =
  let {tparameter_lazy; tparameter_non_lazy; config; tstorage} =
    get_extra derived
  in
  let c =
    compile_contract
      ~scenario_vars
      ~storage
      ~entry_points
      ~tstorage
      ~tparameter_lazy
      ~tparameter_non_lazy
      ~config
      ~private_variables
      ~views
  in
  optimize_tcontract ~config c
