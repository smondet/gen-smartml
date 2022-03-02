(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Basics
open Typed
open Control

let debug_level = 0

let dbg lvl fmt =
  Printf.ksprintf (if debug_level > lvl then print_endline else fun _ -> ()) fmt

type var =
  { typ : Type.t
  ; occs : int
  ; is_mutable : bool }

let empty_env = String.Map.empty

let mk_env xs =
  String.Map.of_list
    (List.map (fun (x, typ) -> (x, {typ; occs = 0; is_mutable = false})) xs)

let string_of_static_id {static_id} =
  Printf.sprintf "static_contract%d" static_id

let string_of_dynamic_id {dynamic_id} =
  Printf.sprintf "dynamic_contract%d" dynamic_id

let string_of_contract_id = function
  | C_static id -> string_of_static_id id
  | C_dynamic id -> string_of_dynamic_id id

type state =
  { tvar_counter : int
  ; vars : var String.Map.t
  ; constraints : (line_no * typing_constraint) list
  ; inside_contract : bool
  ; config : Config.t
  ; warnings : smart_except list list }

module type ACTION = sig
  include MONAD_CORE

  val set : state -> unit t

  val get : state t

  val warn : smart_except list -> unit t

  val run : 'a t -> state -> 'a * state

  val map_list : ('a -> 'b t) -> 'a list -> 'b list t
end

module Action : ACTION = struct
  type 'a t = state -> 'a * state

  let run x s = x s

  let return (x : 'a) : 'a t = fun s -> (x, s)

  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
   fun s ->
    let y, s = x s in
    f y s

  let map f x = bind x (fun x -> return (f x))

  let apply f x = bind f (fun f -> bind x (fun x -> return (f x)))

  let set s _ = ((), s)

  let get s = (s, s)

  let warn w s = ((), {s with warnings = w :: s.warnings})

  let map_list f =
    let rec map_list acc s = function
      | [] -> (List.rev acc, s)
      | x :: xs ->
          let y, s = f x s in
          map_list (y :: acc) s xs
    in
    flip (map_list [])
end

module ActionM = struct
  include Action
  include Monad (Action)
end

open ActionM

open Syntax (ActionM)

let get_vars =
  let* s = get in
  return s.vars

let get_config =
  let* s = get in
  return s.config

let modify f =
  let* s = get in
  set (f s)

let set_config config = modify (fun s -> {s with config})

let set_vars vars = modify (fun s -> {s with vars})

let set_inside_contract inside_contract =
  modify (fun s -> {s with inside_contract})

let modify_vars f = modify (fun s -> {s with vars = f s.vars})

let fresh_tvar =
  let* ({tvar_counter = i} as s) = get in
  let* () = set {s with tvar_counter = i + 1} in
  return (Type.unknown_raw (ref (Type.UUnknown ("a" ^ string_of_int i))))

let string_of_scenario_var i = Printf.sprintf "scenario_var%s" i

let drop_non_scenario_vars ~keep_storage =
  let* vars = get_vars in
  set_vars
    (String.Map.filter
       (fun v _ ->
         Utils.String.is_prefix "scenario_var" v
         || (Option.is_some keep_storage && v = "__storage__"))
       vars)

let pp_bin ~line_no op x y () = [`Text op; `Exprs [x; y]; `Br; `Line line_no]

let pp_bin_op ~line_no op x y () =
  [`Expr x; `Text op; `Expr y; `Br; `Line line_no]

let add_constraint ~line_no c =
  modify (fun st -> {st with constraints = (line_no, c) :: st.constraints})

let assertEqual ~line_no ~pp t1 t2 =
  if Type.unF t1 != Type.unF t2
  then add_constraint ~line_no (AssertEqual (t1, t2, pp))
  else return ()

let checkComparable ~line_no x y =
  let pp = pp_bin ~line_no "comparison between" x y in
  let* () = assertEqual ~line_no x.et y.et ~pp in
  let* () = add_constraint ~line_no (IsComparable x) in
  let* () = add_constraint ~line_no (IsComparable y) in
  return ()

let solve_constraints =
  let* s = get in
  let* config = get_config in
  Solver.run ~config s.constraints;
  modify (fun s -> {s with constraints = []})

let err ~line_no msg =
  raise (SmartExcept [`Text "Declaration Error"; `Br; `Text msg; `Line line_no])

let add_var_general ~line_no name typ is_mutable =
  modify_vars (fun vars ->
      match String.Map.find_opt name vars with
      | None -> String.Map.add name {typ; occs = 0; is_mutable} vars
      | Some _ ->
          err ~line_no (Printf.sprintf "Variable name %S already in use." name))

let add_var ~line_no name typ = add_var_general ~line_no name typ false

let add_mutable_var ~line_no name typ = add_var_general ~line_no name typ true

let check_var ~line_no ?meta kind name =
  let* {vars; inside_contract} = get in
  match String.Map.find_opt name vars with
  | Some {typ} when not inside_contract -> return typ
  | Some {typ; occs; is_mutable} ->
      let* () =
        if occs = 1 && not (meta = Some ())
        then add_constraint ~line_no (IsNotHot (name, typ))
        else return ()
      in
      let* () =
        set_vars (String.Map.add name {typ; occs = occs + 1; is_mutable} vars)
      in
      return typ
  | None ->
      err
        ~line_no
        (Format.asprintf
           "%s variable %S escapes its scope. (inside_contract: %b)"
           kind
           name
           inside_contract)

let remove_var ~line_no name =
  modify_vars (fun vars ->
      match String.Map.find_opt name vars with
      | None -> err ~line_no (Printf.sprintf "Missing variable %S." name)
      | Some _ -> String.Map.filter (fun x _ -> not (String.equal name x)) vars)

let scoped x =
  let* old_vars = get_vars in
  let* r = x in
  let* () = set_vars old_vars in
  return r

let scoped_no_occs x =
  let* old_vars = get_vars in
  let* () =
    set_vars
      (String.Map.map
         (fun {typ; is_mutable} -> {typ; occs = 0; is_mutable})
         old_vars)
  in
  let* r = x in
  let* () = set_vars old_vars in
  return r

let scoped_branch x =
  let* old_vars = get_vars in
  let* r = x in
  let* new_vars = get_vars in
  let* () = set_vars old_vars in
  return (String.Map.map snd (String.Map.intersect old_vars new_vars), r)

let max_occs b1 b2 =
  let f _ x y =
    match (x, y) with
    | Some {typ; occs = occs1; is_mutable}, Some {occs = occs2} ->
        Some {typ; occs = max occs1 occs2; is_mutable}
    | Some {typ; occs; is_mutable}, None | None, Some {typ; occs; is_mutable} ->
        Some {typ; occs; is_mutable}
    | None, None -> None
  in
  String.Map.merge f b1 b2

let no_new_occs ~line_no old_vars new_vars =
  let f name x y =
    match (x, y) with
    | Some {typ; occs = 0}, Some {occs = 1} ->
        Some (add_constraint ~line_no (IsNotHot (name, typ)))
        (* TODO better error message *)
    | _ -> None
  in
  iter_list id String.Map.(values (merge f old_vars new_vars))

let for_variant ~line_no name t =
  let open Type in
  match name with
  | "None" ->
      let* () =
        assertEqual ~line_no t Type.unit ~pp:(fun () ->
            [`Text "Argument to None must be unit."])
      in
      let* t = fresh_tvar in
      return (option t)
  | "Some" -> return (option t)
  | "Left" ->
      let* u = fresh_tvar in
      return (tor t u)
  | "Right" ->
      let* u = fresh_tvar in
      return (tor u t)
  | _ -> return (uvariant name t)

let check_assignable line_no vars =
  let f x y =
    match (x, y) with
    | Some x, _ | _, Some x -> Some x
    | None, None -> None
  in
  let alg = monoid_talg f None in
  let f_texpr ln t = function
    | EVar (x, _) ->
      ( match String.Map.find_opt x vars with
      | None ->
          Some
            [ `Text (Printf.sprintf "Variable %S is not defined" x)
            ; `Line line_no ]
      | Some {is_mutable = false} ->
          Some
            [ `Text (Printf.sprintf "Variable %S is not mutable" x)
            ; `Line line_no ]
      | Some _ -> None )
    | EItem {items; key = _} -> items
    | e -> alg.f_texpr ln t e
  in
  let alg = {alg with f_texpr} in
  fun e ->
    match cata_texpr alg e with
    | None -> ()
    | Some err -> raise (SmartExcept err)

let check_command_f line_no c =
  let* config = get_config in
  let module Printer = (val Printer.get config : Printer.Printer) in
  let assertEqual = assertEqual ~line_no in
  let add_var = add_var ~line_no in
  let remove_var = remove_var ~line_no in
  let add_constraint = add_constraint ~line_no in
  match c with
  | CMatch (scrutinee, cases) ->
      let* scrutinee = scrutinee in
      let* rt = fresh_tvar in
      let check_case (constructor, arg_name, body) =
        scoped_branch
          (let* at = fresh_tvar in
           let* vt = for_variant ~line_no constructor at in
           let* () =
             assertEqual scrutinee.et vt ~pp:(fun () ->
                 [ `Text "match scrutinee has incompatible type:"
                 ; `Expr scrutinee
                 ; `Br
                 ; `Text "Expected type:"
                 ; `Type vt
                 ; `Line line_no ])
           in
           let* () = add_var arg_name at in
           let* body = body in
           let* () =
             assertEqual body.ct rt ~pp:(fun () ->
                 [ `Text "match branch has type"
                 ; `Type body.ct
                 ; `Text "instead of"
                 ; `Type rt
                 ; `Line line_no ])
           in
           let* () = remove_var arg_name in
           return (constructor, arg_name, body))
      in
      let* cases = map_list check_case cases in
      let* () =
        set_vars List.(fold_left max_occs String.Map.empty (map fst cases))
      in
      let c = CMatch (scrutinee, List.map snd cases) in
      return (build_tcommand ~line_no c rt)
  | CMatchCons {expr; id; ok_match; ko_match} ->
      let* expr = expr in
      let* at = fresh_tvar in
      let vt = Type.list at in
      let* () =
        assertEqual expr.et vt ~pp:(fun () ->
            [ `Text "match list scrutinee is not a list:"
            ; `Expr expr
            ; `Br
            ; `Text "Expected type:"
            ; `Type vt
            ; `Line line_no ])
      in
      let* vars1, ok_match =
        scoped_branch
          (let t =
             Type.record_default_layout Config.Comb [("head", at); ("tail", vt)]
           in
           let* () = add_var id t in
           ok_match)
      in
      let* vars2, ko_match = scoped_branch ko_match in
      let* () = set_vars (max_occs vars1 vars2) in
      let* () =
        assertEqual ok_match.ct ko_match.ct ~pp:(fun () ->
            [`Text "sp.match_cons: cannot unify branches"; `Line line_no])
      in
      let c = CMatchCons {expr; id; ok_match; ko_match} in
      return (build_tcommand ~line_no c ok_match.ct)
  | CMatchProduct (scrutinee, Pattern_record (name, bs), c) ->
      if List.length bs < 1
      then
        raise
          (SmartExcept
             [`Text "Must match at least one record field"; `Line line_no]);
      let* bs' =
        map_list
          (fun x ->
            let* t = fresh_tvar in
            return (x, t))
          bs
      in
      let vars = List.map (fun ({var}, t) -> (var, t)) bs' in
      let fields = List.map (fun ({field}, t) -> (field, t)) bs' in
      let* scrutinee = scrutinee in
      let t = Type.urecord fields in
      let* () =
        assertEqual scrutinee.et t ~pp:(fun () ->
            [ `Text "match tuple scrutinee is not a record:"
            ; `Expr scrutinee
            ; `Br
            ; `Text "Expected type:"
            ; `Type t
            ; `Line line_no ])
      in
      let* c =
        scoped
          (let* () = iter_list (fun (name, occs) -> add_var name occs) vars in
           c)
      in
      let c' = CMatchProduct (scrutinee, Pattern_record (name, bs), c) in
      return (build_tcommand ~line_no c' c.ct)
  | CMatchProduct (scrutinee, Pattern_tuple ns, c) ->
      if List.length ns < 2
      then
        raise
          (SmartExcept
             [ `Text "Matched tuple must have at least two elements"
             ; `Line line_no ]);
      let* scrutinee = scrutinee in
      let* ts =
        map_list
          (fun n ->
            let* t = fresh_tvar in
            return (n, t))
          ns
      in
      let t = Type.tuple (List.map snd ts) in
      let* () =
        assertEqual scrutinee.et t ~pp:(fun () ->
            [ `Text "match tuple scrutinee is not a tuple:"
            ; `Expr scrutinee
            ; `Br
            ; `Text "Expected type:"
            ; `Type t
            ; `Line line_no ])
      in
      let* c =
        scoped
          (let* () = iter_list (fun (name, occs) -> add_var name occs) ts in
           c)
      in
      let c' = CMatchProduct (scrutinee, Pattern_tuple ns, c) in
      return (build_tcommand ~line_no c' c.ct)
  | CModifyProduct (e, Pattern_record (name, bs), body) ->
      let* e = e in
      let t = Type.urecord [] in
      let* () =
        assertEqual e.et t ~pp:(fun () ->
            [ `Text "sp.match_record scrutinee is not a record:"
            ; `Expr e
            ; `Br
            ; `Text "Expected type:"
            ; `Type t
            ; `Line line_no ])
      in
      let* () = add_mutable_var ~line_no name e.et in
      let* body = body in
      let* () =
        assertEqual body.ct Type.unit ~pp:(fun () ->
            [ `Text "sp.modify_record result type must not have an sp.result"
            ; `Line line_no ])
      in
      let c' = CModifyProduct (e, Pattern_record (name, bs), body) in
      return (build_tcommand ~line_no c' Type.unit)
  | CModifyProduct (lhs, Pattern_tuple vars, body) ->
      if List.length vars < 2
      then
        raise
          (SmartExcept
             [ `Text "sp.modify_tuple requires at least two arguments "
             ; `Line line_no ]);
      let* lhs = lhs in
      let* vars' =
        map_list
          (fun n ->
            let* t = fresh_tvar in
            return (n, t))
          vars
      in
      let t = Type.tuple (List.map snd vars') in
      let* () =
        assertEqual lhs.et t ~pp:(fun () ->
            [ `Text "scrutinee is not a tuple:"
            ; `Expr lhs
            ; `Br
            ; `Text "Expected type:"
            ; `Type t
            ; `Line line_no ])
      in
      let* body =
        scoped
          (let* () = iter_list (uncurry (add_mutable_var ~line_no)) vars' in
           body)
      in
      let* () =
        assertEqual body.ct t ~pp:(fun () ->
            [ `Text "sp.match_record result type:"
            ; `Type body.ct
            ; `Br
            ; `Text "Expected type:"
            ; `Type t
            ; `Line line_no ])
      in
      let c' = CModifyProduct (lhs, Pattern_tuple vars, body) in
      return (build_tcommand ~line_no c' Type.unit)
  | CModifyProduct (lhs, Pattern_single var, body) ->
      (* TODO check whether lhs is assigneable *)
      let* lhs = lhs in
      let* body =
        scoped
          (let* () = add_mutable_var ~line_no var lhs.et in
           body)
      in
      let* () =
        assertEqual body.ct lhs.et ~pp:(fun () ->
            [ `Text "sp.match result type:"
            ; `Type body.ct
            ; `Br
            ; `Text "Expected type:"
            ; `Type lhs.et
            ; `Line line_no ])
      in
      let c' = CModifyProduct (lhs, Pattern_single var, body) in
      return (build_tcommand ~line_no c' Type.unit)
  | CDefineLocal {var; rhs; is_mutable} ->
      let* rhs = rhs in
      let* () = add_var_general ~line_no var rhs.et is_mutable in
      let c' = CDefineLocal {var; rhs; is_mutable} in
      return (build_tcommand ~line_no c' Type.unit)
  | CBind (None, c1, c2) ->
      let* state = get in
      let c1, state = run c1 state in
      let c2, state = run c2 state in
      let* () = set state in
      return (build_tcommand ~line_no (CBind (None, c1, c2)) c2.ct)
  | CBind (Some x, c1, c2) ->
      let* c1 = c1 in
      let* () = add_var x c1.ct in
      let* c2 = c2 in
      return (build_tcommand ~line_no (CBind (Some x, c1, c2)) c2.ct)
  | CFor (name, e, body) ->
      let* e = e in
      let* t = fresh_tvar in
      let* () =
        assertEqual e.et (Type.list t) ~pp:(fun () ->
            [ `Text
                (Printf.sprintf
                   "for (%s : %s) in"
                   (Printer.variable_to_string name Simple)
                   (Printer.type_to_string t))
            ; `Expr e
            ; `Line line_no ])
      in
      let* old_vars = get_vars in
      let* new_vars, body =
        scoped_branch
          (let* () = add_mutable_var ~line_no name t in
           body)
      in
      let* () = no_new_occs ~line_no old_vars new_vars in
      let* () =
        assertEqual body.ct Type.unit ~pp:(fun () ->
            [ `Text "for-loop body has non-unit type"
            ; `Type body.ct
            ; `Line line_no ])
      in
      return (build_tcommand ~line_no (CFor (name, e, body)) Type.unit)
  | CIf (cond, c1, c2) ->
      let* cond = cond in
      let* vars1, c1 = scoped_branch c1 in
      let* vars2, c2 = scoped_branch c2 in
      let* () = set_vars (max_occs vars1 vars2) in
      let* () =
        assertEqual cond.et Type.bool ~pp:(fun () ->
            [`Text "sp.if"; `Expr cond; `Text ":"; `Line line_no])
      in
      let* () =
        assertEqual c1.ct c2.ct ~pp:(fun () ->
            [`Text "sp.if: cannot unify branches"; `Line line_no])
      in
      return (build_tcommand ~line_no (CIf (cond, c1, c2)) c1.ct)
  | CSetResultType (c, t) ->
      let* c = c in
      let* () =
        assertEqual c.ct t ~pp:(fun () ->
            [ `Type c.ct
            ; `Text "is not compatible with type"
            ; `Type t
            ; `Line line_no ])
      in
      return (build_tcommand ~line_no (CSetResultType (c, t)) t)
  | CWhile (e, body) ->
      let* e = e in
      let* () =
        assertEqual e.et Type.bool ~pp:(fun () ->
            [`Text "while"; `Expr e; `Line line_no])
      in
      let* old_vars = get_vars in
      let* new_vars, body = scoped_branch body in
      let* () = no_new_occs ~line_no old_vars new_vars in
      let* () =
        assertEqual body.ct Type.unit ~pp:(fun () ->
            [`Text "while body has non-unit type"; `Type body.ct; `Line line_no])
      in
      return (build_tcommand ~line_no (CWhile (e, body)) Type.unit)
  | CSetType (e, t) ->
      let* e = scoped_no_occs e in
      let* () =
        assertEqual e.et t ~pp:(fun () ->
            [ `Expr e
            ; `Text "is not compatible with type"
            ; `Type t
            ; `Line line_no ])
      in
      return (build_tcommand ~line_no (CSetType (e, t)) Type.unit)
  | CSetVar (lhs, rhs) ->
      let* vars = get_vars in
      let* lhs = scoped_no_occs lhs in
      let* rhs = rhs in
      check_assignable line_no vars lhs;
      let* () =
        assertEqual lhs.et rhs.et ~pp:(fun () ->
            [`Text "set"; `Expr lhs; `Text "="; `Expr rhs; `Line line_no])
      in
      return (build_tcommand ~line_no (CSetVar (lhs, rhs)) Type.unit)
  | c ->
      (* Commands that don't modify the context. *)
      let* c = sequence_command_f c in
      let* t =
        match c with
        | CMatch _ | CMatchCons _ | CMatchProduct _ | CModifyProduct _
         |CDefineLocal _ | CFor _ | CBind _ | CIf _ | CSetType _
         |CSetResultType _ | CWhile _ | CSetVar _ ->
            assert false (* Already treated above. *)
        | CVerify (e, _message) ->
            let* () =
              assertEqual e.et Type.bool ~pp:(fun () ->
                  [`Text "not a boolean expression"; `Line line_no])
            in
            return Type.unit
        | CDelItem (x, y) ->
            let* tvalue = fresh_tvar in
            let* () = add_constraint (IsAnyMap (y.et, tvalue, x)) in
            return Type.unit
        | CUpdateSet (x, y, add) ->
            let pp () =
              [ `Expr x
              ; `Text "."
              ; `Text (if add then "add" else "remove")
              ; `Text "("
              ; `Expr y
              ; `Text ")"
              ; `Br
              ; `Line line_no ]
            in
            let* () = assertEqual x.et (Type.set ~telement:y.et) ~pp in
            return Type.unit
        | CFailwith _ -> fresh_tvar
        | CNever e ->
            let* () =
              assertEqual e.et Type.never ~pp:(fun () ->
                  [`Text "Bad type in sp.never"; `Expr e; `Line line_no])
            in
            fresh_tvar
        | CResult e -> return e.et
        | CComment _ | CTrace _ -> return Type.unit
        | CSetEntryPoint (name, ep) ->
            let* tparameter_lazy =
              check_var ~line_no "Self parameter (lazy)" "__parameter_lazy__"
            in
            let* () =
              let* t_ep = fresh_tvar in
              assertEqual
                (Type.uvariant name t_ep)
                tparameter_lazy
                ~pp:(fun () ->
                  [`Text "sp.set_entry_point: incompatible type"; `Line line_no])
            in
            let* tstorage = check_var ~line_no "Storage" "__storage__" in
            let expected =
              Type.(
                lambda
                  no_effects
                  (pair tparameter_lazy tstorage)
                  (pair (list operation) tstorage))
            in
            let* () =
              assertEqual ep.et expected ~pp:(fun () ->
                  [ `Text "Expected entry point of type"
                  ; `Type expected
                  ; `Text ", but got"
                  ; `Type ep.et
                  ; `Line line_no ])
            in
            return Type.unit
      in
      return (build_tcommand ~line_no c t)

let check_view ~line_no tstorage private_variables (ov : _ view) =
  scoped_no_occs
    (let* () = add_mutable_var ~line_no "__storage__" tstorage in
     let* t = fresh_tvar in
     let* () =
       when_ ov.has_param (add_mutable_var ~line_no "__parameter__" t)
     in
     let* () =
       iter_list (fun (n, e) -> add_var ~line_no n e.et) private_variables
     in
     let* body = ov.body in
     return
       { ov with
         body
       ; tparameter_derived = T (if ov.has_param then Some t else None) })

let check_entry_point
    ~config
    ~line_no
    ~tparameter
    ~tparameter_lazy
    ~tstorage
    ~private_variables
    (ep : _ entry_point) =
  let is_param = function
    | {e = EVar ("__parameter__", _)} -> true
    | _ -> false
  in
  let uses_param =
    exists_command ~exclude_create_contract:false is_param (fun _ -> false)
  in
  scoped_no_occs
    (let* () =
       add_mutable_var ~line_no "__operations__" (Type.list Type.operation)
     in
     let* () = add_mutable_var ~line_no "__storage__" tstorage in
     let* tparameter_ep_derived =
       match ep.tparameter_ep with
       | `Absent -> return Type.unit
       | `Present -> fresh_tvar
       | `Annotated t -> return t
     in
     let* () = add_mutable_var ~line_no "__parameter__" tparameter_ep_derived in
     let* () = add_var ~line_no "__contract__" (Type.contract tparameter) in
     let* () = add_var ~line_no "__parameter_lazy__" tparameter_lazy in
     let* () =
       iter_list (fun (n, e) -> add_var ~line_no n e.et) private_variables
     in
     let* body = ep.body in
     let* () =
       assertEqual ~line_no body.ct Type.unit ~pp:(fun () ->
           [ `Text "Entry point "
           ; `Text ep.channel
           ; `Text " has a sp.result."
           ; `Br
           ; `Line body.line_no ])
     in
     let* () =
       if config.Config.warn_unused
          && ep.tparameter_ep = `Present
          && not (uses_param body)
       then
         warn
           [ `Text "Entry point"
           ; `Text ep.channel
           ; `Text "has unused parameter."
           ; `Line ep.line_no ]
       else return ()
     in
     return
       { ep with
         body
       ; lazify = Some (Option.default config.lazy_entry_points ep.lazify)
       ; tparameter_ep_derived = T tparameter_ep_derived })

let check_contract_f
    ~line_no (c : (texpr t, tcommand t, Type.t, untyped) contract_f) =
  dbg 1 "[checker] contract: begin";
  let ({ balance
       ; storage
       ; baker
       ; tstorage_explicit
       ; metadata
       ; flags
       ; entry_points
       ; entry_points_layout }
        : _ contract_f) =
    c
  in
  (* Merge scenario flags with contract flags (Contract flags take precedence) *)
  let* config = get_config in
  let config = List.fold_left Config.apply_flag config flags in
  let* balance = sequence_option balance in
  let* storage = sequence_option storage in
  let* baker = sequence_option baker in
  let* () =
    when_some balance (fun balance ->
        assertEqual ~line_no balance.et Type.token ~pp:(fun () ->
            [ `Expr balance
            ; `Text "is not a valid amount in sp.create_contract"
            ; `Line line_no ]))
  in
  let* () =
    when_some baker (fun baker ->
        assertEqual ~line_no baker.et (Type.option Type.key_hash) ~pp:(fun () ->
            [ `Expr baker
            ; `Text "is not a valid optional baker in sp.create_contract"
            ; `Line line_no ]))
  in
  let* tstorage = fresh_tvar in
  let* () =
    when_some storage (fun s ->
        assertEqual ~line_no tstorage s.et ~pp:(fun () ->
            [ `Expr s
            ; `Text "is not a valid storage in sp.create_contract"
            ; `Type tstorage
            ; `Line line_no ]))
  in
  let* () =
    when_some tstorage_explicit (fun t ->
        assertEqual ~line_no tstorage t ~pp:(fun () ->
            [ `Text "Invalid storage type:"
            ; `Type t
            ; `Text "instead of"
            ; `Type tstorage
            ; `Line line_no ]))
  in
  let* metadata =
    map_list
      (fun (x, y) ->
        let+ y = sequence_meta y in
        (x, y))
      metadata
  in
  scoped
    (let line_no = [] in
     let* () = set_inside_contract true in
     let* old_vars = get_vars in
     let* () =
       set_vars
         (String.Map.filter
            (fun k _ -> Base.String.is_prefix ~prefix:"scenario_var" k)
            old_vars)
     in
     dbg 2 "[checker] contract: privates";
     let* private_variables =
       scoped_no_occs
         (let* () = add_mutable_var ~line_no "__storage__" tstorage in
          map_list sequence_snd c.private_variables)
     in
     dbg 2 "[checker] contract: views";
     let* views =
       map_list (check_view ~line_no tstorage private_variables) c.views
     in
     let* tparameter = fresh_tvar in
     let* tparameter_lazy = fresh_tvar in
     let* tparameter_non_lazy = fresh_tvar in
     dbg 2 "[checker] contract: entry points";
     let* () =
       when_some entry_points_layout (fun l ->
           let f ({target} : Layout.l) =
             let* t = fresh_tvar in
             return (target, t)
           in
           let* row = map_list f (Binary_tree.to_list l) in
           let t = Type.variant (Unknown.value l) row in
           assertEqual ~line_no:[] t tparameter_non_lazy ~pp:(fun () ->
               [ `Text "incompatible entry points layout"
               ; `Type t
               ; `Type tparameter_non_lazy ]))
     in
     let* entry_points =
       map_list
         (check_entry_point
            ~config
            ~line_no
            ~tparameter
            ~tparameter_lazy
            ~tstorage
            ~private_variables)
         entry_points
     in
     let eps_public = List.filter (fun ep -> ep.originate) entry_points in
     let eps_lazy, eps_non_lazy =
       List.partition (fun ep -> Option.get ep.lazify) eps_public
     in
     let mk_row =
       List.map (fun (ep : _ entry_point) ->
           (ep.channel, get_extra ep.tparameter_ep_derived))
     in
     let mk_variant = function
       | [] -> Type.unit
       | eps -> Type.variant (Unknown.unknown ()) (mk_row eps)
     in
     let* () =
       let t = mk_variant eps_lazy in
       assertEqual ~line_no:[] t tparameter_lazy ~pp:(fun () ->
           [`Text "incompatible lazy parameter type"])
     in
     let* () =
       let t = mk_variant eps_non_lazy in
       assertEqual ~line_no:[] t tparameter_non_lazy ~pp:(fun () ->
           [`Text "incompatible non-lazy parameter type"])
     in
     let* () =
       let t = mk_variant eps_public in
       assertEqual ~line_no:[] t tparameter ~pp:(fun () ->
           [`Text "incompatible parameter type"])
     in
     let tparameter_lazy =
       match eps_lazy with
       | [] -> None
       | _ -> Some tparameter_lazy
     in
     let tparameter_non_lazy =
       match eps_non_lazy with
       | [] -> None
       | _ -> Some tparameter_non_lazy
     in
     let* () = set_inside_contract false in
     dbg 1 "[checker] contract: end";
     return
       { c with
         balance
       ; storage
       ; baker
       ; metadata
       ; entry_points
       ; private_variables
       ; views
       ; derived =
           T
             { tparameter
             ; tparameter_lazy
             ; tparameter_non_lazy
             ; tparameter_full = mk_variant entry_points
             ; tstorage
             ; config } })

let check_mprim0 ~line_no :
    Michelson_base.Type.mtype Michelson_base.Primitive.prim0 -> _ = function
  | Self None -> check_var ~line_no "Self parameter" "__parameter_contract__"
  | Self (Some name) ->
      let* t_contract = check_var ~line_no "Self parameter" "__contract__" in
      let* t_ep = fresh_tvar in
      let* () =
        assertEqual
          ~line_no
          (Type.contract (Type.uvariant name t_ep))
          t_contract
          ~pp:(fun () ->
            [`Text "sp.self_entry_point: incompatible type"; `Line line_no])
      in
      return (Type.contract t_ep)
  | p ->
      let* config = get_config in
      let module Printer = (val Printer.get config : Printer.Printer) in
      let t = Michelson_base.Typing.type_prim0 p in
      return (Type.of_mtype t)

let check_mprim1 ~line_no p x =
  let* config = get_config in
  let module Printer = (val Printer.get config : Printer.Printer) in
  let name = Printer.mprim1_to_string ~language:SmartPy p in
  let t1, t = Michelson_base.Typing.type_prim1 p in
  let t1 = Type.of_mtype t1 in
  let* () =
    assertEqual ~line_no x.et t1 ~pp:(fun () ->
        [ `Text name
        ; `Text "expects a"
        ; `Type t1
        ; `Br
        ; `Text "Got:"
        ; `Type x.et
        ; `Line line_no ])
  in
  return (Type.of_mtype t)

let check_mprim2 ~line_no p x1 x2 =
  let* config = get_config in
  let module Printer = (val Printer.get config : Printer.Printer) in
  let name = Printer.mprim2_to_string p in
  let t1, t2, t = Michelson_base.Typing.type_prim2 p in
  let t1 = Type.of_mtype t1 in
  let t2 = Type.of_mtype t2 in
  let* () =
    assertEqual ~line_no x1.et t1 ~pp:(fun () ->
        [ `Text name
        ; `Text "expects a"
        ; `Type t1
        ; `Text "as its first argument"
        ; `Br
        ; `Text "Got:"
        ; `Type x1.et
        ; `Line line_no ])
  in
  let* () =
    assertEqual ~line_no x2.et t2 ~pp:(fun () ->
        [ `Text name
        ; `Text "expects a"
        ; `Type t2
        ; `Text "as its second argument"
        ; `Br
        ; `Text "Got:"
        ; `Type x2.et
        ; `Line line_no ])
  in
  return (Type.of_mtype t)

let check_mprim3 ~line_no p x1 x2 x3 =
  let* config = get_config in
  let module Printer = (val Printer.get config : Printer.Printer) in
  let name = Printer.mprim3_to_string p in
  let t1, t2, t3, t = Michelson_base.Typing.type_prim3 p in
  let t1 = Type.of_mtype t1 in
  let t2 = Type.of_mtype t2 in
  let t3 = Type.of_mtype t3 in
  let* () =
    assertEqual ~line_no x1.et t1 ~pp:(fun () ->
        [ `Text name
        ; `Text "expects a"
        ; `Type t1
        ; `Text "as its first argument"
        ; `Br
        ; `Text "Got:"
        ; `Type x1.et
        ; `Line line_no ])
  in
  let* () =
    assertEqual ~line_no x2.et t2 ~pp:(fun () ->
        [ `Text name
        ; `Text "expects a"
        ; `Type t2
        ; `Text "as its second argument"
        ; `Br
        ; `Text "Got:"
        ; `Type x2.et
        ; `Line line_no ])
  in
  let* () =
    assertEqual ~line_no x3.et t3 ~pp:(fun () ->
        [ `Text name
        ; `Text "expects a"
        ; `Type t3
        ; `Text "as its third argument"
        ; `Br
        ; `Text "Got:"
        ; `Type x3.et
        ; `Line line_no ])
  in
  return (Type.of_mtype t)

let name_view view id = view ^ "_view_" ^ string_of_contract_id id

let check_prim0 ~line_no =
  let check_var = check_var ~line_no in
  let assertEqual = assertEqual ~line_no in
  function
  | ECstContract {type_} -> return (Type.contract type_)
  | ECst (Int {i; is_nat} as x) ->
    ( match Unknown.get is_nat with
    | Some true when Big_int.sign_big_int i < 0 ->
        raise
          (SmartExcept
             [`Text "sp.nat cannot take negative values"; `Br; `Line line_no])
    | _ -> return (Type.type_of_literal x) )
  | ECst (Mutez i) when Big_int.sign_big_int i < 0 ->
      raise
        (SmartExcept
           [ `Text "sp.tez/sp.mutez cannot take negative values"
           ; `Br
           ; `Line line_no ])
  | ECst x -> return (Type.type_of_literal x)
  | EBounded x -> return (Type.bounded (Type.type_of_literal x) false [x])
  | EMetaLocal name -> check_var ~meta:() "Meta Local" name
  | EMatchCons name -> check_var "Match list" name
  | EContract_data id ->
      let name = string_of_contract_id id ^ "_storage" in
      check_var "Contract var" name
  | EAccount_of_seed {seed = _} -> return Type.account
  | EContract_address _ -> return Type.address
  | EContract_balance _ -> return Type.token
  | EContract_baker _ ->
      let* config = get_config in
      return (Type.option (Type.baker_type_of_protocol config.protocol))
  | EContract_typed (id, entry_point) ->
      let name = string_of_contract_id id in
      let* t = check_var "Contract var" name in
      ( match entry_point with
      | None -> return t
      | Some entry_point ->
          let* t_ep = fresh_tvar in
          let* () =
            assertEqual
              (Type.contract (Type.uvariant entry_point t_ep))
              t
              ~pp:(fun () ->
                [ `Text "sp.contract: incompatible type or missing entry_point "
                ; `Text entry_point
                ; `Type t_ep
                ; `Type t
                ; `Line line_no ])
          in
          return (Type.contract t_ep) )
  | EConstant (_, t) -> return t
  | EConstantVar id ->
      let name = string_of_scenario_var id in
      check_var "Scenario var" name

let check_prim1 ~line_no prim =
  let assertEqual = assertEqual ~line_no in
  let add_constraint = add_constraint ~line_no in
  match prim with
  | ESum ->
      fun x ->
        let t = Type.intOrNat () in
        let* () =
          assertEqual x.et (Type.list t) ~pp:(fun () ->
              [`Text "sum"; `Expr x; `Line line_no])
        in
        return t
  | EResolve -> fun x -> return x.et
  | ESetDelegate ->
      fun x ->
        let* config = get_config in
        let* () =
          assertEqual
            x.et
            (Type.option (Type.baker_type_of_protocol config.protocol))
            ~pp:(fun () -> [`Text "not an optional hash"; `Line line_no])
        in
        return Type.operation
  | EUnpack t ->
      fun x ->
        let pp () = [`Text "unpack"; `Expr x; `Type t; `Line line_no] in
        let* () = assertEqual x.et Type.bytes ~pp in
        return (Type.option t)
  | EPack -> fun _ -> return Type.bytes
  | EImplicit_account ->
      fun x ->
        let* () =
          assertEqual x.et Type.key_hash ~pp:(fun () ->
              [`Text "sp.implicit_account("; `Expr x; `Text ")"; `Line line_no])
        in
        return (Type.contract Type.unit)
  | EToInt ->
      fun x ->
        let* () = add_constraint (HasInt x) in
        return (Type.int ())
  | ENeg ->
      fun x ->
        let* t = fresh_tvar in
        let* () = add_constraint (HasNeg (x, t)) in
        return t
  | ESign ->
      fun x ->
        let* () =
          assertEqual x.et (Type.int ()) ~pp:(fun () ->
              [`Text "sp.sign"; `Expr x; `Line line_no])
        in
        return (Type.int ())
  | EConcat_list ->
      fun x ->
        let* t = fresh_tvar in
        let pp expr () = [`Text "sp.concat"; `Expr expr; `Line line_no] in
        let* () = assertEqual x.et (Type.list t) ~pp:(pp x) in
        return t
  | ESize ->
      fun x ->
        let* () = add_constraint (HasSize x) in
        return (Type.nat ())
  | EListRev ->
      fun x ->
        let* t = fresh_tvar in
        let* () =
          assertEqual x.et (Type.list t) ~pp:(fun () ->
              [`Expr x; `Text ".rev()"; `Line line_no])
        in
        return (Type.list t)
  | EListItems _rev ->
      fun x ->
        let* tkey = fresh_tvar in
        let* tvalue = fresh_tvar in
        let* () =
          assertEqual x.et (Type.map ~big:false ~tkey ~tvalue) ~pp:(fun () ->
              [`Expr x; `Text ".items()"; `Line line_no])
        in
        return (Type.list (Type.key_value tkey tvalue))
  | EListKeys _rev ->
      fun x ->
        let* tkey = fresh_tvar in
        let* tvalue = fresh_tvar in
        let* () =
          assertEqual x.et (Type.map ~big:false ~tkey ~tvalue) ~pp:(fun () ->
              [`Expr x; `Text ".keys()"; `Line line_no])
        in
        return (Type.list tkey)
  | EListValues _rev ->
      fun x ->
        let* tkey = fresh_tvar in
        let* tvalue = fresh_tvar in
        let* () =
          assertEqual x.et (Type.map ~big:false ~tkey ~tvalue) ~pp:(fun () ->
              [`Expr x; `Text ".values()"; `Line line_no])
        in
        return (Type.list tvalue)
  | EListElements _rev ->
      fun x ->
        let* telement = fresh_tvar in
        let* () =
          assertEqual x.et (Type.set ~telement) ~pp:(fun () ->
              [`Expr x; `Text ".elements()"; `Line line_no])
        in
        return (Type.list telement)
  | EAddress ->
      fun x ->
        let* t = fresh_tvar in
        let* () =
          assertEqual x.et (Type.contract t) ~pp:(fun () ->
              [`Text "sp.to_address("; `Expr x; `Text ")"; `Line line_no])
        in
        return Type.address
  | EType_annotation t ->
      fun x ->
        let pp () =
          [ `Text "Mismatch type annotation"
          ; `Expr x
          ; `Br
          ; `Text "Expected type:"
          ; `Br
          ; `Type t
          ; `Line line_no ]
        in
        let* () = assertEqual x.et t ~pp in
        return t
  | EVariant name -> fun x -> for_variant ~line_no name x.et
  | EIsVariant name ->
      fun x ->
        let* t = fresh_tvar in
        let tvariant =
          match name with
          | "None" | "Some" -> Type.option t
          | _ -> Type.uvariant name t
        in
        let* () =
          assertEqual x.et tvariant ~pp:(fun () ->
              [ `Text "Incompatible variant types"
              ; `Br
              ; `Type x.et
              ; `Br
              ; `Text "and"
              ; `Br
              ; `Type tvariant
              ; `Br
              ; `Line line_no ])
        in
        return Type.bool
  | EProject i ->
      fun x ->
        let* t = fresh_tvar in
        let* () =
          assertEqual x.et (Type.utuple i t) ~pp:(fun () ->
              [ `Text "Attribute error"
              ; `Expr x
              ; `Text "has no component"
              ; `Text (string_of_int i)
              ; `Text "of type"
              ; `Type t
              ; `Br
              ; `Line line_no ])
        in
        return t
  | EAttr name ->
      fun x ->
        let* t = fresh_tvar in
        let* () =
          assertEqual
            x.et
            (Type.urecord [(name, t)])
            ~pp:(fun () ->
              [ `Text "Attribute error"
              ; `Expr x
              ; `Text "has no field"
              ; `Text name
              ; `Text "of type"
              ; `Type t
              ; `Br
              ; `Line line_no ])
        in
        return t
  | EReadTicket ->
      fun x ->
        let* t = fresh_tvar in
        let* () =
          assertEqual x.et (Type.ticket t) ~pp:(fun () ->
              [`Expr x; `Text "is not a valid ticket"; `Line line_no])
        in
        let t_data = Type.tuple [Type.address; t; Type.nat ()] in
        return (Type.pair t_data x.et)
  | EJoinTickets ->
      fun x ->
        let* t = fresh_tvar in
        let ticket_t = Type.ticket t in
        let* () =
          assertEqual x.et (Type.pair ticket_t ticket_t) ~pp:(fun () ->
              [`Expr x; `Text "is not a valid ticket list"; `Line line_no])
        in
        return (Type.option ticket_t)
  | EPairingCheck ->
      fun x ->
        let* () =
          assertEqual
            x.et
            (Type.list (Type.pair Type.bls12_381_g1 Type.bls12_381_g2))
            ~pp:(fun () ->
              [ `Expr x
              ; `Text "is not a valid list of BLS12 pairs"
              ; `Line line_no ])
        in
        return Type.bool
  | EVotingPower ->
      fun x ->
        let* config = get_config in
        let* () =
          assertEqual
            x.et
            (Type.baker_type_of_protocol config.protocol)
            ~pp:(fun () ->
              [ `Expr x
              ; `Text "voting_power requires tz1[1-3] address as parameter"
              ; `Line line_no ])
        in
        return (Type.nat ())
  | EUnbounded ->
      fun x ->
        let* t = fresh_tvar in
        let* () =
          assertEqual x.et (Type.bounded t false []) ~pp:(fun () ->
              [ `Expr x
              ; `Text "voting_power requires tz1[1-3] address as parameter"
              ; `Line line_no ])
        in
        return t
  | EConvert ->
      fun x ->
        let* u = fresh_tvar in
        let* () = add_constraint (IsConvertible (x.et, u)) in
        return u
  | EStaticView (static_id, name) ->
      fun params ->
        let* t =
          check_var "view" (name_view name (C_static static_id)) ~line_no
        in
        let* return_type = fresh_tvar in
        let* () =
          assertEqual
            t
            Type.(lambda no_effects params.et return_type)
            ~pp:(fun () ->
              [ `Text "Wrong view type"
              ; `Br
              ; `Type t
              ; `Br
              ; `Type params.et
              ; `Br
              ; `Type return_type
              ; `Br
              ; `Line line_no ])
        in
        return (Type.option return_type)

let check_prim2 ~line_no e p =
  let assertEqual = assertEqual ~line_no in
  let add_constraint = add_constraint ~line_no in
  let pp_bin = pp_bin ~line_no in
  let pp_bin_op = pp_bin_op ~line_no in
  match p with
  | EBinOp op ->
      fun x y ->
        ( match op with
        | BLe | BLt | BGe | BGt | BEq | BNeq ->
            let* () = checkComparable ~line_no x y in
            return Type.bool
        | BAdd ->
            let* () = assertEqual x.et y.et ~pp:(pp_bin_op "+" x y) in
            let e = build_texpr ~line_no e x.et in
            let* () = add_constraint (HasAdd (e, x, y)) in
            return x.et
        | BMul {overloaded} ->
            let* t = fresh_tvar in
            let e = build_texpr ~line_no e t in
            let* () = add_constraint (HasMul (e, x, y, overloaded)) in
            if overloaded
            then return t
            else
              let pp = pp_bin_op "*" x y in
              let* () = assertEqual x.et y.et ~pp in
              let* () = assertEqual e.et x.et ~pp in
              return x.et
        | BMod ->
            let pp = pp_bin_op "%" x y in
            let* () = assertEqual x.et y.et ~pp in
            let* () = assertEqual x.et (Type.intOrNat ()) ~pp in
            return (Type.nat ())
        | BEDiv ->
            let* a = fresh_tvar in
            let* b = fresh_tvar in
            let et = Type.option (Type.pair a b) in
            let e = build_texpr ~line_no e et in
            let* () = add_constraint (HasDiv (e, x, y)) in
            return et
        | BDiv ->
            let pp () =
              [ `Text "in expression"
              ; `Expr x
              ; `Text "/"
              ; `Expr y
              ; `Br
              ; `Text "Both expressions must be of type"
              ; `Type (Type.nat ())
              ; `Br
              ; `Line line_no ]
            in
            let* () = assertEqual x.et y.et ~pp in
            let* () = assertEqual x.et (Type.nat ()) ~pp in
            return (Type.nat ())
        | BSub ->
            let pp = pp_bin_op "-" x y in
            let* () = assertEqual x.et y.et ~pp in
            let* et = fresh_tvar in
            let e = build_texpr ~line_no e et in
            let* () = add_constraint (HasSub (e, x, y)) in
            return et
        | BOr ->
            let pp () =
              [ `Text "bad type for or:"
              ; `Expr x
              ; `Text "|"
              ; `Expr y
              ; `Text "is not a nat or a bool"
              ; `Br
              ; `Line line_no ]
            in
            let* () = assertEqual x.et y.et ~pp in
            let e = build_texpr ~line_no e x.et in
            let* () = add_constraint (HasBitArithmetic (e, x, y)) in
            return x.et
        | BAnd ->
            let pp () =
              [ `Text "bad type for and:"
              ; `Expr x
              ; `Text "&"
              ; `Expr y
              ; `Text "is not a nat or a bool"
              ; `Br
              ; `Line line_no ]
            in
            let* () = assertEqual x.et y.et ~pp in
            let e = build_texpr ~line_no e x.et in
            let* () = add_constraint (HasBitArithmetic (e, x, y)) in
            return x.et
        | BXor ->
            let pp () =
              [ `Text "bad type for xor:"
              ; `Expr x
              ; `Text "^"
              ; `Expr y
              ; `Text "is not a nat or a bool"
              ; `Br
              ; `Line line_no ]
            in
            let* () = assertEqual x.et y.et ~pp in
            let e = build_texpr ~line_no e x.et in
            let* () = add_constraint (HasBitArithmetic (e, x, y)) in
            return x.et
        | BMax ->
            let pp = pp_bin "max" x y in
            let* () = assertEqual x.et y.et ~pp in
            let* () = assertEqual x.et (Type.intOrNat ()) ~pp in
            return x.et
        | BMin ->
            let pp = pp_bin "min" x y in
            let* () = assertEqual x.et y.et ~pp in
            let* () = assertEqual x.et (Type.intOrNat ()) ~pp in
            return x.et )
  | EContains ->
      fun x y ->
        let* () = add_constraint (HasContains (y, x, line_no)) in
        return Type.bool
  | ECons ->
      fun x l ->
        let* () =
          assertEqual l.et (Type.list x.et) ~pp:(fun () ->
              [`Text "cannot cons"; `Exprs [x; l]; `Line line_no])
        in
        return l.et
  | EGetOpt ->
      fun x y ->
        let* r = fresh_tvar in
        let* () = add_constraint (IsAnyMap (x.et, r, y)) in
        return (Type.option r)
  | EAdd_seconds ->
      fun t s ->
        let pp () =
          [`Text "sp.add_seconds("; `Exprs [t; s]; `Text ")"; `Line line_no]
        in
        let* () = assertEqual Type.timestamp t.et ~pp in
        let* () = assertEqual (Type.int ()) s.et ~pp in
        return Type.timestamp
  | ECallLambda ->
      fun parameter l ->
        let* t = fresh_tvar in
        let* () =
          assertEqual
            l.et
            Type.(lambda (unknown_effects ()) parameter.et t)
            ~pp:(fun () ->
              [ `Expr l
              ; `Text "does not apply to"
              ; `Expr parameter
              ; `Line line_no ])
        in
        return t
  | EApplyLambda ->
      fun parameter lambda ->
        let* t1 = fresh_tvar in
        let* t2 = fresh_tvar in
        let* () =
          assertEqual
            lambda.et
            (Type.lambda Type.no_effects (Type.pair parameter.et t1) t2)
            ~pp:(fun () ->
              [ `Expr lambda
              ; `Text "does not apply to"
              ; `Expr parameter
              ; `Line line_no ])
        in
        return Type.(lambda no_effects t1 t2)
  | ETicket ->
      fun content amount ->
        let* () =
          assertEqual amount.et (Type.nat ()) ~pp:(fun () ->
              [`Expr amount; `Text "is not a valid ticket amount"; `Line line_no])
        in
        return (Type.ticket content.et)
  | ESplitTicket ->
      fun ticket decomposition ->
        let* t = fresh_tvar in
        let ticket_t = Type.ticket t in
        let* () =
          assertEqual ticket.et ticket_t ~pp:(fun () ->
              [`Expr ticket; `Text "is not a ticket"; `Line line_no])
        in
        let* () =
          assertEqual
            decomposition.et
            (Type.pair (Type.nat ()) (Type.nat ()))
            ~pp:(fun () ->
              [ `Expr decomposition
              ; `Text "is not a ticket decomposition (nat * nat) for"
              ; `Expr ticket
              ; `Line line_no ])
        in
        return (Type.option (Type.pair ticket_t ticket_t))
  | EView (_name, return_type) ->
      fun address _params ->
        let* () =
          assertEqual address.et Type.address ~pp:(fun () -> [`Expr address])
        in
        return (Type.option return_type)

let check_prim3 ~line_no p =
  let assertEqual = assertEqual ~line_no in
  let add_constraint = add_constraint ~line_no in
  match p with
  | ERange ->
      fun a b step ->
        let pp () =
          [ `Text "Range takes integers, it cannot be applied to"
          ; `Exprs [a; b; step]
          ; `Br
          ; `Line line_no ]
        in
        let* () = assertEqual a.et b.et ~pp in
        let* () = assertEqual a.et step.et ~pp in
        let* () = add_constraint (IsInt (a.et, pp)) in
        return (Type.list a.et)
  | ESplit_tokens ->
      fun mutez quantity total ->
        let pp () =
          [ `Text "sp.split_tokens("
          ; `Exprs [mutez; quantity; total]
          ; `Text ")"
          ; `Line line_no ]
        in
        let* () = assertEqual Type.token mutez.et ~pp in
        let* () = assertEqual (Type.nat ()) quantity.et ~pp in
        let* () = assertEqual (Type.nat ()) total.et ~pp in
        return Type.token
  | EUpdate_map ->
      fun key value map ->
        let* tvalue = fresh_tvar in
        let pp () =
          [ `Expr value
          ; `Text "is of type"
          ; `Type value.et
          ; `Text ", but should be an option."
          ; `Line line_no ]
        in
        let* () = assertEqual value.et (Type.option tvalue) ~pp in
        let* () = add_constraint (IsAnyMap (key.et, tvalue, map)) in
        return map.et
  | EGet_and_update ->
      fun key value map ->
        let* tvalue = fresh_tvar in
        let pp () =
          [ `Expr value
          ; `Text "is of type"
          ; `Type value.et
          ; `Text ", but should be an option."
          ; `Line line_no ]
        in
        let* () = assertEqual value.et (Type.option tvalue) ~pp in
        let* () = add_constraint (IsAnyMap (key.et, tvalue, map)) in
        return (Type.pair value.et map.et)
  | ETest_ticket ->
      fun ticketer content amount ->
        let* () =
          assertEqual ticketer.et Type.address ~pp:(fun () ->
              [ `Expr ticketer
              ; `Text "is not a valid ticketer (it should be an address)"
              ; `Line line_no ])
        in
        let* () =
          assertEqual amount.et (Type.nat ()) ~pp:(fun () ->
              [`Expr amount; `Text "is not a valid ticket amount"; `Line line_no])
        in
        return (Type.ticket content.et)

let check_lambda
    ~line_no {name; body; clean_stack; with_storage; with_operations} =
  scoped_no_occs
    (let* () = return () in
     let* () =
       if clean_stack
       then drop_non_scenario_vars ~keep_storage:with_storage
       else return ()
     in
     let* tParams = fresh_tvar in
     let* tResult = fresh_tvar in
     let* () = add_var ~line_no name tParams in
     let* () =
       when_
         with_operations
         (add_mutable_var ~line_no "__operations__" (Type.list Type.operation))
     in
     let* body = body in
     let* () =
       assertEqual ~line_no body.ct tResult ~pp:(fun () ->
           [ `Text "lambda result type"
           ; `Type body.ct
           ; `Text "is not"
           ; `Type tResult
           ; `Line line_no ])
     in
     return
       { name
       ; tParams = T tParams
       ; body
       ; tResult = T tResult
       ; clean_stack
       ; with_storage
       ; with_operations })

let check_expr_f line_no (ee : (texpr t, tcommand t, Type.t, untyped) expr_f) =
  let assertEqual = assertEqual ~line_no in
  let add_constraint = add_constraint ~line_no in
  let check_var = check_var ~line_no in
  match ee with
  | ELambda l ->
      let* l = check_lambda ~line_no l in
      let effects =
        ( { with_storage = Unknown.value l.with_storage
          ; with_operations = Unknown.value l.with_operations }
          : Type.effects )
      in
      return
        (build_texpr
           ~line_no
           (ELambda l)
           Type.(lambda effects (get_extra l.tParams) (get_extra l.tResult)))
  | ECreate_contract {contract_template; baker; balance; storage} ->
      let* config = get_config in
      let* baker = baker in
      let* balance = balance in
      let* storage = storage in
      let* () =
        assertEqual
          baker.et
          (Type.option (Type.baker_type_of_protocol config.protocol))
          ~pp:(fun () ->
            [ `Expr baker
            ; `Text "is not a valid optional baker in sp.contract"
            ; `Line line_no ])
      in
      let* () =
        assertEqual balance.et Type.token ~pp:(fun () ->
            [ `Expr balance
            ; `Text "is not a valid amount in sp.create_contract"
            ; `Line line_no ])
      in
      let* c = check_contract_f ~line_no contract_template in
      let* () =
        assertEqual storage.et (get_extra c.derived).tstorage ~pp:(fun () ->
            [ `Text "sp.create_contract: incompatible storage type"
            ; `Line line_no ])
      in
      let e =
        ECreate_contract {contract_template = c; baker; balance; storage}
      in
      let et =
        Type.record_default_layout
          Config.Comb
          [("operation", Type.operation); ("address", Type.address)]
      in
      return (build_texpr ~line_no e et)
  | e ->
      (* Expressions that don't modify the context. *)
      let* e = sequence_expr_f e in
      let e =
        match e with
        | ECreate_contract _ | ELambda _ ->
            assert false (* Already treated above. *)
        | ( EVar _ | EPrivate _ | EMPrim0 _ | EMPrim1 _ | EMPrim1_fail _
          | EMPrim2 _ | EMPrim3 _ | EPrim0 _ | EPrim1 _ | EPrim2 _ | EPrim3 _
          | EOpenVariant _ | EItem _ | ESaplingVerifyUpdate _ | EMichelson _
          | EMapFunction _ | EContract _ | ESlice _ | EMake_signature _
          | ETransfer _ | EMatch _ | ETuple _ | ERecord _ | EList _ | EMap _
          | ESet _ | EHasEntryPoint _ | EIf _ | EIsFailing _ | ECatchException _
            ) as e ->
            e
      in
      let* et =
        match e with
        | EVar (id, Scenario) ->
            let name = string_of_scenario_var id in
            check_var "Scenario var" name
        | EVar (name, _) -> check_var "variable" name
        | EPrivate n -> check_var "Private var" n
        | ELambda _ | ECreate_contract _ -> assert false
        | EList xs ->
            let* t = fresh_tvar in
            let item x =
              assertEqual x.et t ~pp:(fun () ->
                  [ `Text "bad type for list item"
                  ; `Expr x
                  ; `Text "is not a"
                  ; `Type t
                  ; `Line line_no ])
            in
            let* () = iter_list item xs in
            return (Type.list t)
        | EMPrim0 p -> check_mprim0 ~line_no p
        | EMPrim1 (p, x) -> check_mprim1 ~line_no p x
        | EMPrim1_fail _ -> assert false
        | EMPrim2 (p, x1, x2) -> check_mprim2 ~line_no p x1 x2
        | EMPrim3 (p, x1, x2, x3) -> check_mprim3 ~line_no p x1 x2 x3
        | EPrim0 p -> check_prim0 ~line_no p
        | EPrim1 (p, x) -> check_prim1 ~line_no p x
        | EPrim2 (p, x, y) -> check_prim2 ~line_no e p x y
        | EPrim3 (p, x, y, z) -> check_prim3 ~line_no p x y z
        | EOpenVariant (name, x, _missing_message) ->
            let* et =
              match name with
              | "None" -> return Type.unit
              | _ -> fresh_tvar
            in
            let tv =
              match name with
              | "Some" | "None" -> Type.option et
              | _ -> Type.uvariant name et
            in
            let* () =
              assertEqual x.et tv ~pp:(fun () ->
                  [ `Text "Variant error"
                  ; `Expr x
                  ; `Text "has no constructor"
                  ; `Text name
                  ; `Br
                  ; `Line line_no ])
            in
            return et
        | ESlice {offset; length; buffer} ->
            let pp expr () = [`Text "sp.slice"; `Expr expr; `Line line_no] in
            let* () = assertEqual offset.et (Type.nat ()) ~pp:(pp offset) in
            let* () = assertEqual length.et (Type.nat ()) ~pp:(pp length) in
            let* () = add_constraint (HasSlice buffer) in
            return (Type.option buffer.et)
        | EItem {items; key; default_value} ->
            let* et = fresh_tvar in
            let* () = add_constraint (HasGetItem (items, key, et)) in
            let* () =
              match default_value with
              | None -> return ()
              | Some e ->
                  assertEqual e.et et ~pp:(fun () ->
                      [ `Expr items
                      ; `Text "bad type for default value."
                      ; `Expr e
                      ; `Br
                      ; `Line line_no ])
            in
            return et
        | ERecord entries ->
            let layout = Unknown.unknown () in
            let row = List.map (map_snd (fun {et} -> et)) entries in
            return (Type.record_or_unit layout row)
        | EMap (big, entries) ->
            let* tkey = fresh_tvar in
            let* tvalue = fresh_tvar in
            let item (k, v) =
              let* () =
                assertEqual k.et tkey ~pp:(fun () ->
                    [ `Text "bad type for map key"
                    ; `Expr k
                    ; `Text "is not a"
                    ; `Type tkey
                    ; `Br
                    ; `Line line_no ])
              in
              let* () =
                assertEqual v.et tvalue ~pp:(fun () ->
                    [ `Text "bad type for map value"
                    ; `Expr v
                    ; `Text "is not a"
                    ; `Type tvalue
                    ; `Br
                    ; `Line line_no ])
              in
              return ()
            in
            let* () = iter_list item entries in
            return (Type.map ~big ~tkey ~tvalue)
        | ESet entries ->
            let* telement = fresh_tvar in
            let check k =
              assertEqual k.et telement ~pp:(fun () ->
                  [ `Text "bad type for set element"
                  ; `Expr k
                  ; `Text "is not an element of"
                  ; `Type (Type.set ~telement)
                  ; `Br
                  ; `Line line_no ])
            in
            let* () = iter_list check entries in
            return (Type.set ~telement)
        | ETransfer {arg; amount; destination} ->
            let pp () =
              [ `Text "transfer("
              ; `Exprs [arg; amount; destination]
              ; `Text ")"
              ; `Br
              ; `Line line_no ]
            in
            let* () = assertEqual destination.et (Type.contract arg.et) ~pp in
            let* () = assertEqual amount.et Type.token ~pp in
            return Type.operation
        | EMake_signature {secret_key; message} ->
            let pp e () = [`Text "sp.make_signature"; `Expr e; `Line line_no] in
            let* () =
              assertEqual Type.secret_key secret_key.et ~pp:(pp secret_key)
            in
            let* () = assertEqual Type.bytes message.et ~pp:(pp message) in
            return Type.signature
        | EContract {arg_type; address} ->
            let* () =
              assertEqual address.et Type.address ~pp:(fun () ->
                  [`Expr address])
            in
            return (Type.option (Type.contract arg_type))
        | EMapFunction {l; f} as e ->
            let* s = fresh_tvar in
            let* t = fresh_tvar in
            let* target = fresh_tvar in
            let* () =
              assertEqual
                f.et
                Type.(lambda no_effects s t)
                ~pp:(fun () -> [`Text "map"; `Expr f; `Line line_no])
            in
            let e = build_texpr ~line_no e target in
            let* () = add_constraint (HasMap (e, l, f)) in
            return target
        | EMichelson ({name; parsed; typesIn; typesOut}, args) ->
            let pp () =
              [`Text "sp.michelson"; `Text name; `Exprs args; `Line line_no]
            in
            let err message =
              raise (SmartExcept [`Text message; `Rec (pp ()); `Line line_no])
            in
            let code = Michelson.Of_micheline.instruction parsed in
            let ts =
              List.map (Typing.mtype_of_type ~with_annots:true) typesIn
            in
            let code =
              Michelson.typecheck_instr
                ~strict_dup:false
                ~tparameter:(Michelson.mt_unit, None)
                (* TODO: tparameter *)
                (Stack_ok ts)
                code
            in
            let* t_out =
              match (typesOut, code.stack_out) with
              | _, Error msg ->
                  raise
                    (SmartExcept
                       [ `Text "Error in inlined Michelson code: "
                       ; `Text msg
                       ; `Line line_no ])
              | [t], Ok Stack_failed ->
                  return t (* FAILED is compatible with anything *)
              | [t], Ok (Stack_ok [t_result]) ->
                  let t_specified = Typing.mtype_of_type ~with_annots:false t in
                  if Michelson.equal_mtype t_result t_specified
                  then return t
                  else
                    raise
                      (SmartExcept
                         [ `Text "Output from Michelson code is of type"
                         ; `Type (Type.of_mtype t_result)
                         ; `Text "Expected: "
                         ; `Type t
                         ; `Line line_no ])
              | [_], Ok (Stack_ok _) ->
                  raise
                    (SmartExcept
                       [ `Text "Non-singleton output stack from Michelson code"
                       ; `Line line_no ])
              | _ -> assert false
            in
            let rec matchTypes tin targs =
              match (tin, targs) with
              | [], [] -> return ()
              | [], _ -> err "Too many arguments"
              | _, [] -> err "Too few arguments"
              | t1 :: tin, t2 :: targs ->
                  let* () = assertEqual t1 t2 ~pp in
                  matchTypes tin targs
            in
            let* () = matchTypes (List.map (fun e -> e.et) args) typesIn in
            return t_out
        | ETuple es -> return (Type.tuple (List.map (fun {et} -> et) es))
        | EMatch (scrutinee, clauses) ->
            let* result_t = fresh_tvar in
            let* row =
              clauses
              |> map_list (fun (constructor, rhs) ->
                     let* arg_t = fresh_tvar in
                     let* () =
                       assertEqual
                         rhs.et
                         Type.(lambda no_effects arg_t result_t)
                         ~pp:(fun () ->
                           [ `Text "incompatible result types in match"
                           ; `Line line_no ])
                     in
                     return (constructor, arg_t))
            in
            let* () =
              assertEqual
                scrutinee.et
                (Type.variant (Unknown.unknown ()) row)
                ~pp:(fun () -> [`Text "incoherent scrutinee type"])
            in
            return result_t
        | ESaplingVerifyUpdate {state; transaction} ->
            let* () =
              assertEqual
                transaction.et
                (Type.sapling_transaction None)
                ~pp:(fun () ->
                  [ `Expr transaction
                  ; `Text "is expected to be a sapling transaction "
                  ; `Line line_no ])
            in
            let* () =
              assertEqual state.et (Type.sapling_state None) ~pp:(fun () ->
                  [ `Expr state
                  ; `Text "is expected to be a sapling state "
                  ; `Line line_no ])
            in
            let* () = add_constraint (SaplingVerify (state, transaction)) in
            return (Type.option (Type.pair (Type.int ()) state.et))
        | EHasEntryPoint name ->
            let* tparameter_lazy =
              check_var "Self parameter (lazy)" "__parameter_lazy__"
            in
            let* () =
              let* t_ep = fresh_tvar in
              assertEqual
                (Type.uvariant name t_ep)
                tparameter_lazy
                ~pp:(fun () ->
                  [`Text "sp.has_entry_point: incompatible type"; `Line line_no])
            in
            return Type.bool
        | EIf (cond, a, b) ->
            let* () =
              assertEqual a.et b.et ~pp:(fun () ->
                  [ `Text "incompatible result types in if-then-else"
                  ; `Line line_no ])
            in
            let* () =
              assertEqual cond.et Type.bool ~pp:(fun () ->
                  [`Text "non-boolean condition in if-then-else"; `Line line_no])
            in
            return a.et
        | EIsFailing _ -> return Type.bool
        | ECatchException (t, _) -> return (Type.option t)
      in
      return (build_texpr ~line_no e et)

let check = {f_expr = check_expr_f; f_command = check_command_f; f_type = id}

let check_command x = cata_command check x

let check_expr = cata_expr check

let check_contract ({contract} : contract) =
  let c = map_contract_f check_expr check_command id id contract in
  let* tcontract = check_contract_f ~line_no:[] c in
  List.iter
    (fun flag -> Basics.check_initial_flag ~line_no:[] flag)
    contract.flags;
  return {tcontract}

let check_action (config, action) =
  let* () = set_config config in
  match action with
  | New_contract {id; contract; line_no; accept_unknown_types; show; address} ->
      let* ({tcontract = c} as contract) = check_contract {contract} in
      let {tparameter; tparameter_full} = get_extra c.derived in
      let* () =
        add_var ~line_no (string_of_contract_id id) (Type.contract tparameter)
      in
      let* () =
        iter_list
          (fun (view : _ view) ->
            let source =
              match get_extra view.tparameter_derived with
              | Some t -> t
              | None -> Type.unit
            in
            add_var
              ~line_no
              (name_view view.name id)
              Type.(lambda no_effects source view.body.ct))
          c.views
      in
      let* () =
        add_var
          ~line_no
          (string_of_contract_id id ^ "_full")
          (Type.contract tparameter_full)
      in
      let* () =
        add_var
          ~line_no
          (string_of_contract_id id ^ "_storage")
          (get_extra contract.tcontract.derived).tstorage
      in
      return
        ( config
        , New_contract
            { id
            ; contract = contract.tcontract
            ; line_no
            ; accept_unknown_types
            ; show
            ; address } )
  | Compute {var; expression; line_no} ->
      let* expression = check_expr expression in
      let* () = add_var ~line_no (string_of_scenario_var var) expression.et in
      return (config, Compute {var; expression; line_no})
  | Simulation {id; line_no} -> return (config, Simulation {id; line_no})
  | Message
      { id
      ; valid
      ; exception_
      ; params
      ; line_no
      ; title
      ; messageClass
      ; source
      ; sender
      ; chain_id
      ; time
      ; amount
      ; level
      ; voting_powers
      ; message
      ; show
      ; export } ->
      let check_expr = check_expr in
      let* valid = check_expr valid in
      let* exception_ = map_option check_expr exception_ in
      let* params = check_expr params in
      let* voting_powers = check_expr voting_powers in
      let check_aa = function
        | Address a ->
            let* a = check_expr a in
            return (Address a)
        | Account a -> return (Account a)
      in
      let* source = map_option check_aa source in
      let* sender = map_option check_aa sender in
      let* chain_id = map_option check_expr chain_id in
      let* time = map_option check_expr time in
      let* amount = check_expr amount in
      let* level = map_option check_expr level in
      let id' = string_of_contract_id id in
      let* c =
        match id with
        | C_dynamic _ -> check_var ~line_no "contract id" id'
        | _ -> check_var ~line_no "contract id" (id' ^ "_full")
      in
      let* entry = fresh_tvar in
      let* () =
        let pp () =
          [ `Text id'
          ; `Text "is of type"
          ; `Type c
          ; `Text "which lacks entry point"
          ; `Text message
          ; `Text "with argument"
          ; `Type entry
          ; `Line line_no ]
        in
        assertEqual ~line_no ~pp c (Type.contract (Type.uvariant message entry))
      in
      let pp () =
        [ `Text "entry point expects parameter"
        ; `Br
        ; `Type entry
        ; `Br
        ; `Text "but got"
        ; `Br
        ; `Type params.et
        ; `Line line_no ]
      in
      let* () = assertEqual ~line_no params.et entry ~pp in
      return
        ( config
        , Message
            { id
            ; valid
            ; exception_
            ; params
            ; line_no
            ; title
            ; messageClass
            ; source
            ; sender
            ; chain_id
            ; time
            ; amount
            ; level
            ; voting_powers
            ; message
            ; show
            ; export } )
  | ScenarioError {message} -> return (config, ScenarioError {message})
  | Html {tag; inner; line_no} -> return (config, Html {tag; inner; line_no})
  | Verify {condition; line_no} ->
      let* condition = check_expr condition in
      let pp () = [`Text "not a boolean expression"; `Line line_no] in
      let* () = assertEqual ~line_no condition.et Type.bool ~pp in
      return (config, Verify {condition; line_no})
  | Show {expression; html; stripStrings; compile; line_no} ->
      let* expression = check_expr expression in
      return (config, Show {expression; html; stripStrings; compile; line_no})
  | Exception es -> return (config, Exception es)
  | Add_flag flag ->
      return (config, Add_flag flag) (* Handled in Scenario.acc_config. *)
  | Set_delegate {id; line_no; baker} ->
      let* baker = check_expr baker in
      let id' = string_of_contract_id id in
      let* _ = check_var ~line_no "contract id" id' in
      let pp () = [`Text "scenario set_delegate"; `Type baker.et] in
      let* () =
        assertEqual
          ~line_no:[]
          baker.et
          (Type.option (Type.baker_type_of_protocol config.protocol))
          ~pp
      in
      return (config, Set_delegate {id; line_no; baker})
  | DynamicContract {id; model_id; line_no} ->
      let id' = string_of_dynamic_id id in
      let model_id' = string_of_contract_id model_id in
      let* tcontract = check_var ~line_no "contract id" model_id' in
      let* tstorage =
        check_var ~line_no "contract id" (model_id' ^ "_storage")
      in
      let* () = add_var ~line_no id' tcontract in
      let* () = add_var ~line_no (id' ^ "_storage") tstorage in
      return (config, DynamicContract {id; model_id; line_no})
  | Prepare_constant_value {line_no; var; hash; expression} ->
      let* expression = check_expr expression in
      let* () = add_var ~line_no (string_of_scenario_var var) expression.et in
      return (config, Prepare_constant_value {line_no; var; hash; expression})

let default_if condition ~line_no t =
  let open Type in
  match Type.getRepr t with
  | TUnknown {contents = UUnknown _} as t when condition ->
      let* () =
        assertEqual ~line_no (F t) Type.unit ~pp:(fun () -> [`Text "defaulter"])
      in
      return Type.unit
  | t -> return (F t)

let is_storage = function
  | {e = EVar ("__storage__", _); et = _} -> true
  | _ -> false

let rec default_contract {tcontract = c} =
  let ({ tparameter
       ; tparameter_lazy
       ; tparameter_non_lazy
       ; tparameter_full
       ; tstorage
       ; config }
        : _ contract_derived) =
    get_extra c.derived
  in
  let uses_storage =
    exists_contract ~exclude_create_contract:true is_storage (fun _ -> false) c
  in
  let* tstorage = default_if (not uses_storage) ~line_no:[] tstorage in
  let* tparameter = default_if (c.entry_points = []) ~line_no:[] tparameter in
  let* entry_points = map_list default_entry_point c.entry_points in
  let* private_variables =
    let f (n, e) =
      let* e = cata_texpr default_alg e in
      return (n, e)
    in
    map_list f c.private_variables
  in
  let* () = solve_constraints in
  let c =
    { c with
      entry_points
    ; private_variables
    ; derived =
        T
          { tparameter
          ; tparameter_lazy
          ; tparameter_non_lazy
          ; tparameter_full
          ; tstorage
          ; config } }
  in
  match (c.storage, Type.getRepr tstorage) with
  | None, Type.T0 T_unit ->
      let storage =
        Some (build_texpr ~line_no:[] (EPrim0 (ECst Literal.unit)) Type.unit)
      in
      return {tcontract = {c with storage}}
  | _ -> return {tcontract = c}

and default_entry_point (ep : _ entry_point) =
  let* body = cata_tcommand default_alg ep.body in
  return {ep with body}

and default_alg =
  let f_texpr line_no et e =
    let* e = sequence_expr_f e in
    match e with
    | ECreate_contract {contract_template = ct; baker; balance; storage} ->
        let* ct = default_contract {tcontract = ct} in
        return
          (build_texpr
             ~line_no
             (ECreate_contract
                {contract_template = ct.tcontract; baker; balance; storage})
             et)
    | e -> return (build_texpr ~line_no e et)
  in
  let f_tcommand line_no ct = function
    | CBind (x, c1, c2) ->
        let* c1 = c1 in
        let* c2 = c2 in
        return (build_tcommand ~line_no (CBind (x, c1, c2)) ct)
    | c ->
        let* c = sequence_command_f c in
        return (build_tcommand ~line_no c ct)
  in
  {f_texpr; f_tcommand; f_ttype = (fun t -> t)}

let mapM_action_contract f (config, action) =
  match action with
  | New_contract ({contract} as c) ->
      let* y = f {tcontract = contract} in
      return (config, New_contract {c with contract = y.tcontract})
  | ( Compute _ | Simulation _ | Message _ | ScenarioError _ | Html _ | Verify _
    | Show _ | Exception _ | Set_delegate _ | DynamicContract _ | Add_flag _
    | Prepare_constant_value _ ) as c ->
      return (config, c)

let default_action = mapM_action_contract default_contract

(** {1 Non-monadic interface for the outside world} *)

let run_with config f vars x =
  run
    (f x)
    { tvar_counter = 0
    ; vars
    ; constraints = []
    ; inside_contract = false
    ; config
    ; warnings = [] }

let exec_with config f vars x = fst (run_with config f vars x)

let check_lambda config vars =
  exec_with
    config
    (fun l ->
      let body = check_command l.body in
      let* e = check_lambda ~line_no:[] {l with body} in
      let* () = solve_constraints in
      return e)
    (mk_env vars)

let check_expr config vars =
  exec_with
    config
    (fun e ->
      let* e = check_expr e in
      let* () = solve_constraints in
      return e)
    (mk_env vars)

let check_command config vars =
  exec_with
    config
    (fun c ->
      let* c = check_command c in
      let* () = solve_constraints in
      return c)
    (mk_env vars)

let check_contract config =
  exec_with
    config
    (fun c ->
      let* c = check_contract c in
      let* () = solve_constraints in
      let* c = default_contract c in
      let* () = solve_constraints in
      return c)
    empty_env

let check_scenario config s =
  dbg 1 "[checker] scenario: begin";
  let x, {warnings} =
    run_with
      config
      (fun s ->
        let* actions = Action.map_list check_action s.actions in
        let* () = solve_constraints in
        let* actions = Action.map_list default_action actions in
        return {s with actions})
      empty_env
      s
  in
  dbg 1 "[checker] scenario: end";
  (x, warnings)
