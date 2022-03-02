(* Copyright 2019-2021 Smart Chain Arena LLC. *)

module Scenario_bak = Scenario
open SmartML
module Scenario = Scenario_bak
open Utils
open! Basics
open! Scenario
open! Caml.Format

module Error = struct
  type t = {message : string}

  let make message = {message}

  let pp_quick ppf e = pp_print_text ppf e.message
end

module Decorated_result = struct
  type content =
    [ `Path of string
    | `Text of string
    | `Code of string list
    | `Int of int
    | `Error of Error.t
    | `O of (string * content) list
    ]

  type 'a t =
    { result : ('a, Error.t) Result.t
    ; attach : content list }

  let return o = {result = Ok o; attach = []}

  let fail e = {result = Error e; attach = []}

  let bind x ~f =
    match x.result with
    | Ok o ->
        let rr = f o in
        {result = rr.result; attach = x.attach @ rr.attach}
    | Error _ as e -> {result = e; attach = x.attach}

  let attach ~a x = {x with attach = x.attach @ a}
end

(**
   A generic client for Tezos, to be implemented by tezos-client,
   ConseilJS, or some JSON output, etc..
 *)
module type Tezos_client = sig
  module Io : sig
    type 'a t

    val return : 'a -> 'a t

    val bind : 'a t -> f:('a -> 'b t) -> 'b t
  end

  type state

  val originate :
       state
    -> id:int
    -> contract:string
    -> storage:string
    -> balance:string
    -> string Decorated_result.t Io.t

  val transfer :
       ?arg:string
    -> ?entry_point:string
    -> ?amount:int
    -> ?sender:string Basics.account_or_address
    -> ?source:string Basics.account_or_address
    -> state
    -> dst:string
    -> unit Decorated_result.t Io.t

  val get_contract_storage :
    state -> address:string -> string Decorated_result.t Io.t

  val run_script :
       state
    -> contract:string
    -> parameter:string
    -> storage:string
    -> string Decorated_result.t Io.t
end

module History_event = struct
  type t =
    { actions : Basics.taction list
    ; status : [ `Success | `Failure | `None ]
    ; expecting_success : bool
    ; attachements : Decorated_result.content list }

  let make ~actions ?(expecting_success = true) ?(attachements = []) status =
    {actions; status; expecting_success; attachements}
end

module Make_interpreter (Client : Tezos_client) (Prims : Primitives.Primitives) =
struct
  module IO = Base.Monad.Make (struct
    type 'a t = ('a, Error.t) Result.t Client.Io.t

    let return x = Client.Io.return (Ok x)

    let bind x ~f =
      Client.Io.bind x ~f:(function
          | Ok x -> f x
          | Error x -> Client.Io.return (Error x))

    let map = `Define_using_bind
  end)

  module Decorated_IO = Base.Monad.Make (struct
    type 'a t = 'a Decorated_result.t Client.Io.t

    let return x = Client.Io.return (Decorated_result.return x)

    let bind x ~f =
      Client.Io.bind x ~f:(function
          | Decorated_result.{result = Ok o; attach = a} ->
              Client.Io.bind (f o) ~f:(fun res ->
                  Client.Io.return (Decorated_result.attach ~a res))
          | {result = Error _; attach = _} as e -> Client.Io.return e)

    let map = `Define_using_bind
  end)

  open IO
  module Hashtbl = Caml.Hashtbl
  module Queue = Caml.Queue

  module State = struct
    type t =
      { client : Client.state
      ; smartml : scenario_state
      ; compute_variables : (string, string) Hashtbl.t
      ; history : History_event.t Queue.t
      ; log_advancement : string -> unit }

    let of_smartml ?(log_advancement = fun _ -> ()) ~client smartml =
      { client
      ; smartml
      ; history = Caml.Queue.create ()
      ; log_advancement
      ; compute_variables = Hashtbl.create 5 }

    let fresh ~client = of_smartml ~client (scenario_state ())

    let add_contract_address {smartml; _} ~id ~address =
      Hashtbl.replace smartml.addresses id address

    let get_contract state ~id =
      Option.(
        Hashtbl.find_opt state.smartml.addresses id
        >>= fun kt1 ->
        Hashtbl.find_opt state.smartml.contracts id >>= fun c -> return (kt1, c))

    let add_history state ev = Queue.add ev state.history

    let log_advancement state fmt =
      Caml.Format.kasprintf state.log_advancement fmt
  end

  let primitives = (module Prims : Primitives.Primitives)

  let update_tcontract ~config state ~id ~contract =
    (* Piece of “duplicated” code from `smartML/lib/smartml_scenario.ml`: *)
    let contract =
      match Hashtbl.find_opt state.State.smartml.contracts id with
      | Some contract -> contract
      | None ->
          Interpreter.interpret_contract
            ~config
            ~primitives
            ~scenario_state:state.smartml
            contract
    in
    Hashtbl.replace state.smartml.contracts id contract;

    (* let t = Hashtbl.find state.smartml.contract_data_types id in *)
    contract

  let deal_with_result
      ?(add_attachments = [])
      ?not_valid
      ?result_content
      ?(actions = [])
      state
      making_result =
    let open Decorated_result in
    Client.Io.bind (making_result ()) ~f:(fun result ->
        let ev ~a status =
          History_event.
            { actions
            ; status
            ; expecting_success = Base.Poly.(not_valid = None)
            ; attachements = add_attachments @ result.attach @ a }
        in
        match (result.result, not_valid) with
        | Ok o, None ->
            let a =
              Base.Option.value_map ~default:[] result_content ~f:(fun conv ->
                  conv o)
            in
            State.add_history state (ev ~a `Success);
            IO.return o
        | Ok o, Some _ ->
            let a =
              Base.Option.value_map ~default:[] result_content ~f:(fun conv ->
                  conv o)
            in
            State.add_history state (ev ~a `Failure);
            Client.Io.return (Error (Error.make "Unexpected success"))
        | Error e, None ->
            State.add_history state (ev ~a:[`Error e] `Failure);
            Client.Io.return (Error e)
        | Error e, Some v ->
            State.add_history state (ev ~a:[`Error e] `Success);
            IO.return v)

  let compute_texpr_contract ~config ~primitives ~scenario_state expr =
    let module Printer = (val Printer.get config : Printer.Printer) in
    let tparameter_ep =
      let fields =
        Utils.List.sort
          compare
          (Hashtbl.fold
             (fun key {template = {tcontract = {derived}}} l ->
               let tstorage = (get_extra derived).tstorage in
               let key = Printer.string_of_contract_id key in
               (sprintf "k%s" key, tstorage) :: l)
             scenario_state.contracts
             [])
        @ Utils.List.sort
            compare
            (Hashtbl.fold
               (fun id v l ->
                 let key = sprintf "v%s" id in
                 (key, Basics.type_of_value v) :: l)
               scenario_state.variables
               [])
      in
      let layout =
        Unknown.value (Type.comb_layout_of_row `Right (List.map fst fields))
      in
      Type.record_or_unit layout fields
    in
    let params = Expr.params ~line_no:[] in
    let subst =
      let contracts = Hashtbl.create 10 in
      Hashtbl.iter
        (fun key _ ->
          let getData =
            Expr.attr
              ~line_no:params.line_no
              params
              ~name:(sprintf "k%s" (Printer.string_of_contract_id key))
          in
          Hashtbl.replace contracts key getData)
        scenario_state.contracts;
      let scenario_variables = Hashtbl.create 10 in
      Hashtbl.iter
        (fun id _ ->
          let key = sprintf "v%s" id in
          let getData = Expr.attr ~line_no:params.line_no params ~name:key in
          Hashtbl.replace scenario_variables id getData)
        scenario_state.variables;
      {Replacer.contracts; scenario_variables}
    in
    let expr = Reducer.reduce_expr ~config ~primitives ~scenario_state expr in
    let expr = erase_types_expr expr in
    let expr = Replacer.replace_expr subst expr in
    let line_no = expr.line_no in
    let entryPoint =
      { channel = "compute"
      ; tparameter_ep = `Annotated tparameter_ep
      ; originate = true
      ; lazify = None
      ; lazy_no_code = None
      ; line_no = []
      ; body =
          Command.set
            ~line_no:expr.line_no
            (Expr.storage ~line_no)
            (Expr.some ~line_no expr)
      ; tparameter_ep_derived = U }
    in
    let contract =
      ( { template_id = None
        ; balance = None
        ; tstorage_explicit = None
        ; storage = Some (Expr.none ~line_no)
        ; baker = None
        ; entry_points = [entryPoint]
        ; entry_points_layout = None
        ; flags = []
        ; private_variables = []
        ; metadata = []
        ; views = []
        ; unknown_parts = None
        ; derived = U }
        : _ contract_f )
    in
    let c = {contract} in
    let c = Replacer.replace_contract subst c in
    let c = Checker.check_contract config c in
    let c = Reducer.reduce_contract ~primitives ~scenario_state c in
    Closer.close_contract c

  let compute ~(state : State.t) ~expression ~config ~action continuation =
    let module Printer = (val Printer.get config : Printer.Printer) in
    let scenario_state = state.smartml in
    let compute_contract =
      try
        let compute_contract =
          compute_texpr_contract ~config ~primitives ~scenario_state expression
        in
        Some
          (Interpreter.interpret_contract
             ~config
             ~primitives
             ~scenario_state:state.smartml
             compute_contract)
      with
      | e ->
          print_endline (Printer.exception_to_string false e);
          None
    in
    let michelson_contract =
      let scenario_vars = String.Map.of_hashtbl scenario_state.variables in
      Option.map
        (fun compute_contract ->
          Michelson.display_tcontract
            (Compiler.compile_instance ~scenario_vars compute_contract))
        compute_contract
    in
    let storage_layout =
      match compute_contract with
      | None -> None
      | Some compute_contract ->
        ( match
            Type.unF
              (get_extra compute_contract.template.tcontract.derived).tparameter
          with
        | TVariant {row = [("compute", F (TRecord {layout; _}))]} ->
            Unknown.get layout
        | _ -> None )
    in
    (* let michelson_storage = Compiler.michelson_storage contract in *)
    let add_attachments =
      let code s = `Code (Base.String.split ~on:'\n' s) in
      [ `O
          [ ( "debug"
            , `O
                [ ("expression", code (Printer.texpr_to_string expression))
                ; ( "layout"
                  , code
                      (Base.Option.value_map
                         ~default:"NONE"
                         ~f:Layout.show
                         storage_layout) ) ] ) ] ]
    in
    deal_with_result
      ~add_attachments
      ~actions:[action]
      state
      Decorated_IO.(
        fun () ->
          let bug s =
            Client.Io.return (Decorated_result.fail (Error.make ("BUG: " ^ s)))
          in
          match michelson_contract with
          | None -> bug "no michelson contract"
          | Some michelson_contract ->
              (let get_storage id kt1 prev_m =
                 prev_m
                 >>= fun prev ->
                 Client.get_contract_storage state.client ~address:kt1
                 >>= fun res ->
                 return
                   (* This has to be the same format as in
                        smartML/lib/smartml_scenario.ml *)
                   ( ( sprintf "k%s" (Printer.string_of_contract_id id)
                     , Base.String.(
                         tr ~target:'\n' ~replacement:' ' res |> strip) )
                   :: prev )
               in
               Hashtbl.fold get_storage state.smartml.addresses (return []))
              >>= fun storages ->
              begin
                match storage_layout with
                | None -> bug "storage-layout-not-set"
                | Some layout ->
                    let get_field f =
                      if Base.String.prefix f 1 = "v"
                      then
                        match
                          Hashtbl.find_opt
                            state.compute_variables
                            (String.sub f 1 (String.length f - 1))
                        with
                        | None -> kasprintf bug "missing variable: %S" f
                        | Some s -> return s
                      else
                        match
                          Base.List.Assoc.find storages ~equal:String.equal f
                        with
                        | Some s -> return s
                        | None -> kasprintf bug "missing storage: %S" f
                    in
                    let rec make_pairs = function
                      | Binary_tree.Leaf Layout.{target = field} ->
                          get_field field
                      | Binary_tree.Node (l, r) ->
                          make_pairs l
                          >>= fun left ->
                          make_pairs r
                          >>= fun right ->
                          let paren ppf s =
                            let needs_paren =
                              (* hacky but possibly correct implementation *)
                              match s with
                              | "" -> assert false
                              | _ ->
                                  if s.[0] = '"'
                                  then false
                                  else String.contains s ' '
                            in
                            if needs_paren
                            then fprintf ppf "(%s)" s
                            else fprintf ppf "%s" s
                          in
                          return (asprintf "Pair %a %a" paren left paren right)
                    in
                    make_pairs layout
              end
              >>= fun storages_laid_out ->
              print_endline "======== run script ===========";
              Client.run_script
                state.client
                ~parameter:storages_laid_out
                ~storage:"None"
                ~contract:michelson_contract
              >>= fun compute_result ->
              let pos1 = String.index compute_result '(' in
              let pos2 = String.rindex compute_result ')' in
              let computed =
                String.sub compute_result (pos1 + 6) (pos2 - pos1 - 6)
              in
              let computed =
                if computed.[0] = '('
                then
                  let pos1 = String.index computed '(' in
                  let pos2 = String.rindex computed ')' in
                  String.sub computed (pos1 + 1) (pos2 - pos1 - 1)
                else computed
              in
              let computed =
                Base.String.(tr ~target:'\n' ~replacement:' ' computed |> strip)
              in
              Printf.ksprintf print_endline "===== Computed: %s ======" computed;
              continuation ~bug computed)

  let handle_action ~config state action =
    let module Printer = (val Printer.get config : Printer.Printer) in
    let add_simple_history status action =
      let () =
        State.add_history state History_event.(make ~actions:[action] status)
      in
      return ()
    in
    match action with
    | New_contract {id; contract; line_no; accept_unknown_types; address} ->
        let config = (get_extra contract.derived).config in
        State.log_advancement
          state
          "New contract %s (l. %d)"
          (Printer.string_of_contract_id id)
          (head_line_no line_no);
        let contract =
          Reducer.reduce_contract
            ~primitives
            ~scenario_state:state.smartml
            {tcontract = contract}
        in
        let contract_full = update_tcontract ~config state ~id ~contract in
        let scenario_vars = String.Map.of_hashtbl state.smartml.variables in
        let compiled_contract =
          Compiler.compile_instance ~scenario_vars contract_full
        in
        let storage =
          match contract_full.state.storage with
          | None -> "missing storage"
          | Some storage ->
              let storage =
                Compiler.compile_value ~config ~scenario_vars storage
              in
              let storage =
                match compiled_contract.lazy_entry_points with
                | None -> storage
                | Some entry_points ->
                    Michelson.MLiteral.pair
                      storage
                      (Michelson.erase_types_literal entry_points)
              in
              Michelson.string_of_literal storage
        in
        let balance =
          Printf.sprintf
            "%.06f"
            (Big_int.float_of_big_int contract_full.state.balance /. 1000000.)
        in
        if List.length
             (Michelson.has_error_tcontract
                ~accept_missings:false
                compiled_contract)
           <> 0
        then return (assert accept_unknown_types)
        else
          deal_with_result
            ~result_content:(fun s -> [`Text (asprintf "New contract: %s" s)])
            ~actions:[action]
            state
            (fun () ->
              let static_id =
                match id with
                | C_static {static_id} -> static_id
                | C_dynamic _ -> assert false
              in
              if address = Aux.address_of_contract_id ~html:false id None
              then
                Client.originate
                  state.client
                  ~id:static_id
                  ~contract:(Michelson.display_tcontract compiled_contract)
                  ~storage
                  ~balance
              else Client.Io.return (Decorated_result.return address))
          >>= fun address ->
          State.add_contract_address state ~id ~address;
          return ()
    | Set_delegate _ -> return ()
    | Message
        { id
        ; valid
        ; params
        ; line_no
        ; title = _
        ; messageClass = _
        ; sender
        ; source
        ; chain_id = _
        ; time = _
        ; amount
        ; message } ->
        State.log_advancement
          state
          "Calling message %s#%s (l. %d)"
          (Printer.string_of_contract_id id)
          message
          (head_line_no line_no);
        ( match State.get_contract state ~id with
        | None -> assert false
        | Some (kt1, smartml_contract) ->
            let scenario_state = state.smartml in
            let amount =
              let pp () =
                [`Text "Computing amount"; `Expr amount; `Line amount.line_no]
              in
              Value.unMutez
                ~pp
                (Interpreter.interpret_expr_external
                   ~config
                   ~primitives
                   ~no_env:(pp ())
                   ~scenario_state
                   amount)
            in
            let valid =
              let pp () =
                [`Text "Computing valid"; `Expr valid; `Line valid.line_no]
              in
              Value.unBool
                ~pp
                (Interpreter.interpret_expr_external
                   ~config
                   ~primitives
                   ~no_env:(pp ())
                   ~scenario_state
                   valid)
            in
            let parse_address = function
              | Account x -> Account x
              | Address (address : Basics.Typed.texpr) ->
                  let pp () =
                    [ `Text "Computing address"
                    ; `Expr address
                    ; `Line address.line_no ]
                  in
                  Address
                    (Value.unAddress
                       ~pp
                       (Interpreter.interpret_expr_external
                          ~config
                          ~primitives
                          ~no_env:(pp ())
                          ~scenario_state
                          address))
            in
            let params_type =
              match
                Caml.(
                  List.find_opt
                    (fun x -> (x : _ entry_point).channel = message)
                    smartml_contract.template.tcontract.entry_points)
              with
              | Some x -> get_extra x.tparameter_ep_derived
              | None -> assert false
            in
            let pp () = assert false in
            let c = (line_no, AssertEqual (params_type, params.et, pp)) in
            Solver.run ~config [c];
            let params =
              let tvalue =
                Interpreter.interpret_expr_external
                  ~config
                  ~primitives
                  ~no_env:
                    [ `Text "Computing params"
                    ; `Expr params
                    ; `Line params.line_no ]
                  ~scenario_state
                  params
              in
              let scenario_vars =
                String.Map.of_hashtbl scenario_state.variables
              in
              let mich = Compiler.compile_value ~config ~scenario_vars tvalue in
              Michelson.string_of_literal mich
            in
            let entry_point =
              match
                List.length smartml_contract.template.tcontract.entry_points
              with
              | 1 -> None
              | _ -> Some message
            in
            let not_valid = if valid then None else Some () in
            deal_with_result ?not_valid ~actions:[action] state (fun () ->
                Client.transfer
                  state.client
                  ?sender:(Option.map parse_address sender)
                  ?source:(Option.map parse_address source)
                  ~dst:kt1
                  ?entry_point
                  ~amount:(Big_int.int_of_big_int amount)
                  ~arg:params) )
    | Verify {condition; line_no} ->
        State.log_advancement
          state
          "Verifying condition (l. %d)"
          (head_line_no line_no);
        compute
          ~state
          ~expression:condition
          ~config
          ~action
          (fun ~bug computed ->
            if computed <> "True"
            then bug "Verify Error"
            else Decorated_IO.return ())
    | Compute {var; expression; line_no} ->
        State.log_advancement
          state
          "Compute variable %s (l. %d)"
          var
          (head_line_no line_no);
        let value =
          let pp () =
            [ `Text "Computing expression"
            ; `Expr expression
            ; `Line expression.line_no ]
          in
          Interpreter.interpret_expr_external
            ~config
            ~primitives
            ~no_env:(pp ())
            ~scenario_state:state.smartml
            expression
        in
        Printf.ksprintf print_endline "compute tezos";
        compute ~state ~expression ~config ~action (fun ~bug:_ v ->
            Printf.ksprintf
              print_endline
              "compute store %s"
              (Printer.type_to_string expression.et);
            Hashtbl.replace state.compute_variables var v;
            Hashtbl.replace state.smartml.variables var value;
            Decorated_IO.return ())
    | ScenarioError _ | Exception _ | DynamicContract _ ->
        add_simple_history `Failure action
    | Html _ | Add_flag _ -> add_simple_history `Success action
    | Simulation _ | Show _ | Prepare_constant_value _ ->
        add_simple_history `None action

  let run {scenario = {actions}} state =
    Base.List.fold actions ~init:(return ()) ~f:(fun prevm (config, action) ->
        prevm
        >>= fun () ->
        try handle_action ~config state action with
        | e ->
            print_endline "EXCEPTION";
            raise e)
end
