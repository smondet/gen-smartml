(* Copyright 2019-2021 Smart Chain Arena LLC. *)

module Scenario_bak = Scenario
open SmartML
module Scenario = Scenario_bak
open Writer
open Utils
open Control
open Basics
open Typed
open Scenario
open Printf

type context =
  { primitives : (module Primitives.Primitives)
  ; html : bool
  ; buffer : Buffer.t
  ; output_dir : string option
  ; out : out_channel option
  ; errors : ([ `Error | `Warning ] * smart_except list) list ref
  ; install : string
  ; actions : (int * (Config.t * taction)) list }

type delayed_operation =
  { id : contract_id
  ; line_no : line_no
  ; title : string
  ; messageClass : string
  ; source : string option
  ; sender : string option
  ; chain_id : string option
  ; time : Bigint.t
  ; level : Bigint.t
  ; voting_powers : texpr
  ; scenario_state : scenario_state
  ; op : operation }

let noop _ = ()

let incremental = false

let appendIn {html; buffer} s =
  if html && incremental then SmartDom.addOutput s else bprintf buffer "%s\n" s

let appendOut {out} s =
  match out with
  | None -> ()
  | Some h -> fprintf h "%s\n" s

let appendError ctxt severity msg_out msg_full =
  ctxt.errors := (severity, msg_full) :: !(ctxt.errors);
  appendOut ctxt msg_out

let closeOut ctxt =
  if ctxt.html
  then
    if incremental
    then noop
    else
      fun () ->
      match ctxt.output_dir with
      | None -> SmartDom.setOutput (Buffer.contents ctxt.buffer)
      | Some od ->
          let name = "log" in
          let name, l =
            let html = Buffer.contents ctxt.buffer in
            let html = wrap_html_document ~install:ctxt.install html in
            write_html (sprintf "%s/%s" od name) html
          in
          appendOut ctxt (sprintf " => %s %d" name l)
  else (*fun () -> close_out h*) noop

let with_file ~config ~id ctxt step name w x =
  let module Printer = (val Printer.get config : Printer.Printer) in
  match ctxt.output_dir with
  | None -> ()
  | Some od ->
      let name =
        sprintf
          "%s/step_%03d_cont_%s_%s"
          od
          step
          (Printer.string_of_contract_id id)
          name
      in
      let name, l = w name x in
      appendOut ctxt (sprintf " => %s %d" name l)

let update_contract ?address scenario_state id contract =
  Hashtbl.replace scenario_state.contracts id contract;
  Interpreter.update_contract_address ?address scenario_state id

let update_constants scenario_state hash constant =
  Hashtbl.replace scenario_state.constants hash constant

let make_toc ~in_browser actions =
  let prep (step, (_, action)) =
    match action with
    | Html {tag; inner} ->
        let toc i = Some (i, step, inner) in
        ( match tag with
        | "h1" -> toc 1
        | "h2" -> toc 2
        | "h3" -> toc 3
        | "h4" -> toc 4
        | _ -> None )
    | _ -> None
  in
  let table_of_contents = List.map_some prep actions in
  let goto d d' l =
    let x = ref d in
    while !x <> d' do
      if !x < d'
      then (
        if in_browser then l := "<ul>" :: !l;
        incr x )
      else (
        if in_browser then l := "</ul>" :: !l;
        decr x )
    done
  in
  let d, table_of_contents =
    List.fold_left
      (fun (d, l) (d', id, s') ->
        let l = ref l in
        goto d d' l;
        let link =
          if in_browser
          then sprintf "<li><a href='#label%i'>%s</a>" id s'
          else sprintf "%s %s" (String.sub "\n####" 0 d') s'
        in
        (d', link :: !l))
      (1, [])
      table_of_contents
  in
  let table_of_contents = ref table_of_contents in
  goto d 1 table_of_contents;
  String.concat "" (List.rev !table_of_contents)

let parse_address ~config ~primitives ~scenario_state = function
  | Account x -> x.pkh
  | Address (address : Basics.Typed.texpr) ->
      let pp () =
        [`Text "Computing address"; `Expr address; `Line address.line_no]
      in
      Value.unAddress
        ~pp
        (Interpreter.interpret_expr_external
           ~config
           ~primitives
           ~no_env:(pp ())
           ~scenario_state
           address)

let handle_new_contract
    ?address
    ({primitives} as ctxt)
    scenario_state
    output_in
    step
    ~id
    ~line_no
    ~instance
    ~accept_unknown_types
    ~show =
  let config = (get_extra instance.template.tcontract.derived).config in
  let module Printer = (val Printer.get config : Printer.Printer) in
  let _appendIn = appendIn ctxt in
  let appendOut = appendOut ctxt in
  let appendError = appendError ctxt in
  let with_file name = with_file ~id ctxt step name in
  let contract = Reducer.reduce_instance ~primitives ~scenario_state instance in
  update_contract ?address scenario_state id contract;
  appendOut
    ("Creating contract " ^ Interpreter.get_contract_address scenario_state id);
  let scenario_vars = String.Map.of_hashtbl scenario_state.variables in
  let compiled_contract = Compiler.compile_instance ~scenario_vars contract in
  let compiled_contract =
    if config.simplify_via_michel
    then (
      let michel, michelson =
        Compiler.simplify_via_michel ~config compiled_contract
      in
      if config.dump_michel
      then with_file "pre_michelson" ~config write_contract_michel michel;
      michelson )
    else compiled_contract
  in
  appendOut
    ( " -> "
    ^ Base.Option.value_map
        compiled_contract.storage
        ~default:"missing storage"
        ~f:Michelson.string_of_tliteral );
  ( if ctxt.html && show
  then
    let def =
      match config.languages with
      | x :: _ -> Config.show_language x
      | [] -> "Types"
    in
    Html.full_html
      ~config
      ~contract
      ~compiled_contract
      ~def
      ~onlyDefault:false
      ~id:(sprintf "%s_%d" (Printer.string_of_contract_id id) step)
      ~line_no
      ~accept_missings:accept_unknown_types
      ~contract_id:(Some id)
    |> Html.render
    |> fun x -> output_in := x :: !output_in );
  let contract_micheline = Michelson.to_micheline_tcontract compiled_contract in
  let storage_with_micheline =
    Option.map
      (fun x ->
        (x, Michelson.(To_micheline.literal (Michelson.erase_types_literal x))))
      compiled_contract.storage
  in
  if Option.is_some ctxt.output_dir
  then begin
    begin
      match storage_with_micheline with
      | Some (storage, storage_micheline) ->
          let name = "storage" in
          with_file
            name
            write_mliteral
            ~config
            (Michelson.erase_types_literal storage);
          with_file name write_micheline ~config storage_micheline
      | None -> ()
    end;
    let sizes =
      [ ("storage", Option.map snd storage_with_micheline)
      ; ("contract", Some contract_micheline) ]
    in
    let get_size (name, x) =
      try
        [ name
        ; Option.fold
            ~none:"missing"
            ~some:(fun x ->
              Printf.sprintf "%i" (Micheline_encoding.micheline_size x))
            x ]
      with
      | Failure s -> [name; s]
    in
    let name = "sizes" in
    with_file ~config name write_csv (List.map get_size sizes);
    ( match contract.state.storage with
    | None -> ()
    | Some storage ->
        let name = "storage" in
        with_file ~config name write_tvalue storage );
    let name = "types" in
    with_file ~config name write_contract_types contract;
    let write_metadata_file (name, metadata) =
      with_file ~config ("metadata." ^ name) write_metadata metadata
    in
    List.iter
      write_metadata_file
      (Metadata.for_contract ~config contract compiled_contract);
    begin
      match
        Michelson.has_error_tcontract
          ~accept_missings:accept_unknown_types
          compiled_contract
      with
      | [] -> ()
      | _ ->
          appendError
            `Warning
            "Error in generated contract"
            [`Text "Error in generated Michelson contract"]
    end;
    ( match contract.template.tcontract.unknown_parts with
    | Some msg when not accept_unknown_types ->
        ksprintf
          (appendError `Warning)
          "Warning: unknown types or type errors: %s"
          msg
          [`Text "Error (unknown) in generated Michelson contract"]
    | _ -> () );
    let name = "contract" in
    with_file ~config name write_contract_michelson compiled_contract;
    with_file ~config name write_micheline contract_micheline;
    List.iter
      (fun language ->
        with_file ~config name (write_pretty ~language) contract;
        if ctxt.html
        then
          with_file
            ~config
            name
            (write_pretty_html ~language ~install:ctxt.install)
            contract)
      config.languages;
    if config.decompile
    then (
      let contract_michel =
        let st = Michel.Transformer.{var_counter = ref 0} in
        let c = Michel_decompiler.decompile_contract st compiled_contract in
        Michel.Transformer.smartMLify st c
      in
      with_file ~config "pre_smartml" write_contract_michel contract_michel;
      let decompiled =
        match Michel.Typing.typecheck_precontract contract_michel with
        | Error msg -> raise (SmartExcept [`Text msg])
        | Ok c -> Decompiler.smartML_of_michel config c
      in
      let suffix =
        {|

@sp.add_test(name = "Test")
def test():
    s = sp.test_scenario()
    s += Contract()
|}
      in
      with_file
        ~config
        "decompiled"
        (write_pretty ~language:SmartPy ~suffix)
        decompiled )
  end

let handle_set_delegate ~config scenario_state ~id ~line_no ~baker =
  let module Printer = (val Printer.get config : Printer.Printer) in
  let {template; state} =
    match Hashtbl.find_opt scenario_state.contracts id with
    | None ->
        raise
          (SmartExcept
             [ `Text
                 (sprintf
                    "Missing contract in scenario %s"
                    (Printer.string_of_contract_id id))
             ; `Line line_no ])
    | Some contract -> contract
  in
  update_contract scenario_state id {template; state = {state with baker}}

let handle_message
    ~config
    ~queue
    ~reverted
    ({primitives} as ctxt)
    output_in
    scenario_state
    step
    ~id
    ~params
    ~params_expr
    ~line_no
    ~title
    ~messageClass
    ~sender
    ~source
    ~chain_id
    ~time
    ~amount
    ~level
    ~(voting_powers : texpr)
    ~message
    ~show
    ~export =
  let _appendIn = appendIn ctxt in
  let appendOut = appendOut ctxt in
  let _appendError = appendError ctxt in
  let with_file = with_file ctxt step in
  let voting_powers_expr = voting_powers in
  let module Printer = (val Printer.get config : Printer.Printer) in
  let voting_powers =
    let pp () =
      [ `Text "Computing voting powers"
      ; `Expr voting_powers
      ; `Line voting_powers.line_no ]
    in
    List.map
      (fun (k, v) -> (Value.unKey_hash ~pp k, Value.unInt ~pp v))
      (Value.unMap
         ~pp
         (Interpreter.interpret_expr_external
            ~config
            ~primitives
            ~no_env:(pp ())
            ~scenario_state
            voting_powers))
  in
  let result =
    Contract.execMessageInner
      ~config
      ~primitives
      ~scenario_state
      ~title
      ~execMessageClass:
        (if messageClass <> "" then " " ^ messageClass else messageClass)
      ~context:
        (Interpreter.build_context
           ?sender
           ?source
           ?chain_id
           ~time
           ~amount
           ~level
           ~voting_powers
           ~line_no
           ~debug:false
           ())
      ~channel:message
      ~id
      ~params
      ~line_no
  in
  if export
  then begin
    let name = "params" in
    with_file ~config ~id name write_tvalue params;
    let scenario_vars = String.Map.of_hashtbl scenario_state.variables in
    let params = Compiler.compile_value ~config ~scenario_vars params in
    with_file ~config ~id name write_mliteral params;
    let params = Michelson.To_micheline.literal params in
    with_file ~config ~id name write_micheline params
  end;
  ( match params_expr with
  | Some params_expr ->
      appendOut
        (sprintf
           "Executing %s(%s)..."
           message
           (Printer.texpr_to_string params_expr))
  | None ->
      appendOut
        (sprintf
           "Executing (queue) %s(%s)..."
           message
           (Printer.value_to_string params)) );
  if show then output_in := result.html :: !output_in;
  match result.error with
  | None ->
      let contract =
        Base.Option.value_exn ~message:"No contract" result.contract
      in
      update_contract scenario_state id contract;
      let storage =
        let scenario_vars = String.Map.of_hashtbl scenario_state.variables in
        Option.cata
          "missing storage"
          (fun x ->
            Michelson.string_of_literal
              (Compiler.compile_value ~config ~scenario_vars x))
          contract.state.storage
      in
      appendOut (sprintf " -> %s" storage);
      let sender = Some (Interpreter.get_contract_address scenario_state id) in
      let update_balance = function
        | Transfer {amount} ->
            let balance =
              Big_int.sub_big_int
                (Hashtbl.find scenario_state.contracts id).state.balance
                amount
            in
            if Big_int.compare_big_int balance Big_int.zero_big_int < 0
            then raise (SmartExcept [`Text "Balance < 0"; `Line line_no]);
            update_contract
              scenario_state
              id
              { template = contract.template
              ; state = {contract.state with balance} }
        | _ -> ()
      in
      List.iter update_balance result.operations;
      let f op : delayed_operation =
        appendOut ("  + " ^ Printer.operation_to_string op);
        { id
        ; line_no
        ; title
        ; messageClass
        ; source
        ; sender
        ; chain_id
        ; time
        ; level
        ; voting_powers = voting_powers_expr
        ; scenario_state
        ; op }
      in
      let todo = List.map f result.operations in
      let depth_first =
        match config.protocol with
        | Delphi | Edo -> false
        | Florence | Granada | Hangzhou | Ithaca -> true
      in
      queue := if depth_first then todo @ !queue else !queue @ todo
  | Some error -> reverted := Some error

let handle_action
    ~config
    ~queue
    ~reverted
    ~output_in
    ({primitives} as ctxt)
    scenario_state
    (step, action) =
  let module Printer = (val Printer.get config : Printer.Printer) in
  let appendIn = appendIn ctxt in
  let appendOut = appendOut ctxt in
  let appendError = appendError ctxt in
  let scenario_vars = String.Map.of_hashtbl scenario_state.variables in
  match action with
  | New_contract {id; contract; accept_unknown_types; line_no; show; address} ->
      let {config} = get_extra contract.derived in
      let instance =
        Interpreter.interpret_contract
          ~config
          ~primitives
          ~scenario_state
          {tcontract = contract}
      in
      handle_new_contract
        ctxt
        scenario_state
        output_in
        step
        ~id
        ~line_no
        ~instance
        ~accept_unknown_types
        ~show
        ~address
  | Compute {var; expression; line_no} ->
      let value =
        Interpreter.interpret_expr_external
          ~config
          ~primitives
          ~no_env:[`Text "Computing expression"; `Expr expression; `Line line_no]
          ~scenario_state
          expression
      in
      Hashtbl.replace scenario_state.variables var value
  | Simulation {id; line_no} ->
    ( match Hashtbl.find_opt scenario_state.contracts id with
    | None -> assert false
    | Some contract ->
        Html.simulation contract step ~line_no |> Html.render |> appendIn )
  | Set_delegate {id; line_no; baker} ->
      let baker =
        Value.un_baker
          (Interpreter.interpret_expr_external
             ~config
             ~primitives
             ~no_env:[`Text "Computing baker"; `Expr baker; `Line baker.line_no]
             ~scenario_state
             baker)
      in
      handle_set_delegate ~config scenario_state ~id ~line_no ~baker
  | Message
      { id
      ; params
      ; line_no
      ; title
      ; messageClass
      ; sender
      ; source
      ; chain_id
      ; time
      ; amount
      ; level
      ; voting_powers
      ; message
      ; show
      ; export } ->
      let time =
        match time with
        | None -> scenario_state.time
        | Some time ->
            let pp () =
              [`Text "Computing now"; `Expr time; `Line time.line_no]
            in
            Value.unTimestamp
              ~pp
              (Interpreter.interpret_expr_external
                 ~config
                 ~primitives
                 ~no_env:(pp ())
                 ~scenario_state
                 time)
      in
      scenario_state.time <- time;
      let level =
        match level with
        | None -> scenario_state.level
        | Some level ->
            let pp () =
              [`Text "Computing level"; `Expr level; `Line level.line_no]
            in
            Value.unInt
              ~pp
              (Interpreter.interpret_expr_external
                 ~config
                 ~primitives
                 ~no_env:(pp ())
                 ~scenario_state
                 level)
      in
      scenario_state.level <- level;
      let sender =
        Option.map (parse_address ~config ~primitives ~scenario_state) sender
      in
      let source =
        match
          Option.map (parse_address ~config ~primitives ~scenario_state) source
        with
        | None -> sender
        | source -> source
      in
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
      let params_value =
        Interpreter.interpret_expr_external
          ~config
          ~primitives
          ~no_env:[`Text "Computing params"; `Expr params; `Line params.line_no]
          ~scenario_state
          params
      in
      let parse_chain_id (chain_id : Basics.Typed.texpr) =
        let pp () =
          [`Text "Computing chain_id"; `Expr chain_id; `Line chain_id.line_no]
        in
        Value.unChain_id
          ~pp
          (Interpreter.interpret_expr_external
             ~config
             ~primitives
             ~no_env:(pp ())
             ~scenario_state
             chain_id)
      in
      handle_message
        ~config
        ~queue
        ~reverted
        ctxt
        output_in
        scenario_state
        step
        ~id
        ~params:params_value
        ~params_expr:(Some params)
        ~line_no
        ~title
        ~messageClass
        ~sender
        ~source
        ~chain_id:(Option.map parse_chain_id chain_id)
        ~time
        ~amount
        ~level
        ~voting_powers
        ~message
        ~show
        ~export
  | Exception exn -> appendError `Error (Printer.pp_smart_except false exn) exn
  | ScenarioError {message} ->
      appendError
        `Error
        (sprintf " !!! Python Error: %s" message)
        [`Text "Python Error"; `Text message]
  | Html {tag; inner} ->
      if inner = "[[TABLEOFCONTENTS]]"
      then begin
        appendOut "Table Of Contents";
        let table_of_contents = make_toc ~in_browser:false ctxt.actions in
        let table_of_contents_html = make_toc ~in_browser:true ctxt.actions in
        appendOut table_of_contents;
        appendIn (sprintf "<%s>%s</%s>" tag table_of_contents_html tag)
      end
      else begin
        ( match tag with
        | "h1" | "h2" | "h3" | "h4" ->
            appendIn (sprintf "<span id='label%i'></span>" step)
        | _ -> () );
        appendOut "Comment...";
        appendOut (sprintf " %s: %s" tag inner);
        appendIn (sprintf "<%s>%s</%s>" tag inner tag)
      end
  | Verify {condition; line_no} ->
      appendOut (sprintf "Verifying %s..." (Printer.texpr_to_string condition));
      let value =
        Interpreter.interpret_expr_external
          ~config
          ~primitives
          ~no_env:
            [ `Text "Computing condition"
            ; `Expr condition
            ; `Line condition.line_no ]
          ~scenario_state
          condition
      in
      let result = Value.bool_of_value value in
      if result
      then appendOut " OK"
      else begin
        appendIn
          (sprintf
             "Verification Error: <br>%s<br> is false."
             (Printer.texpr_to_string condition));
        appendError
          `Error
          " KO"
          [ `Text "Verification Error"
          ; `Br
          ; `Expr condition
          ; `Br
          ; `Text "is false"
          ; `Line line_no ]
      end
  | Show {expression; html; stripStrings; compile} ->
      appendOut (sprintf "Computing %s..." (Printer.texpr_to_string expression));
      let value =
        Interpreter.interpret_expr_external
          ~config
          ~primitives
          ~no_env:
            [ `Text "Computing expression"
            ; `Expr expression
            ; `Line expression.line_no ]
          ~scenario_state
          expression
      in
      let scenario_vars = String.Map.of_hashtbl scenario_state.variables in
      if compile
      then begin
        let name = "expression" in
        let expression = Compiler.compile_value ~config ~scenario_vars value in
        let expression_micheline = Michelson.To_micheline.literal expression in
        let with_file name w x =
          match ctxt.output_dir with
          | None -> ()
          | Some od ->
              let name = sprintf "%s/step_%03d_%s" od step name in
              let name, l = w name x in
              appendOut (sprintf " => %s %d" name l)
        in
        with_file name write_tvalue value;
        with_file name write_mliteral expression;
        with_file name write_micheline expression_micheline
      end;
      let result =
        Printer.value_to_string ~options:Printer.Options.string value
      in
      let options =
        if html
        then
          if stripStrings
          then Printer.Options.htmlStripStrings
          else Printer.Options.html
        else Printer.Options.string
      in
      ( match expression.e with
      | EPrim0 (EConstantVar id) ->
        ( match String.Map.find_opt id scenario_vars with
        | Some hash ->
            appendIn
              (sprintf
                 "<div class='execMessage execMessageOK' style='margin-bottom: \
                  1rem;'><h3>Constant: <span \
                  class='keyword'>%s</span><h3>Value: <div \
                  class='subtype'>%s</div></div>"
                 (Printer.value_to_string ~options hash)
                 (Printer.value_to_string ~options value))
        | None -> () )
      | _ ->
          appendIn
            (sprintf
               "<div class='execMessage'>%s</div>"
               (Printer.value_to_string ~options value)) );
      appendOut (sprintf " => %s" result)
  | DynamicContract {id; model_id; line_no} ->
      let lookup id =
        match Hashtbl.find_opt scenario_state.contracts id with
        | Some c ->
          ( match c.template.tcontract.template_id with
          | None ->
              raise (SmartExcept [`Text "Missing template id"; `Line line_no])
          | Some cid -> cid.static_id )
        | None ->
            raise
              (SmartExcept
                 [ `Text "Contract id"
                 ; `Text (show_contract_id id)
                 ; `Text "not found"
                 ; `Line line_no ])
      in
      let dynamic = lookup (C_dynamic id) in
      let model = lookup model_id in
      if dynamic <> model
      then
        raise
          (SmartExcept
             [ `Text "Expected dynamic contract from template"
             ; `Text (string_of_int model)
             ; `Text ", got"
             ; `Text (string_of_int dynamic)
             ; `Line line_no ])
  | Add_flag _ -> () (* Handled in Scenario.acc_config. *)
  | Prepare_constant_value {var; hash; expression} ->
      let value =
        Interpreter.interpret_expr_external
          ~config
          ~primitives
          ~no_env:
            [ `Text "Computing expression"
            ; `Expr expression
            ; `Line expression.line_no ]
          ~scenario_state
          expression
      in
      let hash =
        match hash with
        | Some hash -> hash
        | None ->
            let primitives = primitives in
            let module P = (val primitives : Primitives.Primitives) in
            let interpreted_value =
              Interpreter.interpret_expr_external
                ~config
                ~primitives
                ~no_env:
                  [ `Text "Preparing constant value"
                  ; `Expr expression
                  ; `Line expression.line_no ]
                ~scenario_state
                expression
            in
            (*
          Steps:
          --------------
          1. Pack micheline value
          2. Chop the prefix that idenfies the value as packed ("0x05...")
          3. Hash the bytes with black2b (32 bytes)
          4. Encode the hash with base58 ("expr" prefixed)
        *)
            ( try
                interpreted_value
                |> Compiler.pack_value ~config ~scenario_vars
                |> Base.String.chop_prefix_exn
                     ~prefix:Micheline_encoding.pack_prefix
                |> P.Crypto.blake2b
                |> Misc.Hex.hexcape
                |> Bs58.encode_expr
              with
            | Data_encoding.Binary.Write_error write_error ->
                raise
                  (ExecFailure
                     ( Value.string "Pack error when preparing constant value"
                     , [ `Text
                           "Could not prepare constant due to invalid content"
                       ; `Br
                       ; `Expr expression
                       ; `Br
                       ; `Value interpreted_value
                       ; `Text
                           (Format.asprintf
                              "%a"
                              Data_encoding.Binary.pp_write_error
                              write_error)
                       ; `Line expression.line_no ] )) )
      in
      (* Update table of constants *)
      update_constants scenario_state hash value;
      (* Register a variable that points to the generated hash *)
      Hashtbl.replace scenario_state.variables var (Value.string hash)

let run_operation
    ~id
    ~line_no
    ~title
    ~messageClass
    ~source
    ~sender
    ~chain_id
    ~time
    ~level
    ~voting_powers
    ~scenario_state
    ~queue
    ~reverted
    ~output_in
    ~config
    ctxt
    state
    step = function
  | Transfer {params; destination = {address; entry_point}; amount} ->
    ( match Hashtbl.find_opt scenario_state.rev_addresses address with
    | None -> ()
    | Some id ->
        let module Printer = (val Printer.get config : Printer.Printer) in
        let message = Base.Option.value entry_point ~default:"default" in
        let show_arg = Printer.value_to_string params in
        let arg_shown =
          let the_max = 20 in
          if String.length show_arg > the_max
          then Base.String.prefix show_arg the_max ^ "..."
          else show_arg
        in
        let title =
          sprintf
            "Follow-up-transfer: %s (%s)%s"
            (Aux.address_of_contract_id ~html:false id (Some message))
            arg_shown
            (if title = "" then "" else sprintf " (%s)" title)
        in
        let show = true in
        let export = false in
        handle_message
          ~config
          ~queue
          ~reverted
          ctxt
          output_in
          state
          step
          ~id
          ~params
          ~params_expr:None
          ~line_no
          ~title
          ~messageClass
          ~sender
          ~source
          ~chain_id
          ~time
          ~amount
          ~level
          ~voting_powers
          ~message
          ~show
          ~export )
  | SetDelegate baker -> handle_set_delegate ~config state ~id ~line_no ~baker
  | CreateContract {id; instance} ->
      handle_new_contract
        ctxt
        state
        output_in
        step
        ~id
        ~line_no
        ~instance
        ~accept_unknown_types:false
        ~show:true
        ?address:None

let run ({primitives} as ctxt) language initial_global_state =
  let appendIn = appendIn ctxt in
  let appendOut = appendOut ctxt in
  let appendError = appendError ctxt in
  Hashtbl.clear Html.simulatedContracts;
  let module Printer = (val Printer.get_by_language language : Printer.Printer)
  in
  ( try
      let check_valid reverted scenario_state config action =
        let to_check =
          match action with
          | Message {message; valid; exception_; line_no} ->
              Some (Some valid, exception_, line_no, `Text message)
          | _ -> None
        in
        match to_check with
        | None -> `OK
        | Some (valid, (exception_ : texpr option), line_no, message) ->
            let exception_ =
              Option.map
                (fun exception_ ->
                  let pp () =
                    [ `Text "Computing exception"
                    ; `Expr exception_
                    ; `Line line_no ]
                  in
                  Interpreter.interpret_expr_external
                    ~config
                    ~primitives
                    ~no_env:(pp ())
                    ~scenario_state
                    exception_)
                exception_
            in
            let valid =
              match valid with
              | None -> false
              | Some valid ->
                  let pp () =
                    [`Text "Computing valid"; `Expr valid; `Line line_no]
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
            ( match (reverted, valid, exception_) with
            | None, true, None -> `OK
            | None, true, Some exception_ ->
                appendError
                  `Error
                  (sprintf
                     " -> !!! Unexpected Exception declaration in valid \
                      operation !!!")
                  [ `Text
                      "Unexpected Exception declaration in valid operation, \
                       please remove the exception declaration."
                  ; `Br
                  ; `Value exception_
                  ; `Br
                  ; `Line line_no ];
                `Error "Reverted transaction - unexpected error"
            | None, false, _ ->
                appendError
                  `Error
                  (sprintf " -> !!! Valid but expected ERROR !!!")
                  [ `Text "Expected error in transaction but valid."
                  ; `Br
                  ; message (*; `Expr params*)
                  ; `Br
                  ; `Line line_no ];
                `OK_but "Valid but expected ERROR"
            | Some error, true, _ ->
                appendError
                  `Error
                  (sprintf
                     " -> !!! Unexpected ERROR !!! %s"
                     (Printer.error_to_string error))
                  [ `Text
                      "Unexpected error in transaction, please use \
                       .run(valid=False, ..)"
                  ; `Br
                  ; `Rec (Execution.to_smart_except error)
                  ; `Br
                  ; message (* ; `Expr params *)
                  ; `Br
                  ; `Line line_no ];
                `Error "Reverted transaction - unexpected error"
            | Some error, false, exception_ ->
              begin
                match exception_ with
                | None ->
                    appendOut
                      (sprintf
                         " -> --- Expected failure in transaction --- %s"
                         (Printer.error_to_string error));
                    `Error "Reverted transaction"
                | Some exception_ ->
                    let except =
                      match error with
                      | Exec_failure (except, _) -> except
                    in
                    if Printer.value_to_string except
                       = Printer.value_to_string exception_
                    then begin
                      appendOut
                        (sprintf
                           " -> --- Expected failure in transaction --- %s"
                           (Printer.error_to_string error));
                      `Error "Reverted transaction"
                    end
                    else begin
                      appendError
                        `Error
                        (sprintf
                           " -> !!! Wrong or unsupported exception matching \
                            !!! Expected failure in transaction --- %s"
                           (Printer.error_to_string error))
                        [ `Text
                            "Wrong or unsupported exception matching (expected \
                             failure in transaction)"
                        ; `Br
                        ; `Rec (Execution.to_smart_except error)
                        ; `Br
                        ; `Text "Received:"
                        ; `Value except
                        ; `Br
                        ; `Text "Expected:"
                        ; `Value exception_
                        ; `Br
                        ; `Line line_no ];
                      `Error
                        "Reverted transaction - wrong or unsupported exception \
                         matching"
                    end
              end )
      in
      let global_state = ref initial_global_state in
      List.iter
        (fun (step, (config, x)) ->
          let reverted = ref None in
          let queue = ref [] in
          let new_state = Basics.copy_scenario_state !global_state in
          let output_in = ref [] in
          handle_action
            ~config
            ~queue
            ~reverted
            ~output_in
            ctxt
            new_state
            (step, x);
          while
            match !queue with
            | [] -> false
            | { id
              ; line_no
              ; title
              ; messageClass
              ; source
              ; sender
              ; chain_id
              ; time
              ; level
              ; voting_powers
              ; scenario_state
              ; op }
              :: rest ->
                queue := rest;
                run_operation
                  ~id
                  ~line_no
                  ~title
                  ~messageClass
                  ~source
                  ~sender
                  ~chain_id
                  ~time
                  ~level
                  ~voting_powers
                  ~scenario_state
                  ~queue
                  ~reverted
                  ~output_in
                  ~config
                  ctxt
                  new_state
                  step
                  op;
                true
          do
            ()
          done;
          let multiple = List.length !output_in > 1 in
          let output_in = String.concat "\n" (List.rev !output_in) in
          let pp msg =
            appendIn
              (Printf.sprintf
                 "<div style='border: double; padding: 5px; margin-bottom: \
                  10px'>%s</br>%s</div>"
                 msg
                 output_in)
          in
          match check_valid !reverted !global_state config x with
          | `OK ->
              global_state := new_state;
              if multiple then pp "Multiple operations" else appendIn output_in
          | `OK_but msg ->
              global_state := new_state;
              pp msg
          | `Error msg -> pp msg)
        ctxt.actions
    with
  | exn ->
      let except = Printer.exception_to_smart_except exn in
      let s = Printer.pp_smart_except ctxt.html except in
      appendError `Error (" (Exception) " ^ s) except );
  closeOut ctxt ();
  List.rev !(ctxt.errors)

let run ~primitives ~html ~install ~scenario:(s : loaded_scenario) output_dir =
  let actions = List.mapi pair s.scenario.actions in
  run
    { primitives
    ; html
    ; buffer = Buffer.create 111024
    ; output_dir
    ; out =
        ( match output_dir with
        | None -> None
        | Some od -> Some (open_out (Filename.concat od "log.txt")) )
    ; errors = ref (List.map (fun x -> (`Warning, x)) s.warnings)
    ; install
    ; actions }
    s.language
    s.scenario_state

let run_scenario_browser ~primitives ~scenario config =
  let module Printer = (val Printer.get config : Printer.Printer) in
  let scenario =
    try
      load_from_string ~primitives config (Yojson.Basic.from_string scenario)
    with
    | exn -> failwith (Printer.exception_to_string true exn)
  in
  let errors = run ~primitives ~html:true ~install:"static" ~scenario None in
  match errors with
  | [] -> ()
  | l ->
      raise
        (SmartExcept
           [ `Text "Error in Scenario"
           ; `Br
           ; `Rec (List.concat (List.map (fun (_, l) -> [`Rec l; `Br]) l)) ])

let run_all_scenarios_browser ~primitives ~scenario config =
  let module Printer = (val Printer.get config : Printer.Printer) in
  let run_scenario scenario =
    let scenario =
      try load_from_string ~primitives config scenario with
      | exn -> failwith (Printer.exception_to_string true exn)
    in
    let errors = run ~primitives ~html:true ~install:"static" ~scenario None in
    match errors with
    | [] -> ()
    | l ->
        raise
          (SmartExcept
             [ `Text "Error in Scenario"
             ; `Br
             ; `Rec (List.concat (List.map (fun (_, l) -> [`Rec l; `Br]) l)) ])
  in
  SmartDom.setOutput "";
  match Yojson.Basic.from_string scenario with
  | `List l -> List.iter run_scenario l
  | _ -> assert false
