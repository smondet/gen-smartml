(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML
open Utils
open Control
open Michelson
open Primitives

type maddress = string [@@deriving show {with_path = false}]

type 'v mvalue_f =
  | Unit
  | Bool      of bool
  | Int       of tezos_int
  | String    of string
  | Operation of 'v moperation
  | Address   of maddress
  | Pair      of 'v * 'v
  | None_
  | Some_     of 'v
  | Left      of 'v
  | Right     of 'v
  | List      of 'v list
  | Set       of 'v list (* TODO Set intsead of list? *)
  | Map       of ('v * 'v) list
  | Closure   of tinstr * 'v list

and 'v moperation =
  | Transfer_tokens of mtype * 'v * Bigint.t * maddress
  | Set_delegate    of string option
  | Create_contract of
      { code : instr
      ; balance : Bigint.t
      ; baker : string option
      ; tparameter : mtype
      ; tstorage : mtype
      ; storage : 'v }
[@@deriving show {with_path = false}, map]

type mvalue = {mv : mvalue mvalue_f} [@@deriving show {with_path = false}]

let rec cata_mvalue f {mv} = f (map_mvalue_f (cata_mvalue f) mv)

type tmvalue =
  { t : mtype
  ; tv : tmvalue mvalue_f }
[@@deriving show {with_path = false}]

let rec cata_tmvalue f {t; tv} = f t (map_mvalue_f (cata_tmvalue f) tv)

let unit = {mv = Unit}

let bool x = {mv = Bool x}

let int x = {mv = Int x}

let string x = {mv = String x}

let operation x = {mv = Operation x}

let address x = {mv = Address x}

let pair x y = {mv = Pair (x, y)}

let none = {mv = None_}

let some x = {mv = Some_ x}

let left x = {mv = Left x}

let right x = {mv = Right x}

let list xs = {mv = List xs}

let set xs = {mv = Set xs}

let mk_map xs = {mv = Map xs}

let closure f xs = {mv = Closure (f, xs)}

let rec mvalue_of_tliteral x =
  match x.tliteral with
  | Int x -> int x
  | Bool x -> bool x
  | String x | Bytes x -> string x
  | Unit -> unit
  | Pair (x, y) -> pair (mvalue_of_tliteral x) (mvalue_of_tliteral y)
  | None_ -> none
  | Left x -> left (mvalue_of_tliteral x)
  | Right y -> right (mvalue_of_tliteral y)
  | Some_ x -> some (mvalue_of_tliteral x)
  | Seq xs ->
    ( match x.t with
    | Ok {mt = MT1 (T_list, _)} -> list (List.map mvalue_of_tliteral xs)
    | Ok {mt = MT1 (T_set, _)} -> set (List.map mvalue_of_tliteral xs)
    | _ -> assert false )
  | AnyMap xs ->
      let f (k, v) = (mvalue_of_tliteral k, mvalue_of_tliteral v) in
      mk_map (List.map f xs)
  | Instr i -> closure i []
  | Elt _ -> assert false
  | Constant _ -> assert false

let mvalue_of_tvalue_f t =
  let open Basics in
  function
  | Literal l | Bounded (_, l) ->
      let l = Compiler.compile_literal l in
      let tparameter = (mt_unit, None) (* TODO *) in
      let t = Typing.mtype_of_type ~with_annots:false t in
      let l = Michelson.typecheck_literal ~strict_dup:true ~tparameter t l in
      mvalue_of_tliteral l
  | Contract _ -> failwith "Michelson_interpreter TODO: Contract"
  | Record (layout, entries) ->
      Binary_tree.cata snd pair (Compiler.compile_record ~layout entries)
  | Variant (layout, row, name, x) ->
    ( match name with
    | "None" -> none
    | "Some" -> some x
    | "Left" -> left x
    | "Right" -> right x
    | _ ->
        let ctxt = Compiler.compile_variant ~row ~layout name in
        let l x _ = left x in
        let r _ x = right x in
        Binary_tree.context_cata x l r ctxt )
  | List (_, xs) -> {mv = List xs}
  | Ticket _ -> failwith "Michelson_interpreter TODO: ticket"
  | Set (_, xs) -> set xs
  | Map (_, _, _, xs) -> mk_map xs
  | Tuple xs -> Compiler.compile_tuple pair xs
  | Operation _ -> failwith "Michelson_interpreter TODO: operation"
  | Closure _ -> failwith "Michelson_interpreter TODO: closure"

let mvalue_of_value t v =
  let v = Value.typecheck t v in
  Basics.cata_tvalue mvalue_of_tvalue_f v

let typecheck_mvalue_f (x : (mtype -> tmvalue) mvalue_f) t =
  let tv =
    match (t.mt, x) with
    | MT0 (T_int | T_nat | T_mutez | T_timestamp), (Int _ as tv) ->
        tv (* TODO check positivy for nat *)
    | MT0 T_bool, (Bool _ as tv) -> tv
    | MT0 (T_bytes | T_string), (String _ as tv) -> tv
    | MT0 T_unit, (Unit as tv) -> tv
    | MT2 (T_pair _, tx, ty), Pair (x, y) -> Pair (x tx, y ty)
    | MT1 (T_option, _), (None_ as tv) -> tv
    | MT2 (T_or _, tx, _), Left x -> Left (x tx)
    | MT2 (T_or _, _, ty), Right y -> Right (y ty)
    | MT1 (T_option, t), Some_ x -> Some_ (x t)
    | MT1 (T_list, t), List xs -> List (List.map (fun x -> x t) xs)
    | MT1 (T_set, t), Set xs -> Set (List.map (fun x -> x t) xs)
    | MT2 (T_map, tk, tv), Map xs ->
        Map (List.map (fun (k, v) -> (k tk, v tv)) xs)
    | MT2 (T_big_map, tk, tv), Map xs ->
        Map (List.map (fun (k, v) -> (k tk, v tv)) xs)
    | MT0 T_address, (Address _ as tv) -> tv
    | MT0 T_operation, Operation op ->
        let op =
          match op with
          | Transfer_tokens (t, x, amount, dest) ->
              let x = x t in
              Transfer_tokens (t, x, amount, dest)
          | Create_contract {balance; baker; tparameter; tstorage; storage; code}
            ->
              let storage = storage tstorage in
              Create_contract
                {balance; baker; tparameter; tstorage; storage; code}
          | Set_delegate _ as x -> x
        in
        Operation op
    | MT2 (T_lambda, _, _), Closure _ ->
        failwith "Michelson_interpreter TODO: lambdas"
    | ( _
      , ( Int _ | Bool _ | String _ | Unit | Pair _ | None_ | Left _ | Right _
        | Some_ _ | List _ | Set _ | Map _ | Closure _ | Operation _ | Address _
          ) ) ->
        failwith ("typecheck_mvalue: " ^ show_mtype t)
  in
  {t; tv}

let typecheck_mvalue t x = cata_mvalue typecheck_mvalue_f x t

let operation_of_moperation ~config op =
  let open Basics in
  match op with
  | Transfer_tokens _ -> assert false (* TODO *)
  | Set_delegate d -> SetDelegate d
  | Create_contract {balance; baker; tparameter; tstorage; code; storage} ->
      let open Basics in
      let id =
        C_dynamic {dynamic_id = 1001}
        (* FIXME use counter from interpreter *)
      in
      let storage = Some storage in
      let state =
        {balance; baker; storage; lazy_entry_points = []; metadata = []}
      in
      let parsed = Micheline.sequence (To_micheline.instruction code) in
      let im : Type.t inline_michelson =
        { name = "inlined"
        ; parsed
        ; typesIn = [Type.of_mtype (mt_pair tparameter tstorage)]
        ; typesOut = [Type.of_mtype (mt_pair (mt_list mt_operation) tstorage)]
        }
      in
      let body =
        let line_no = [] in
        let params = Expr.params ~line_no in
        let storage = Expr.local ~line_no "__storage__" in
        let code =
          Expr.inline_michelson
            ~line_no
            im
            [Expr.tuple ~line_no [params; storage]]
        in
        let _ =
          Command.mk_match_product
            ~line_no
            code
            (Pattern_tuple ["ops"; "new_storage"])
            (Command.seq
               ~line_no
               [ Command.set ~line_no storage (Expr.local ~line_no "new_storage")
               ; Command.set
                   ~line_no
                   (Expr.operations ~line_no)
                   (Expr.local ~line_no "ops") ])
        in
        Command.seq ~line_no []
      in
      let ep =
        { channel = "main"
        ; tparameter_ep = `Absent
        ; originate = true
        ; lazify = None
        ; lazy_no_code = None
        ; line_no = []
        ; body
        ; tparameter_ep_derived = U }
      in
      let contract =
        { template_id = None
        ; balance = None
        ; storage = None
        ; baker = None
        ; tstorage_explicit = None
        ; entry_points = [ep]
        ; entry_points_layout =
            Some (Layout.leaf "main" "main")
            (* TODO Why does putting 'None' here yield an error? *)
        ; unknown_parts = None
        ; flags = []
        ; private_variables = []
        ; metadata = []
        ; views = []
        ; derived = U }
      in
      let template = Checker.check_contract config {contract} in
      let instance = {template; state} in
      CreateContract {id; instance}

let rec value_of_mvalue config (t : Type.t) (x : mvalue) : Value.t =
  let of_mvalue = value_of_mvalue config in
  match (Type.unF t, x.mv) with
  | TInt {isNat}, Int x when Unknown.get isNat = Some false -> Value.int x
  | TInt {isNat}, Int x when Unknown.get isNat = Some true -> Value.nat x
  | T0 T_int, Int x -> Value.int x
  | T0 T_nat, Int x -> Value.nat x
  | T0 T_mutez, Int x -> Value.mutez x
  | T0 T_timestamp, Int x -> Value.timestamp x
  | T0 T_bool, Bool x -> Value.bool x
  | T0 T_bytes, String x -> Value.bytes x
  | T0 T_string, String x -> Value.string x
  | T0 T_unit, Unit -> Value.unit
  | T2 (T_pair _, tx, ty), Pair (x, y) ->
      Value.tuple [of_mvalue tx x; of_mvalue ty y]
  | T1 (T_option, t), None_ -> Value.none t
  | T1 (T_option, t), Some_ x -> Value.some (of_mvalue t x)
  | T2 (T_or _, tx, ty), Left x -> Value.left ty (of_mvalue tx x)
  | T2 (T_or _, tx, ty), Right y -> Value.left tx (of_mvalue ty y)
  | T1 (T_list, t), List xs -> Value.build_list (List.map (of_mvalue t) xs) t
  | T1 (T_set, telement), Set xs ->
      Value.set ~telement (List.map (of_mvalue telement) xs)
  | T2 (T_map, tkey, tvalue), Map xs ->
      let f (k, v) = (of_mvalue tkey k, of_mvalue tvalue v) in
      Value.map ~big:false ~tkey ~tvalue (List.map f xs)
  | T0 T_address, Address x -> Value.literal (Literal.address x)
  | T0 T_operation, Operation op ->
      let op = map_moperation (fun _ -> assert false) op in
      Value.operation (operation_of_moperation ~config op)
  | _, Closure _ -> failwith "Michelson_interpreter TODO: lambdas"
  | TRecord {layout; row}, _ ->
      let rec mk_record (l : Layout.t) x =
        match (l, x.mv) with
        | Leaf {source}, _ ->
          ( match List.assoc_opt source row with
          | None -> assert false
          | Some t -> [(source, of_mvalue t x)] )
        | Node (l1, l2), Pair (x1, x2) -> mk_record l1 x1 @ mk_record l2 x2
        | _ -> assert false
      in
      ( match Unknown.get layout with
      | None -> assert false
      | Some layout -> Value.record ~layout (mk_record layout x) )
  | TVariant {layout; row}, _ ->
      let rec mk_variant (l : Layout.t) x =
        match (l, x.mv) with
        | Leaf {source}, _ ->
          ( match List.assoc_opt source row with
          | None -> assert false
          | Some t' ->
              Value.variant_with_type ~line_no:[] source (of_mvalue t' x) t )
        | Node (l, _), Left x -> mk_variant l x
        | Node (_, l), Right x -> mk_variant l x
        | _ -> assert false
      in
      ( match Unknown.get layout with
      | None -> assert false
      | Some layout -> mk_variant layout x )
  | ( _
    , ( Unit | Bool _ | Int _ | String _ | Operation _ | Pair _ | None_
      | Some_ _ | Left _ | Right _ | Address _ | List _ | Set _ | Map _ ) ) ->
      assert false

let value_of_tmvalue_f config t tv =
  match (t.mt, tv) with
  | MT0 T_int, Int x -> Value.int x
  | MT0 T_nat, Int x -> Value.nat x
  | MT0 T_mutez, Int x -> Value.mutez x
  | MT0 T_timestamp, Int x -> Value.timestamp x
  | MT0 T_bool, Bool x -> Value.bool x
  | MT0 T_bytes, String x -> Value.bytes x
  | MT0 T_string, String x -> Value.string x
  | MT0 T_unit, Unit -> Value.unit
  | MT2 (T_pair _, _, _), Pair (x, y) -> Value.tuple [x; y]
  | MT1 (T_option, t), None_ -> Value.none (Type.of_mtype t)
  | _, Left x ->
    ( match t.mt with
    | MT2 (T_or _, _, ty) -> Value.left (Type.of_mtype ty) x
    | _ -> assert false )
  | _, Right y ->
    ( match t.mt with
    | MT2 (T_or _, tx, _) -> Value.right (Type.of_mtype tx) y
    | _ -> assert false )
  | _, Some_ x -> Value.some x
  | MT1 (T_list, t), List xs -> Value.build_list xs (Type.of_mtype t)
  | MT1 (T_set, telement), Set xs ->
      let telement = Type.of_mtype telement in
      Value.set ~telement xs
  | MT2 (T_map, tkey, tvalue), Map xs ->
      let tkey = Type.of_mtype tkey in
      let tvalue = Type.of_mtype tvalue in
      Value.map ~big:false ~tkey ~tvalue xs
  | MT2 (T_big_map, tkey, tvalue), Map xs ->
      let tkey = Type.of_mtype tkey in
      let tvalue = Type.of_mtype tvalue in
      Value.map ~big:true ~tkey ~tvalue xs
  | MT0 T_address, Address x -> Value.literal (Literal.address x)
  | _, Operation op -> Value.operation (operation_of_moperation ~config op)
  | _, Closure _ -> failwith "Michelson_interpreter TODO: lambdas"
  | ( _
    , ( Unit | Bool _ | Int _ | String _ | Pair _ | None_ | Address _ | List _
      | Set _ | Map _ ) ) ->
      assert false

let value_of_tmvalue config = cata_tmvalue (value_of_tmvalue_f config)

type context =
  { config : Config.t
  ; amount : Bigint.t
  ; balance : Bigint.t }

type _ action =
  | Return      : 'a -> 'a action
  | Bind        : 'a action * ('a -> 'b action) -> 'b action
  | Fail        : tmvalue -> 'a action
  | Throw       : string -> 'a action
  | Get_stack : mvalue list action
  | Set_stack   : mvalue list -> unit action
  | Get_context : context action
  | Block       : mvalue list * 'a action -> ('a * mvalue list) action

open Monad (struct
  type nonrec 'a t = 'a action

  let return x = Return x

  let bind x f = Bind (x, f)

  let map f x = bind x (fun x -> return (f x))

  let apply f x = bind f (fun f -> bind x (fun x -> return (f x)))
end)

type 'a result =
  | Ok     of 'a
  | Failed of tmvalue
  | Error  of string

let run (context : context) =
  let rec run : type a. mvalue list -> a action -> (a * mvalue list) result =
   fun stack -> function
    | Return x -> Ok (x, stack)
    | Bind (x, f) ->
      ( match run stack x with
      | Ok (x, stack) -> run stack (f x)
      | (Failed _ | Error _) as err -> err )
    | Fail e -> Failed e
    | Throw msg -> Error msg
    | Get_stack -> Ok (stack, stack)
    | Set_stack stack' -> Ok ((), stack')
    | Get_context -> Ok (context, stack)
    | Block (sub_stack, a) ->
      ( match run sub_stack a with
      | Ok (r, sub_stack') -> Ok ((r, sub_stack'), stack)
      | (Failed _ | Error _) as err -> err )
  in
  run

let block_ stack x = map snd (Block (stack, x))

module M (P : Primitives) = struct
  let of_option = function
    | {mv = None_} -> None
    | {mv = Some_ x} -> Some x
    | _ -> assert false

  let of_string = function
    | {mv = String x} -> x
    | _ -> assert false

  let of_int = function
    | {mv = Int x} -> x
    | _ -> assert false

  let max_mutez =
    let pow = Bigint.power_int_positive_int in
    let sub = Bigint.sub_big_int in
    let one = Bigint.big_int_of_int 1 in
    sub (pow 2 63) one

  let nil = {mv = List []}

  let empty_set = {mv = Set []}

  let empty_map = {mv = Map []}

  let empty_bigmap = {mv = Map []}

  let add t1 t2 x1 x2 =
    let r =
      match (x1.mv, x2.mv) with
      | Int x1, Int x2 -> Bigint.add_big_int x1 x2
      | _ -> assert false
    in
    match (t1.mt, t2.mt) with
    | MT0 T_mutez, MT0 T_mutez when Bigint.gt_big_int r max_mutez ->
        Throw "ADD: mutez overflow"
        (* TODO Should this be an mvalue or is this a different category of failure? *)
    | _ -> return (int r)

  let sub t1 t2 x1 x2 =
    let r =
      match (x1.mv, x2.mv) with
      | Int x1, Int x2 -> Bigint.sub_big_int x1 x2
      | _ -> assert false
    in
    match (t1.mt, t2.mt) with
    | MT0 T_mutez, MT0 T_mutez when Bigint.(lt_big_int r zero_big_int) ->
        Throw "SUB: mutez underflow"
    | _ -> return (int r)

  let mul t1 t2 x1 x2 =
    let r =
      match (x1.mv, x2.mv) with
      | Int x1, Int x2 -> Bigint.mult_big_int x1 x2
      | _ -> assert false
    in
    match (t1.mt, t2.mt) with
    | (MT0 T_mutez, _ | _, MT0 T_mutez) when Bigint.gt_big_int r max_mutez ->
        Throw "MUL: mutez overflow"
    | _ -> return (int r)

  let big256 = Bigint.of_int 256

  let lsl_ x1 x2 =
    match (x1.mv, x2.mv) with
    | Int x1, Int x2 ->
        if Bigint.le_big_int x2 big256
        then
          let x2 = Bigint.int_of_big_int x2 in
          return (int (Bigint.shift_left_big_int x1 x2))
        else Throw "LSL: second operand too large"
    | _ -> assert false

  let lsr_ x1 x2 =
    match (x1.mv, x2.mv) with
    | Int x1, Int x2 ->
        if Bigint.le_big_int x2 big256
        then
          let x2 = Bigint.int_of_big_int x2 in
          return (int (Bigint.shift_right_big_int x1 x2))
        else Throw "LSR: second operand too large"
    | _ -> assert false

  let xor x1 x2 =
    match (x1.mv, x2.mv) with
    | Bool x1, Bool x2 -> bool (x1 <> x2)
    | Int x1, Int x2 -> int (Bigint.xor_big_int x1 x2)
    | _ -> assert false

  let and_ x1 x2 =
    match (x1.mv, x2.mv) with
    | Bool x1, Bool x2 -> bool (x1 && x2)
    | Int x1, Int x2 -> int (Bigint.and_big_int x1 x2)
    | _ -> assert false

  let or_ x1 x2 =
    match (x1.mv, x2.mv) with
    | Bool x1, Bool x2 -> bool (x1 || x2)
    | Int x1, Int x2 -> int (Bigint.or_big_int x1 x2)
    | _ -> assert false

  let ediv x1 x2 =
    match (x1.mv, x2.mv) with
    | Int x1, Int x2 ->
        if Bigint.(equal zero_big_int x2)
        then none
        else
          let q, r = Bigint.quomod_big_int x1 x2 in
          (* TODO check signs *)
          some (pair (int q) (int r))
    | _ -> assert false

  let car = function
    | {mv = Pair (x, _)} -> x
    | _ -> assert false

  let cdr = function
    | {mv = Pair (_, x)} -> x
    | _ -> assert false

  let eq = function
    | {mv = Int x} -> bool Bigint.(eq_big_int zero_big_int x)
    | _ -> assert false

  let neq = function
    | {mv = Int x} -> bool (not Bigint.(eq_big_int zero_big_int x))
    | _ -> assert false

  let gt = function
    | {mv = Int x} -> bool Bigint.(gt_big_int x zero_big_int)
    | _ -> assert false

  let lt = function
    | {mv = Int x} -> bool Bigint.(lt_big_int x zero_big_int)
    | _ -> assert false

  let le = function
    | {mv = Int x} -> bool Bigint.(le_big_int x zero_big_int)
    | _ -> assert false

  let ge = function
    | {mv = Int x} -> bool Bigint.(ge_big_int x zero_big_int)
    | _ -> assert false

  let not_ = function
    | {mv = Bool x} -> bool (not x)
    (* | {mv=Int x} -> TODO two's complement *)
    | _ -> assert false

  let abs = function
    | {mv = Int x} -> int (Bigint.abs_big_int x)
    | _ -> assert false

  let neg = function
    | {mv = Int x} -> int (Bigint.minus_big_int x)
    | _ -> assert false

  let is_nat = function
    | {mv = Int x} ->
        if Bigint.(ge_big_int x zero_big_int) then some {mv = Int x} else none
    | _ -> assert false

  let cast s = s

  let rename s = s

  let concat1 = function
    | {mv = List xs} ->
        let f = function
          | {mv = String x} -> x
          (* TODO '0x' prefix? *)
          | _ -> assert false
        in
        let xs = List.map f xs in
        string (String.concat "" xs)
    | _ -> assert false

  let concat2 x1 x2 =
    match (x1.mv, x2.mv) with
    | String x1, String x2 -> string (x1 ^ x2)
    | _ -> assert false

  let size x =
    match x.mv with
    | List xs -> int (Bigint.big_int_of_int (List.length xs))
    | Set xs -> int (Bigint.big_int_of_int (List.length xs))
    | Map xs -> int (Bigint.big_int_of_int (List.length xs))
    | String x -> int (Bigint.big_int_of_int (String.length x))
    | _ -> assert false

  let pack t1 x =
    let* {config} = Get_context in
    let x = typecheck_mvalue t1 x in
    let x = value_of_tmvalue config x in
    return
      (string (Compiler.pack_value ~config ~scenario_vars:String.Map.empty x))

  let unpack t = function
    | {mv = String x} ->
        let* {config} = Get_context in
        let x = Compiler.unpack_value ~config (Type.of_mtype t) x in
        let x = mvalue_of_value (Type.of_mtype t) x in
        return (some x)
    | _ -> assert false

  let cons x xs =
    match xs.mv with
    | List xs -> list (x :: xs)
    | _ -> assert false

  let rec getn n =
    assert (n >= 0);
    function
    | x when n = 0 -> x
    | {mv = Pair (x, _)} when n = 1 -> x
    | {mv = Pair (_, x)} -> getn (n - 2) x
    | _ -> assert false

  let rec updaten n x1 x2 =
    assert (n >= 0);
    match x2 with
    | _ when n = 0 -> x1
    | {mv = Pair (_, b)} when n = 1 -> pair x1 b
    | {mv = Pair (a, b)} -> pair a (updaten (n - 2) x1 b)
    | _ -> assert false

  let rec compare_ t x1 x2 =
    match (t.mt, x1.mv, x2.mv) with
    | MT0 T_address, _, _ ->
        failwith "Michelson_interpreter TODO: compare addresses"
        (*https://tezos.gitlab.io/michelson-reference/#instr-COMPARE*)
    | _, Bool x1, Bool x2 -> compare x1 x2
    | _, Int x1, Int x2 -> Bigint.compare x1 x2
    | _, String x1, String x2 -> compare x1 x2 (* TODO check encoding *)
    | MT2 (T_pair _, t1, t2), Pair (x11, x12), Pair (x21, x22) ->
      ( match compare_ t1 x11 x21 with
      | 0 -> compare_ t2 x12 x22
      | r -> r )
    | _, Unit, Unit -> 0
    | _, None_, None_ -> 0
    | _, None_, Some_ _ -> -1
    | _, Some_ _, None_ -> 1
    | MT1 (T_option, t), Some_ x1, Some_ x2 -> compare_ t x1 x2
    | _, Left _, Right _ -> -1
    | _, Right _, Left _ -> 1
    | MT2 (T_or _, t, _), Left x1, Left x2
     |MT2 (T_or _, _, t), Right x1, Right x2 ->
        compare_ t x1 x2
    | _ -> assert false

  let compare_fst t (x1, _) (x2, _) = compare_ t x1 x2

  let get t1 x1 = function
    | {mv = Map entries} ->
      ( match List.find_opt (fun (k, _) -> compare_ t1 k x1 = 0) entries with
      | Some (_, v) -> some v
      | None -> none )
    | _ -> assert false

  let mem t1 x1 = function
    | {mv = Set els} -> bool (List.exists (fun e -> compare_ t1 e x1 = 0) els)
    | {mv = Map entries} ->
        bool (List.exists (fun (k, _) -> compare_ t1 k x1 = 0) entries)
    | _ -> assert false

  let slice x1 x2 x3 =
    match (x1.mv, x2.mv, x3.mv) with
    | Int offset, Int len, String x3 ->
        let offset = Bigint.int_of_big_int offset in
        let len = Bigint.int_of_big_int len in
        (* TODO return None if too big*)
        let l = String.length x3 (* TODO check encoding *) in
        if offset < l && offset + len <= l
        then some {mv = String (String.sub x3 offset len)}
        else none
    | _ -> assert false

  let open_chest x1 x2 x3 =
    match (x1.mv, x2.mv, x3.mv) with
    | String chest_key, String chest, Int time ->
        let time = Bigint.int_of_big_int time in
        let result = P.Timelock.open_chest chest_key chest time in
        ( match result with
        | Correct b -> left (string b)
        | Bogus_cipher -> right (bool false)
        | Bogus_opening -> right (bool true) )
    | _ -> assert false

  let transfer_tokens t1 x1 x2 x3 =
    match (x2.mv, x3.mv) with
    | Int z, Address c -> operation (Transfer_tokens (t1, x1, z, c))
    | _ -> assert false

  let set_delegate d =
    let d = Option.map of_string (of_option d) in
    operation (Set_delegate d)

  let create_contract ~tparameter ~tstorage code baker balance storage =
    let baker = Option.map of_string (of_option baker) in
    let balance = of_int balance in
    [ operation
        (Create_contract {baker; balance; tparameter; tstorage; storage; code})
    ; address "<created address TODO>" ]

  let get_and_update tk k v = function
    | {mv = Map entries} ->
        let previous, without =
          List.partition (fun (k', _) -> compare_ tk k k' = 0) entries
        in
        let previous =
          match previous with
          | [] -> none
          | [(_, v)] -> some v
          | _ -> assert false
        in
        let r =
          match v.mv with
          | None_ -> without
          | Some_ v -> List.sort (compare_fst tk) ((k, v) :: without)
          | _ -> assert false
        in
        (previous, mk_map r)
    | _ -> assert false

  let update tk k v container =
    match container.mv with
    | Set els ->
        let els = List.filter (fun e -> compare_ tk e k <> 0) els in
        ( match v.mv with
        | Bool true -> set (List.sort (compare_ tk) (k :: els))
        | Bool false -> set els
        | _ -> assert false )
    | Map entries ->
        let without =
          List.filter (fun (k', _) -> compare_ tk k k' <> 0) entries
        in
        ( match v.mv with
        | None_ -> mk_map without
        | Some_ v -> mk_map (List.sort (compare_fst tk) ((k, v) :: without))
        | _ -> assert false )
    | _ -> assert false

  let hash h = function
    | {mv = String x} -> string (h x)
    | _ -> assert false

  let blake2b = hash P.Crypto.blake2b

  let sha256 = hash P.Crypto.sha256

  let sha512 = hash P.Crypto.sha512

  let keccak = hash P.Crypto.keccak

  let sha3 = hash P.Crypto.sha3

  let dup n stack =
    assert (n >= 1);
    match List.split_at_opt (n - 1) stack with
    | Some (hi, x :: lo) -> Set_stack ((x :: hi) @ (x :: lo))
    | _ -> assert false

  let dig n stack =
    match List.split_at_opt n stack with
    | Some (hi, x :: lo) -> Set_stack ((x :: hi) @ lo)
    | _ -> assert false

  let dug n = function
    | x :: tail ->
      ( match List.split_at_opt n tail with
      | Some (hi, lo) -> Set_stack (hi @ (x :: lo))
      | _ -> assert false )
    | [] -> assert false

  let dropn n stack =
    assert (n <= List.length stack);
    Set_stack (List.drop n stack)

  let push l stack = Set_stack (mvalue_of_tliteral l :: stack)

  let rec prim0 = function
    | Sender -> assert false
    | Source -> assert false
    | Amount ->
        let* {amount} = Get_context in
        return {mv = Int amount}
    | Balance ->
        let* {balance} = Get_context in
        return {mv = Int balance}
    | Level -> assert false
    | Now -> assert false
    | Self _ -> assert false
    | Self_address -> assert false
    | Chain_id -> assert false
    | Total_voting_power -> assert false
    | Sapling_empty_state _ -> assert false
    | Unit_ -> return unit
    | None_ _ -> return none
    | Nil _ -> return nil
    | Empty_set _ -> return empty_set
    | Empty_map _ -> return empty_map
    | Empty_bigmap _ -> return empty_bigmap

  and prim1 op t1 x =
    let okify f = return (f x) in
    match op with
    | Car -> return (car x)
    | Cdr -> okify cdr
    | Left _ -> okify left
    | Right _ -> okify right
    | Some_ -> okify some
    | Eq -> okify eq
    | Abs -> okify abs
    | Neg -> okify neg
    | Int -> return x
    | IsNat -> okify is_nat
    | Neq -> okify neq
    | Le -> okify le
    | Lt -> okify lt
    | Ge -> okify ge
    | Gt -> okify gt
    | Not -> okify not_
    | Concat1 -> okify concat1
    | Size -> okify size
    | Address -> assert false
    | Implicit_account -> assert false
    | Contract _ -> assert false
    | Pack -> pack t1 x
    | Unpack t -> unpack t x
    | Hash_key -> assert false
    | Blake2b -> okify blake2b
    | Sha256 -> okify sha256
    | Sha512 -> okify sha512
    | Keccak -> okify keccak
    | Sha3 -> okify sha3
    | Set_delegate -> okify set_delegate
    | Read_ticket -> assert false
    | Join_tickets -> assert false
    | Pairing_check -> assert false
    | Voting_power -> assert false
    | Getn n -> okify (getn n)
    | Cast _ -> okify cast
    | Rename _ -> okify rename

  and prim2 : _ prim2 -> _ =
    let okify2 f x1 x2 = return (f x1 x2) in
    function
    | Pair _ -> fun _ _ -> okify2 pair
    | Add -> add
    | Mul -> mul
    | Sub -> sub
    | Lsr -> fun _ _ -> lsr_
    | Lsl -> fun _ _ -> lsl_
    | Xor -> fun _ _ -> okify2 xor
    | Ediv -> fun _ _ -> okify2 ediv
    | And -> fun _ _ -> okify2 and_
    | Or -> fun _ _ -> okify2 or_
    | Cons -> fun _ _ -> okify2 cons
    | Compare ->
        fun t _ x1 x2 -> return (int (Bigint.of_int (compare_ t x1 x2)))
    | Concat2 -> fun _ _ -> okify2 concat2
    | Get -> fun t1 _ -> okify2 (get t1)
    | Mem -> fun t1 _ -> okify2 (mem t1)
    | Exec -> fun _ _ -> exec
    | Apply -> fun _ _ -> okify2 apply
    | Sapling_verify_update -> assert false
    | Ticket -> assert false
    | Split_ticket -> assert false
    | Updaten n -> fun _ _ -> okify2 (updaten n)
    | View _ -> assert false

  and exec x f : _ action =
    match f.mv with
    | Closure (f, xs) ->
        let xs = List.rev (x :: xs) in
        let xs =
          match xs with
          | [] -> assert false
          | [x] -> x
          | xs -> Compiler.compile_tuple pair xs
        in
        (* TODO check ordering of arguments *)
        let* ys = block_ [xs] (interpret f) in
        ( match ys with
        | [y] -> return y
        | _ -> assert false )
    | _ -> assert false

  and apply x f =
    match f.mv with
    | Closure (f, xs) -> closure f (x :: xs)
    | _ -> assert false

  and prim3 op t1 _ _ x1 x2 x3 =
    match op with
    | Slice -> return [slice x1 x2 x3]
    | Update -> return [update t1 x1 x2 x3]
    | Get_and_update ->
        let y1, y2 = get_and_update t1 x1 x2 x3 in
        return [y1; y2]
    | Transfer_tokens -> return [transfer_tokens t1 x1 x2 x3]
    | Check_signature -> assert false
    | Open_chest -> return [open_chest x1 x2 x3]

  and interpret i : unit action =
    let* stack = Get_stack in
    assert (Michelson.has_error ~accept_missings:false i = []);
    match i.tinstr with
    | MIswap -> dig 1 stack
    | MIdig n -> dig n stack
    | MIdug n -> dug n stack
    | MIdup n -> dup n stack
    | MIdip i -> dipn 1 i stack
    | MIdipn (n, i) -> dipn n i stack
    | MIdrop -> dropn 1 stack
    | MIdropn n -> dropn n stack
    | MIpush (_t, l) -> push l stack
    | MI0 op ->
        let* x = prim0 op in
        Set_stack (x :: stack)
    | MI1 op ->
      ( match (i.stack_in, stack) with
      | Ok (Stack_ok (t1 :: _)), x1 :: xs ->
          let* x = prim1 op t1 x1 in
          Set_stack (x :: xs)
      | _ -> assert false )
    | MI1_fail Never -> assert false
    | MI1_fail Failwith ->
      ( match (i.stack_in, stack) with
      | Ok (Stack_ok (t :: _)), x :: _ -> Fail (typecheck_mvalue t x)
      | _ -> assert false )
    | MI2 op ->
      ( match (i.stack_in, stack) with
      | Ok (Stack_ok (t1 :: t2 :: _)), x1 :: x2 :: xs ->
          let* x = prim2 op t1 t2 x1 x2 in
          Set_stack (x :: xs)
      | _ -> assert false )
    | MI3 op ->
      ( match (i.stack_in, stack) with
      | Ok (Stack_ok (t1 :: t2 :: t3 :: _)), x1 :: x2 :: x3 :: xs ->
          let* x = prim3 op t1 t2 t3 x1 x2 x3 in
          Set_stack (x @ xs)
      | _ -> assert false )
    | MIseq xs -> seq xs
    | MIif (l, r) -> if_ l r stack
    | MIif_none (l, r) -> if_none l r stack
    | MIif_left (l, r) -> if_left l r stack
    | MIif_cons (l, r) -> if_cons l r stack
    | MIloop_left body -> loop_left body stack
    | MIloop body -> loop body stack
    | MIiter body -> iter_ body stack
    | MImap body -> map_ body stack
    | MIlambda (_t1, _t2, b) -> Set_stack ({mv = Closure (b, [])} :: stack)
    | MIunpair bs when List.for_all id bs ->
        let rec unpair r n xxs =
          assert (n >= 0);
          if n = 1
          then List.rev (xxs :: r)
          else
            match xxs with
            | {mv = Pair (x, xs)} -> unpair (x :: r) (n - 1) xs
            | _ -> assert false
        in
        ( match stack with
        | x :: xs -> Set_stack (unpair [] (List.length bs) x @ xs)
        | _ -> assert false )
    | MIpairn n ->
        let hi, lo = List.split_at ~err:"pairn" n stack in
        let x = Compiler.compile_tuple pair hi in
        Set_stack (x :: lo)
    | MIcreate_contract {tparameter; tstorage; code} ->
      ( match stack with
      | baker :: balance :: storage :: xs ->
          let tparameter = fst tparameter (* ? TODO *) in
          let x =
            create_contract
              ~tparameter
              ~tstorage
              (erase_types_instr code)
              baker
              balance
              storage
          in
          Set_stack (x @ xs)
      | _ -> assert false )
    | MIconcat_unresolved -> assert false
    | MIfield [A] ->
      ( match stack with
      | {mv = Pair (x1, _)} :: xs -> Set_stack (x1 :: xs)
      | _ -> assert false )
    | MIfield [D] ->
      ( match stack with
      | {mv = Pair (_, x2)} :: xs -> Set_stack (x2 :: xs)
      | _ -> assert false )
    | MIerror _ | MIcomment _ | MImich _ | MIunpair _ | MIfield _ | MIsetField _
      ->
        failwith ("Michelson_interpreter TODO: " ^ show_tinstr i)

  and dipn n i stack =
    let hi, lo = List.split_at n stack in
    let* lo = block_ lo (interpret i) in
    Set_stack (hi @ lo)

  and if_ l r = function
    | {mv = Bool true} :: xs -> Set_stack xs >> interpret l
    | {mv = Bool false} :: xs -> Set_stack xs >> interpret r
    | _ -> assert false

  and if_none l r = function
    | {mv = None_} :: xs -> Set_stack xs >> interpret l
    | {mv = Some_ x} :: xs -> Set_stack (x :: xs) >> interpret r
    | _ -> assert false

  and if_left l r = function
    | {mv = Left x} :: xs -> Set_stack (x :: xs) >> interpret l
    | {mv = Right x} :: xs -> Set_stack (x :: xs) >> interpret r
    | _ -> assert false

  and if_cons l r = function
    | {mv = List (v :: vs)} :: xs ->
        Set_stack (v :: list vs :: xs) >> interpret l
    | {mv = List []} :: xs -> Set_stack xs >> interpret r
    | _ -> assert false

  and loop_left body = function
    | {mv = Left x} :: stack ->
        Set_stack (x :: stack) >> interpret body >> Get_stack >>= loop_left body
    | {mv = Right x} :: stack -> Set_stack (x :: stack)
    | _ -> assert false

  and loop body = function
    | {mv = Bool true} :: stack ->
        Set_stack stack >> interpret body >> Get_stack >>= loop body
    | {mv = Bool false} :: stack -> Set_stack stack
    | _ -> assert false

  and iter_ body = function
    | {mv = List []} :: stack -> Set_stack stack
    | {mv = List (x :: xs)} :: stack ->
        let* () = Set_stack (x :: stack) >> interpret body in
        let* stack = Get_stack in
        iter_ body (list xs :: stack)
    | {mv = Set []} :: stack -> Set_stack stack
    | {mv = Set (x :: xs)} :: stack ->
        let* () = Set_stack (x :: stack) in
        let* () = interpret body in
        let* stack = Get_stack in
        iter_ body (set xs :: stack)
    | {mv = Map []} :: stack -> Set_stack stack
    | {mv = Map ((k, v) :: xs)} :: stack ->
        let* () = Set_stack (pair k v :: stack) >> interpret body in
        let* stack = Get_stack in
        iter_ body (mk_map xs :: stack)
    | _ -> assert false

  and map_ body = function
    | {mv = List xs} :: stack ->
        let rec f r xs stack =
          match xs with
          | [] -> Set_stack ({mv = List (List.rev r)} :: stack)
          | x :: xs ->
              let* () = Set_stack (x :: stack) >> interpret body in
              Get_stack
              >>= (function
              | y :: stack -> f (y :: r) xs stack
              | _ -> assert false)
        in
        f [] xs stack
    | {mv = Map xs} :: stack ->
        let rec f r xs stack =
          match xs with
          | [] -> Set_stack ({mv = Map (List.rev r)} :: stack)
          | (k, v) :: xs ->
              let* () = Set_stack (pair k v :: stack) >> interpret body in
              Get_stack
              >>= (function
              | y :: stack -> f ((k, y) :: r) xs stack
              | _ -> assert false)
        in
        f [] xs stack
    | _ -> assert false

  and seq xs = iter_list interpret xs

  let interpret context instr stack =
    match run context stack (interpret instr) with
    | Ok ((), r) -> Ok r
    | (Error _ | Failed _) as e -> e
end
