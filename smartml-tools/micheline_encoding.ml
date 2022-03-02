(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open SmartML

(* This code was adapted from https://github.com/tqtezos/TZComet (Copyright (c) 2020 Seb Mondet) *)

(** See src/proto_alpha/lib_protocol/michelson_v1_primitives.ml *)

(* New instructions must be added here, "for" backward compatibility of the encoding. *)
(* Keep the comment above at the end of the list *)
let primitives =
  [("K_parameter", "parameter"); ("K_storage", "storage"); ("K_code", "code")]
  @ List.map
      (fun x -> (x, x))
      [ "False"
      ; "Elt"
      ; "Left"
      ; "None"
      ; "Pair"
      ; "Right"
      ; "Some"
      ; "True"
      ; "Unit"
      ; "PACK"
      ; "UNPACK"
      ; "BLAKE2B"
      ; "SHA256"
      ; "SHA512"
      ; "ABS"
      ; "ADD"
      ; "AMOUNT"
      ; "AND"
      ; "BALANCE"
      ; "CAR"
      ; "CDR"
      ; "CHECK_SIGNATURE"
      ; "COMPARE"
      ; "CONCAT"
      ; "CONS"
      ; "CREATE_ACCOUNT"
      ; "CREATE_CONTRACT"
      ; "IMPLICIT_ACCOUNT"
      ; "DIP"
      ; "DROP"
      ; "DUP"
      ; "EDIV"
      ; "EMPTY_MAP"
      ; "EMPTY_SET"
      ; "EQ"
      ; "EXEC"
      ; "FAILWITH"
      ; "GE"
      ; "GET"
      ; "GT"
      ; "HASH_KEY"
      ; "IF"
      ; "IF_CONS"
      ; "IF_LEFT"
      ; "IF_NONE"
      ; "INT"
      ; "LAMBDA"
      ; "LE"
      ; "LEFT"
      ; "LOOP"
      ; "LSL"
      ; "LSR"
      ; "LT"
      ; "MAP"
      ; "MEM"
      ; "MUL"
      ; "NEG"
      ; "NEQ"
      ; "NIL"
      ; "NONE"
      ; "NOT"
      ; "NOW"
      ; "OR"
      ; "PAIR"
      ; "PUSH"
      ; "RIGHT"
      ; "SIZE"
      ; "SOME"
      ; "SOURCE"
      ; "SENDER"
      ; "SELF"
      ; "STEPS_TO_QUOTA"
      ; "SUB"
      ; "SWAP"
      ; "TRANSFER_TOKENS"
      ; "SET_DELEGATE"
      ; "UNIT"
      ; "UPDATE"
      ; "XOR"
      ; "ITER"
      ; "LOOP_LEFT"
      ; "ADDRESS"
      ; "CONTRACT"
      ; "ISNAT"
      ; "CAST"
      ; "RENAME"
      ; "bool"
      ; "contract"
      ; "int"
      ; "key"
      ; "key_hash"
      ; "lambda"
      ; "list"
      ; "map"
      ; "big_map"
      ; "nat"
      ; "option"
      ; "or"
      ; "pair"
      ; "set"
      ; "signature"
      ; "string"
      ; "bytes"
      ; "mutez"
      ; "timestamp"
      ; "unit"
      ; "operation"
      ; "address"
      ; (* Alpha_002 addition *)
        "SLICE"
      ; (* Alpha_005 addition *)
        "DIG"
      ; "DUG"
      ; "EMPTY_BIG_MAP"
      ; "APPLY"
      ; "chain_id"
      ; "CHAIN_ID"
      ; (* Alpha_008 addition *)
        "LEVEL"
      ; "SELF_ADDRESS"
      ; "never"
      ; "NEVER"
      ; "UNPAIR"
      ; "VOTING_POWER"
      ; "TOTAL_VOTING_POWER"
      ; "KECCAK"
      ; "SHA3"
      ; "PAIRING_CHECK"
      ; "bls12_381_g1"
      ; "bls12_381_g2"
      ; "bls12_381_fr"
      ; "sapling_state"
      ; "sapling_transaction"
      ; "SAPLING_EMPTY_STATE"
      ; "SAPLING_VERIFY_UPDATE"
      ; "ticket"
      ; "TICKET"
      ; "READ_TICKET"
      ; "SPLIT_TICKET"
      ; "JOIN_TICKETS"
      ; "GET_AND_UPDATE"
      ; (* Protocol 10 *)
        "chest"
      ; "chest_key"
      ; "OPEN_CHEST"
      ; "VIEW"
      ; "view"
      ; "constant" ]

let expr_encoding =
  Tezos_micheline.Micheline.canonical_encoding_v1
    ~variant:"michelson_v1"
    (* Data_encoding.Encoding.string *)
    (let open Data_encoding in
    def "michelson.v1.primitives" @@ string_enum primitives)

let json_of_micheline_canonical mich =
  Data_encoding.Json.construct
    expr_encoding
    (* Tezos_micheline.Micheline.canonical_location_encoding *)
    mich

let rec mich_of_node = function
  | Tezos_micheline.Micheline.Int (_, i) -> Basics.Int (Z.to_string i)
  | String (_, s) -> String s
  | Bytes (_, b) -> Bytes (Bytes.to_string b)
  | Prim (_, name, arguments, annotations) ->
      Primitive {name; annotations; arguments = List.map mich_of_node arguments}
  | Seq (_, l) -> Sequence (List.map mich_of_node l)

let mich_of_bytes bytes =
  Data_encoding.Binary.of_bytes_exn
    (* Tezos_micheline.Micheline.canonical_location_encoding *)
    expr_encoding
    (Hex.to_bytes (`Hex bytes))
  |> Tezos_micheline.Micheline.root
  |> mich_of_node

let parse_hex_bytes bytes =
  try
    let mich =
      Data_encoding.Binary.of_bytes_exn
        (* Tezos_micheline.Micheline.canonical_location_encoding *)
        expr_encoding
        (Hex.to_bytes (`Hex bytes))
    in
    let json = json_of_micheline_canonical mich in
    Ok
      ( json
      , let open Tezos_micheline in
        Fmt.str
          "%a"
          Micheline_printer.print_expr
          (Micheline_printer.printable Base.Fn.id mich) )
  with
  | e ->
      let open Tezos_error_monad.Error_monad in
      Error [Exn e]

let pack_node_expression e =
  Data_encoding.Binary.to_bytes_exn
    expr_encoding
    (Tezos_micheline.Micheline.strip_locations e)
  |> Bytes.to_string

let encode_michelson_string s =
  Data_encoding.Binary.to_bytes_exn
    expr_encoding
    Tezos_micheline.Micheline.(String (0, s) |> strip_locations)
  |> Bytes.to_string

let micheline_of_ezjsonm json =
  let enc =
    Tezos_micheline.Micheline.canonical_encoding
      ~variant:"custom"
      Data_encoding.string
  in
  let mich = Data_encoding.Json.destruct enc json in
  Tezos_micheline.Micheline.root mich

let micheline_node_to_micheline_canonical node =
  Tezos_micheline.Micheline.strip_locations node

let micheline_canonical_to_string c =
  Fmt.str
    "%a"
    Tezos_micheline.Micheline_printer.print_expr
    (Tezos_micheline.Micheline_printer.printable Base.Fn.id c)

let micheline_node_to_string node =
  node |> micheline_node_to_micheline_canonical |> micheline_canonical_to_string

let rec node_of_mich = function
  | Basics.Int i -> Tezos_micheline.Micheline.Int (0, Z.of_string i)
  | String s -> String (0, s)
  | Bytes b -> Bytes (0, Bytes.of_string b)
  | Primitive {name; annotations; arguments} ->
      Prim (0, name, List.map node_of_mich arguments, annotations)
  | Sequence l -> Seq (0, List.map node_of_mich l)

let pack_prefix = "\x05"

let micheline_size micheline =
  try
    ( node_of_mich micheline
    |> pack_node_expression
    |> Utils.Misc.Hex.hexcape
    |> String.length )
    (* Devision by 2 is necessary (Each hex char is represented by a nible[4 bits] and we want the size in byte[8 bits] ) *)
    / 2
  with
  | Data_encoding.Binary.Write_error s ->
      failwith
        (Format.asprintf
           "Data_encoding.micheline_size error %a"
           Data_encoding.Binary.pp_write_error
           s)

let micheline_size_opt micheline =
  try Some (micheline_size micheline) with
  | Failure _ -> None

let parse_node instr =
  (*
    Make sure micheline is parsed within curly brackets:
    ==============
      Invalid:
    ==============
      storage int;
      parameter unit;
      code
          {
              CDR;
              NIL operation;
              PAIR
          }
    ==============
      Valid:
    ==============
    {
      storage int;
      parameter unit;
      code
          {
              CDR;
              NIL operation;
              PAIR
          }
    }
  *)
  let instr =
    let trimed = Base.String.strip instr in
    if not (Base.String.is_prefix trimed ~prefix:"{")
    then "{" ^ trimed ^ "}"
    else trimed
  in
  let tokens, _ = Tezos_micheline.Micheline_parser.tokenize instr in
  let node, _ = Tezos_micheline.Micheline_parser.parse_expression tokens in
  mich_of_node node
