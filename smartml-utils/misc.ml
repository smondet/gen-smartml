(* Copyright 2019-2021 Smart Chain Arena LLC. *)

module Dbg = struct
  open Format

  let on = ref false

  let zero = Sys.time ()

  let f fmt =
    let now = Sys.time () in
    if !on
    then eprintf ("\n@[|DBG-lib:%f> " ^^ fmt ^^ "@]\n%!") (now -. zero)
    else ikfprintf ignore err_formatter fmt

  let p g = f "%a" g ()

  let summary ?(len = 200) s =
    try Base.String.sub ~pos:0 ~len s with
    | _ -> s
end

module type JsonGetter = sig
  val null : bool

  val get : string -> Yojson.Basic.t

  val string : string -> string

  val string_option : string -> string option

  val string_list : string -> string list

  val int : string -> int

  val bool : string -> bool
end

let json_getter x =
  ( module struct
    open Yojson.Basic.Util

    let null = x = `Null

    let get attr = member attr x

    let string attr =
      try to_string (get attr) with
      | _ -> failwith ("Cannot parse " ^ attr)

    let string_option attr = to_string_option (get attr)

    let string_list attr = to_list (get attr) |> List.map to_string

    let int attr = to_int (get attr)

    let bool attr = to_bool (get attr)
  end : JsonGetter )

let json_sub x attr =
  let module M = (val x : JsonGetter) in
  let open M in
  json_getter (get attr)

module Hex = struct
  let hexcape s =
    let b = Buffer.create (String.length s * 2) in
    let addf fmt = Printf.ksprintf (Buffer.add_string b) fmt in
    Base.String.iter s ~f:(fun c -> addf "%02x" (Char.code c));
    Buffer.contents b

  let unhex s =
    let s =
      Base.(String.chop_prefix s ~prefix:"0x" |> Option.value ~default:s)
    in
    let length = String.length s - 2 in
    let length, s =
      if length mod 2 != 0 then (length + 1, "0" ^ s) else (length, s)
    in
    let b = Buffer.create ((String.length s / 2) + 2) in
    for i = 0 to length do
      if i mod 2 = 0
      then
        Scanf.sscanf (String.sub s i 2) "%02x" (fun c ->
            Buffer.add_char b (Char.chr c))
    done;
    Buffer.contents b
end

let ( <|> ) x y = if Base.Option.is_some x then x else y ()

let memoize ?clear_after f =
  let cache = Hashtbl.create 10 in
  let rec f' x =
    ( match clear_after with
    | Some n when Hashtbl.length cache > n -> Hashtbl.clear cache
    | _ -> () );
    match Hashtbl.find_opt cache x with
    | None ->
        let y = f f' x in
        Hashtbl.replace cache x y;
        y
    | Some y -> y
  in
  f'

let pp_with_margin ppf n k () =
  let open Format in
  let bak = pp_get_margin ppf () in
  Format.pp_print_flush ppf ();
  pp_set_margin ppf n;
  k ();
  Format.pp_print_flush ppf ();
  pp_set_margin ppf bak

let pp_with_max_indent ppf n k () =
  let open Format in
  let bak = pp_get_max_indent ppf () in
  Format.pp_print_flush ppf ();
  pp_set_max_indent ppf n;
  k ();
  Format.pp_print_flush ppf ();
  pp_set_max_indent ppf bak

type json =
  | J_int    of Bigint.t
  | J_bool   of bool
  | J_string of string
  | J_list   of json list
  | J_record of (string * json) list
[@@deriving eq, ord, show {with_path = false}]

let rec json_to_json : _ -> Yojson.Basic.t = function
  | J_int i -> `String (Bigint.string_of_big_int i)
  | J_bool b -> `Bool b
  | J_string s -> `String s
  | J_record fields ->
      `Assoc (List.map (fun (k, v) -> (k, json_to_json v)) fields)
  | J_list xs -> `List (List.map json_to_json xs)

let pp_json_as_json ?(margin = 180) ?(max_indent = 160) () ppf x =
  pp_with_margin
    ppf
    margin
    (pp_with_max_indent ppf max_indent (fun () ->
         Yojson.Basic.pretty_print ppf (json_to_json x)))
    ()

let tic () =
  let t0 = Unix.gettimeofday () in
  fun msg ->
    let dt = Unix.gettimeofday () -. t0 in
    Printf.eprintf "% 6.3fms %s\n" (dt *. 1000.0) msg

let tictoc msg f x =
  let toc = tic () in
  let y = f x in
  toc msg;
  y

let with_buffer x =
  let b = Buffer.create 64 in
  x b;
  Buffer.contents b

let buffer_protect b p op cl x =
  if p
  then (
    Buffer.add_string b op;
    x ();
    Buffer.add_string b cl )
  else x ()

let buffer_concat b sep f xs =
  List.iteri
    (fun i x ->
      if i <> 0 then Buffer.add_string b sep;
      f x)
    xs

let t = ref None

let tick s =
  let now = Unix.gettimeofday () in
  ( match !t with
  | None -> Printf.eprintf "%s\n" s
  | Some t ->
      let dt = (now -. t) *. 1000.0 in
      Printf.eprintf "%60.1fms\n%s\n" dt s );
  t := Some now
