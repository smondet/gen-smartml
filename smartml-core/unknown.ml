(* Copyright 2019-2021 Smart Chain Arena LLC. *)

type 'a unknown =
  | Unknown
  | Value   of 'a
  | Ref     of 'a t

and 'a t = 'a unknown ref [@@deriving eq, ord, show {with_path = false}]

let rec normalize r =
  match !r with
  | Ref r -> normalize r
  | _ -> r

let equal eq r1 r2 = equal eq (normalize r1) (normalize r2)

let rec get r =
  match !r with
  | Ref r -> get r
  | Unknown -> None
  | Value x -> Some x

let get_ok msg x =
  match get x with
  | None -> failwith ("Unkwown.get_ok: " ^ msg)
  | Some x -> x

let unknown () = ref Unknown

let value x = ref (Value x)

let equalize r1 r2 =
  let r1 = normalize r1 in
  let r2 = normalize r2 in
  if r1 != r2
  then
    match (!r1, !r2) with
    | Unknown, _ ->
        r1 := Ref r2;
        true
    | _, Unknown ->
        r2 := Ref r1;
        true
    | Value a, Value b when a = b ->
        if !r1 != !r2 then r1 := Ref r2;
        true
    | _ -> false
  else true

let equalize_merge eq merge r1 r2 =
  let r1 = normalize r1 in
  let r2 = normalize r2 in
  if r1 != r2
  then
    match (!r1, !r2) with
    | Unknown, _ ->
        r1 := Ref r2;
        true
    | _, Unknown ->
        r2 := Ref r1;
        true
    | Value a, Value b when eq a b ->
        if !r1 != !r2 then r1 := Ref r2;
        true
    | Value a, Value b ->
      ( match merge a b with
      | Some r ->
          r2 := Value r;
          r1 := Ref r2;
          true
      | None -> false )
    | _ -> assert false
  else true

let set r x = equalize r (value x)

let default r x =
  match get r with
  | None ->
      let success = set r x in
      assert success
  | Some _ -> ()
