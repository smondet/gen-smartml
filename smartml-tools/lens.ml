(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils

(** An [('a, 'b) zipper] is an ['a] with a ['b] in focus that is easy
   to manipulate. *)
type ('a, 'b) zipper =
  { focus : 'b
  ; zip : 'b -> 'a }

(** An [('a, 'b) t] gives us a means to access ['b] within ['a]. *)
type ('a, 'b) t = {unzip : 'a -> ('a, 'b) zipper}

let get {unzip} x = (unzip x).focus

let set {unzip} rhs x = (unzip x).zip rhs

let get_and_set {unzip} rhs x =
  let {focus; zip} = unzip x in
  (focus, zip rhs)

let modify x {unzip} f =
  let {focus; zip} = unzip x in
  zip (f focus)

let id =
  let unzip x = {focus = x; zip = (fun x -> x)} in
  {unzip}

let ( @. ) {unzip = a} {unzip = b} =
  let unzip x =
    let za = a x in
    let zb = b za.focus in
    {focus = zb.focus; zip = (fun c -> za.zip (zb.zip c))}
  in
  {unzip}

let some ~err =
  let unzip = function
    | None -> failwith err
    | Some x -> {focus = x; zip = (fun x -> Some x)}
  in
  {unzip}

let option ~err =
  let unzip x =
    { focus = Some x
    ; zip =
        (function
        | None -> failwith err
        | Some x -> x) }
  in
  {unzip}

let assoc ~equal ~key =
  let unzip xs =
    match Base.List.split_while xs ~f:(fun (k, _) -> not (equal k key)) with
    | pre, (k, v) :: post ->
        { focus = Some v
        ; zip =
            (function
            | None -> pre @ post
            | Some v -> pre @ ((k, v) :: post)) }
    | _, [] ->
        { focus = None
        ; zip =
            (function
            | None -> xs
            | Some v -> (key, v) :: xs) }
  in
  {unzip}

let assoc_exn ~equal ~key ~err = assoc ~equal ~key @. some ~err

let nth n =
  let unzip xs =
    match Base.List.split_n xs n with
    | pre, x :: post ->
        { focus = Some x
        ; zip =
            (function
            | Some x -> pre @ (x :: post)
            | None -> failwith "list_nth: cannot delete") }
    | _ ->
        { focus = None
        ; zip =
            (function
            | None -> xs
            | Some _ -> failwith "list_nth: cannot insert") }
  in
  {unzip}

let sorted_list ~equal ~elem =
  let unzip xs =
    match Base.List.split_while xs ~f:(fun k -> not (equal k elem)) with
    | pre, e :: post ->
        { focus = true
        ; zip =
            (function
            | false -> pre @ post
            | true -> pre @ (e :: post)) }
    | _, [] ->
        { focus = false
        ; zip =
            (function
            | false -> xs
            | true -> elem :: xs)
            (* FIXME respect sort order *) }
  in
  {unzip}

let ref r = {unzip = (fun () -> {focus = !r; zip = (fun x -> r := x)})}

let unref =
  { unzip =
      (fun r ->
        { focus = !r
        ; zip =
            (fun x ->
              r := x;
              r) }) }

let make unzip = {unzip}

let bi f g = {unzip = (fun x -> {focus = f x; zip = g})}

let hashtbl_at key tbl =
  let unzip () =
    let zip v =
      Option.cata (Hashtbl.remove tbl key) (Hashtbl.replace tbl key) v
    in
    let focus = Hashtbl.find_opt tbl key in
    {focus; zip}
  in
  {unzip}
