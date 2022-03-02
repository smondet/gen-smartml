(* Copyright 2019-2021 Smart Chain Arena LLC. *)

open Utils_pure

(* A layout explains where the leaves of a record type are
   positioned. This does *not* include the layout of any of its fields
   that are records. *)
type l =
  { source : string
  ; target : string }
[@@deriving eq, ord, show {with_path = false}]

type t = l Binary_tree.t [@@deriving eq, ord, show {with_path = false}]

let leaf source target = Binary_tree.Leaf {source; target}
