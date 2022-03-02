(* Copyright 2019-2021 Smart Chain Arena LLC. *)

(** A lens is essentially a getter together with a setter. Given an
   object of type ['a], we can get/set a location of type ['b] within
   it. *)
type ('a, 'b) t

(** {1 Basics} *)

val get : ('a, 'b) t -> 'a -> 'b
(** Get the ['b] part of ['a]. *)

val set : ('a, 'b) t -> 'b -> 'a -> 'a
(** Set the ['b] part of ['a]. *)

val get_and_set : ('a, 'b) t -> 'b -> 'a -> 'b * 'a
(** Set the ['b] part of ['a] and return the old value. *)

val modify : 'a -> ('a, 'b) t -> ('b -> 'b) -> 'a
(** Modify the ['b] part of ['a]. *)

(** {1 Category} *)

val id : ('a, 'a) t
(** The identity lens. *)

val ( @. ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
(** Compose two lenses. *)

(** {1 Lists} *)

val assoc : equal:('a -> 'a -> bool) -> key:'a -> (('a * 'b) list, 'b option) t
(** Access the first occurrence in an association list. *)

val assoc_exn :
  equal:('a -> 'a -> bool) -> key:'a -> err:string -> (('a * 'b) list, 'b) t
(** Access the first occurrence in an association list, or raise an
   exception. *)

val nth : int -> ('a list, 'a option) t
(** Access the nth element of a list. *)

val sorted_list : equal:('a -> 'a -> bool) -> elem:'a -> ('a list, bool) t

(** {1 Options} *)

val some : err:string -> ('a option, 'a) t
(** Unwrap/wrap ['a] in [Some] ([get] may fail). *)

val option : err:string -> ('a, 'a option) t
(** Wrap/unwrap ['a] in [Some] ([set] may fail). *)

(** {1 References} *)

val ref : 'a ref -> (unit, 'a) t
(** Access a reference. We pretend that all references are
   sub-expressions of [()] *)

val unref : ('a ref, 'a) t

(** {1 Roll your own} *)

(** A zipper represents an ['a] together with a location of type ['b]
   that is in focus (and thus easy to manipulate). *)
type ('a, 'b) zipper =
  { focus : 'b
  ; zip : 'b -> 'a }

val make : ('a -> ('a, 'b) zipper) -> ('a, 'b) t
(** Make a new lens from scratch. *)

val bi : ('a -> 'b) -> ('b -> 'a) -> ('a, 'b) t

val hashtbl_at : 'a -> ('a, 'b) Hashtbl.t -> (unit, 'b option) t
