open Utils_pure.Control
open Type
open Primitive

val unify_types : mtype -> mtype -> mtype Result.t

val type_prim0 : mtype prim0 -> mtype

val type_prim1 : mtype prim1 -> mtype * mtype

val type_prim2 : mtype prim2 -> mtype * mtype * mtype

val type_prim3 : prim3 -> mtype * mtype * mtype * mtype
