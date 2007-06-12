(** Ce module implémente un pretty-printer pour les contraintes. *)

open CommonPrinter
open Constraint.Raw

val ty_constraint : env -> ty_constraint -> env

val as_raw : Constraint.ty_constraint -> ty_constraint
