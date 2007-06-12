(** Ce module implémente un solveur de contraintes. *)

exception Answer of string

val solve_constraint : SolvingConstraint.ty_constraint -> unit
