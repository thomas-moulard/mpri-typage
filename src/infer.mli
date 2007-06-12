(** Ce module g�n�re une contrainte (syntaxique) de typage correspondant
    � un programme. *)

val ty_constraint_of : Syntax.program -> Constraint.ty_constraint

val dump_ty_constraint_of : Syntax.program -> Constraint.ty_constraint
