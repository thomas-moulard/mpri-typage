(** Ce module génère une contrainte (syntaxique) de typage correspondant
    à un programme. *)

val ty_constraint_of : Syntax.program -> Constraint.ty_constraint

val dump_ty_constraint_of : Syntax.program -> Constraint.ty_constraint
