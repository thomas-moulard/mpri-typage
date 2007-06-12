(** Ce module implémente un évaluateur pour les programmes du langage. *)

val eval_program: Syntax.program -> (Syntax.var * Syntax.term) list
