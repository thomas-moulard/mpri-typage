(** Ce module implémente un pretty printer pour les programmes. *)

val program : CommonPrinter.env -> Syntax.Raw.program -> CommonPrinter.env

val term : CommonPrinter.env -> Syntax.Raw.term -> CommonPrinter.env
