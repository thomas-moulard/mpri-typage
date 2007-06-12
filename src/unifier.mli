(** Ce module implémente un unificateur de termes du premier ordre spécialisé
    sur la syntaxe des types définie dans le module {!SolvingConstraint}. *)

open SolvingConstraint

exception Error of string

exception Cycle

(** Cette fonction unifie deux types au premier ordre. *)
val unify : ty -> ty -> unit

(** Cette fonction unifie deux variables (fusion de leur multi-equation). *)
val merge_var : var -> var -> unit
