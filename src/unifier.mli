(** Ce module impl�mente un unificateur de termes du premier ordre sp�cialis�
    sur la syntaxe des types d�finie dans le module {!SolvingConstraint}. *)

open SolvingConstraint

exception Error of string

exception Cycle

(** Cette fonction unifie deux types au premier ordre. *)
val unify : ty -> ty -> unit

(** Cette fonction unifie deux variables (fusion de leur multi-equation). *)
val merge_var : var -> var -> unit
