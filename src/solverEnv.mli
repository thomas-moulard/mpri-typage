(** Ce module implémente l'environnement de travail du solveur. *)

open SolvingConstraint

type env 

val empty_env : env

(** Un pool est un ensemble de variable. Le pool courant correspond 
    à l'ensemble des variables qu'on sait être liées dans le let 
    courant. *)
val current_pool : env -> var list

(** Les let introduisent des noms d'identifiants, ces fonctions
    permettent d'associer ces noms à des variables de type. *)
val lookup : env -> Constraint.Id.Atom.t -> var
val bind : env -> Constraint.Id.Atom.t -> var -> env

(** Iteration sur l'ensemble de toutes les variables. *)
val iter_over_pools : (int -> var list -> unit) -> env -> unit

(** Rang du let courant. *)
val current_rank : env -> int

(** Introduction d'une liste de variable dans l'environnement. 
    Si le premier argument est true alors on augmente le rang. *)
val introduce : bool -> env -> var list -> env

(** Renvoie l'environnement dans lequel on a supprimé tout ce qui
    concerne le let courant. *)
val pop_let : env -> var list -> env

(** Fonctions d'ordre supérieur sur les types et variables. *)
val fold_typ : ('a -> var -> 'a) -> 'a -> ty -> 'a

val foldmap_typ : ('a -> var -> 'a * var) -> 'a -> ty -> 'a * ty

(** Fonctions d'affichage. *)
val string_of_var : var -> string

val string_of_typ : ty -> string

val string_of_scheme : var -> string

val string_of_env : env -> string
