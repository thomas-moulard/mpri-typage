(** Ce module implémente un arbre de syntaxe pour les contraintes qui
    est plus pratique pour une résolution de contrainte à base de Union/Find. 
    La différence avec le module {!Constraint} est minime puisqu'il s'agit juste
    de modifier le type des variables de type. 

    Dans le module {!Constraint},
    il s'agit du type des variables de AlphaCaml (les "atomes" dans le jargon
    d'AlphaCaml), ici, une variable de type est une variable d'unification, 
    c'est-à-dire un élément d'une classe d'équivalence 
    (i.e. d'une multi-équation). 
*)

(** Le type algébrique pour les contraintes utilisées dans le solveur. *)

(** Les variables d'identifiants sont inchangées. *)
type id = Constraint.Id.Atom.t

(** Les constructeurs de type algébrique sont des identifiants de type. *)
and tycon = Constraint.Var.Atom.t

(** Par contre, le type des variables de type est devenu abstrait. 
    On ne peut l'utiliser qu'à travers les opérations fournies par ce module.
*)
and var

type ty_constraint =
    CEq of ty * ty
  | CInst of id * ty
  | CLet of id * clet_scheme * ty_constraint
  | CAnd of ty_constraint * ty_constraint
  | CExists of ex_var list * ty_constraint
  | CTrue
  | CFalse
  | CDump

(** Une petite différence ici : les schémas contraints sont de la forme
    forall vs {C} v et non pas forall vs {C} ty. Nous avons vu en cours
    les raisons de ce changement. *)
and clet_scheme = ex_var list * ty_constraint * var

and ex_var = var

and ty_var = var

and ty =
    CTyVar of var
  | CTyArrow of ty * ty
  | CTyApp of tycon * ty list
  | CTyConstant of ty_constant

and ty_constant = CTyInt | CTyString

(** Convertit une contrainte syntaxique issue du parsing ou bien de la
    génération de contraintes (module {!Infer}) en une contrainte adaptée
    au solveur. *)
val from : Constraint.ty_constraint -> ty_constraint

(** Ces informations concernent la classe d'équivalence de la variable. *)

(** La structure d'une multi-equation est le type auquel elle est égale. *)
val structure : var -> ty option
val set_structure : var -> ty option -> unit

(** Les marques sont utilisées pour les parcours dans le graphe induit par
    les multi-equations. *)
val set_mark : Mark.t -> var -> unit
val mark : var -> Mark.t

(** Le rang correspond à la profondeur du let où la variable est liée. *)
val set_rank : int -> var -> unit
val change_rank : int -> var -> var
val rank : var -> int

(** [generalized_rank] est un rang spécial marquant les variables 
    généralisées.*)
val generalized_rank : int

(** [undefined_rank] est utilisée pour les variables non encore introduites
    dans la contrainte. *)
val undefined_rank : int

(** Lorsqu'on fusionne deux variables et que l'une d'entre elles a un nom,
    on le maintien pour la classe d'équivalence. Sinon, on choisit un nom
    frais. *)
val name : var -> string
val set_name : var -> string -> unit

(** Retourne une variable d'unification fraiche en spécifiant éventuellement
    un nom. *)
val fresh_var : ?name:string -> unit -> var

(** Fusionne deux classes d'équivalence. *)
val equalize : var -> var -> unit

(** Teste si deux variables sont dans la même classe d'équivalence. *)
val equivalent : var -> var -> bool
