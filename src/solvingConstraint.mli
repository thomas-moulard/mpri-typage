(** Ce module impl�mente un arbre de syntaxe pour les contraintes qui
    est plus pratique pour une r�solution de contrainte � base de Union/Find. 
    La diff�rence avec le module {!Constraint} est minime puisqu'il s'agit juste
    de modifier le type des variables de type. 

    Dans le module {!Constraint},
    il s'agit du type des variables de AlphaCaml (les "atomes" dans le jargon
    d'AlphaCaml), ici, une variable de type est une variable d'unification, 
    c'est-�-dire un �l�ment d'une classe d'�quivalence 
    (i.e. d'une multi-�quation). 
*)

(** Le type alg�brique pour les contraintes utilis�es dans le solveur. *)

(** Les variables d'identifiants sont inchang�es. *)
type id = Constraint.Id.Atom.t

(** Les constructeurs de type alg�brique sont des identifiants de type. *)
and tycon = Constraint.Var.Atom.t

(** Par contre, le type des variables de type est devenu abstrait. 
    On ne peut l'utiliser qu'� travers les op�rations fournies par ce module.
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

(** Une petite diff�rence ici : les sch�mas contraints sont de la forme
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
    g�n�ration de contraintes (module {!Infer}) en une contrainte adapt�e
    au solveur. *)
val from : Constraint.ty_constraint -> ty_constraint

(** Ces informations concernent la classe d'�quivalence de la variable. *)

(** La structure d'une multi-equation est le type auquel elle est �gale. *)
val structure : var -> ty option
val set_structure : var -> ty option -> unit

(** Les marques sont utilis�es pour les parcours dans le graphe induit par
    les multi-equations. *)
val set_mark : Mark.t -> var -> unit
val mark : var -> Mark.t

(** Le rang correspond � la profondeur du let o� la variable est li�e. *)
val set_rank : int -> var -> unit
val change_rank : int -> var -> var
val rank : var -> int

(** [generalized_rank] est un rang sp�cial marquant les variables 
    g�n�ralis�es.*)
val generalized_rank : int

(** [undefined_rank] est utilis�e pour les variables non encore introduites
    dans la contrainte. *)
val undefined_rank : int

(** Lorsqu'on fusionne deux variables et que l'une d'entre elles a un nom,
    on le maintien pour la classe d'�quivalence. Sinon, on choisit un nom
    frais. *)
val name : var -> string
val set_name : var -> string -> unit

(** Retourne une variable d'unification fraiche en sp�cifiant �ventuellement
    un nom. *)
val fresh_var : ?name:string -> unit -> var

(** Fusionne deux classes d'�quivalence. *)
val equalize : var -> var -> unit

(** Teste si deux variables sont dans la m�me classe d'�quivalence. *)
val equivalent : var -> var -> bool
