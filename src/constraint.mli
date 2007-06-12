(* This file was generated from constraint.mla. Do not edit! *)

(* Expose the module signatures defined in alphaLib. *)

open AlphaLib.Signatures

(* This module defines identifiers, that is,
   external representations for atoms. *)

module Identifier : Identifier with type t = AlphaLib.Atom.String.t

(* This exception is raised by the functions that convert raw forms
   into internal forms (see below) when an unbound identifier is
   encountered. *)

exception UnboundIdentifier of Identifier.t

(* This module defines atoms of sort id. *)

module Id : Atom with type identifier = Identifier.t

(* This module defines atoms of sort var. *)

module Var : Atom with type identifier = Identifier.t

(* This module reflects the type definitions found in the source
   file, but in a raw form, that is, in a form where atoms are
   represented by identifiers and abstractions are transparent.
   Raw forms are usually produced by a parser and consumed by a
   pretty-printer. Functions that convert to and from raw forms
   are provided. *)

module Raw : sig

type id =
  Identifier.t

 and var =
  Identifier.t

 and ty_constraint = 
  | CEq of ty * ty
  | CInst of id * ty
  | CLet of clet_ty_abs
  | CAnd of ty_constraint * ty_constraint
  | CExists of exists_abs
  | CNewConstant of newconstant_abs
  | CTrue
  | CFalse
  | CDump

 and clet_scheme = 
  clet_scheme_abs

 and ex_var = 
  var

 and ty_var = 
  var

 and ty = 
  | CTyVar of var
  | CTyArrow of ty * ty
  | CTyApp of var * ty list
  | CTyConstant of ty_constant

 and ty_constant = 
  | CTyInt
  | CTyString

 and clet_ty_abs = 
  id * clet_scheme * ty_constraint

 and exists_abs = 
  ex_var list * ty_constraint

 and newconstant_abs = 
  var * ty_constraint

 and clet_scheme_abs = 
  ex_var list * ty_constraint * ty

end

(* The following type definitions reflect those found in the source
   file, this time in an internal form, that is, in a form where both
   atoms and abstractions are opaque (abstract) data structures.
   Functions that convert between the opaque and transparent versions
   of each abstraction are provided. This approach provides safety --
   the contents of an abstraction cannot be inspected without
   appropriate freshening of its bound atoms -- and better efficiency
   -- sets and maps over atoms are usually less costly than sets and
   maps over identifiers. *)

type id =
  Id.Atom.t

 and var =
  Var.Atom.t

 and ty_constraint = 
  | CEq of ty * ty
  | CInst of id * ty
  | CLet of opaque_clet_ty_abs
  | CAnd of ty_constraint * ty_constraint
  | CExists of opaque_exists_abs
  | CNewConstant of opaque_newconstant_abs
  | CTrue
  | CFalse
  | CDump

 and clet_scheme = 
  opaque_clet_scheme_abs

 and ex_var = 
  var

 and ty_var = 
  var

 and ty = 
  | CTyVar of var
  | CTyArrow of ty * ty
  | CTyApp of var * ty list
  | CTyConstant of ty_constant

 and ty_constant = 
  | CTyInt
  | CTyString

 and clet_ty_abs = 
  id * clet_scheme * ty_constraint

 and opaque_clet_ty_abs

 and exists_abs = 
  ex_var list * ty_constraint

 and opaque_exists_abs

 and newconstant_abs = 
  var * ty_constraint

 and opaque_newconstant_abs

 and clet_scheme_abs = 
  ex_var list * ty_constraint * ty

 and opaque_clet_scheme_abs

(* The following functions operate over the expression type "ty_constraint". *)

(*
 * Function:   import_ty_constraint
 * Summary:    converts raw forms into internal forms
 * Parameters: a mapping of identifiers to atoms of sort id
 *             a mapping of identifiers to atoms of sort var
 *             an expression in raw form
 * Results:    an expression in internal form
 *               (semantically equivalent to the input expression)
 *)

val import_ty_constraint : id Identifier.Map.t * var Identifier.Map.t -> Raw.ty_constraint -> ty_constraint

(*
 * Function:   subst_ty_constraint
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort id
 *             a substitution of atoms for atoms at sort var
 *             an expression in internal form
 * Results:    an expression in internal form
 *               (the result of applying these substitutions to the input expression)
 *)

val subst_ty_constraint : Id.Subst.t * Var.Subst.t -> ty_constraint -> ty_constraint

(*
 * Function:   export_ty_constraint
 * Summary:    converts internal forms to raw forms
 * Parameters: a mapping of atoms of sort id to identifiers
 *             a mapping of atoms of sort var to identifiers
 *             an expression in internal form
 * Results:    an expression in raw form
 *               (semantically equivalent to the input expression)
 *               (raises [Foo.Atom.Unknown] if an unknown atom of sort "foo" is encountered
 *)

val export_ty_constraint : Id.AtomIdMap.t * Var.AtomIdMap.t -> ty_constraint -> Raw.ty_constraint

(*
 * Function:   free_ty_constraint
 * Summary:    collects free atoms
 * Parameters: an expression in internal form
 * Results:    the set of all atoms of sort id that occur free in the input expression
 *             the set of all atoms of sort var that occur free in the input expression
 *)

val free_ty_constraint : ty_constraint -> Id.AtomSet.t * Var.AtomSet.t

(* The following functions operate over the expression type "clet_scheme". *)

(*
 * Function:   import_clet_scheme
 * Summary:    converts raw forms into internal forms
 * Parameters: a mapping of identifiers to atoms of sort id
 *             a mapping of identifiers to atoms of sort var
 *             an expression in raw form
 * Results:    an expression in internal form
 *               (semantically equivalent to the input expression)
 *)

val import_clet_scheme : id Identifier.Map.t * var Identifier.Map.t -> Raw.clet_scheme -> clet_scheme

(*
 * Function:   subst_clet_scheme
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort id
 *             a substitution of atoms for atoms at sort var
 *             an expression in internal form
 * Results:    an expression in internal form
 *               (the result of applying these substitutions to the input expression)
 *)

val subst_clet_scheme : Id.Subst.t * Var.Subst.t -> clet_scheme -> clet_scheme

(*
 * Function:   export_clet_scheme
 * Summary:    converts internal forms to raw forms
 * Parameters: a mapping of atoms of sort id to identifiers
 *             a mapping of atoms of sort var to identifiers
 *             an expression in internal form
 * Results:    an expression in raw form
 *               (semantically equivalent to the input expression)
 *               (raises [Foo.Atom.Unknown] if an unknown atom of sort "foo" is encountered
 *)

val export_clet_scheme : Id.AtomIdMap.t * Var.AtomIdMap.t -> clet_scheme -> Raw.clet_scheme

(*
 * Function:   free_clet_scheme
 * Summary:    collects free atoms
 * Parameters: an expression in internal form
 * Results:    the set of all atoms of sort id that occur free in the input expression
 *             the set of all atoms of sort var that occur free in the input expression
 *)

val free_clet_scheme : clet_scheme -> Id.AtomSet.t * Var.AtomSet.t

(* The following functions operate over the pattern type "ex_var". *)

(*
 * Function:   subst_ex_var
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_ex_var : Var.Subst.t -> ex_var -> ex_var

(*
 * Function:   bound_ex_var
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_ex_var : ex_var -> Var.AtomSet.t

(*
 * Function:   bound_free_ex_var
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_free_ex_var : ex_var -> Var.AtomSet.t

(*
 * Function:   export_ex_var
 * Summary:    
 * Parameters: 
 * Results:    
 *)

val export_ex_var : Var.AtomIdMap.t -> ex_var -> Raw.ex_var

(* The following functions operate over the expression type "ty_var". *)

(*
 * Function:   import_ty_var
 * Summary:    converts raw forms into internal forms
 * Parameters: a mapping of identifiers to atoms of sort var
 *             an expression in raw form
 * Results:    an expression in internal form
 *               (semantically equivalent to the input expression)
 *)

val import_ty_var : var Identifier.Map.t -> Raw.ty_var -> ty_var

(*
 * Function:   subst_ty_var
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *             an expression in internal form
 * Results:    an expression in internal form
 *               (the result of applying these substitutions to the input expression)
 *)

val subst_ty_var : Var.Subst.t -> ty_var -> ty_var

(*
 * Function:   export_ty_var
 * Summary:    converts internal forms to raw forms
 * Parameters: a mapping of atoms of sort var to identifiers
 *             an expression in internal form
 * Results:    an expression in raw form
 *               (semantically equivalent to the input expression)
 *               (raises [Foo.Atom.Unknown] if an unknown atom of sort "foo" is encountered
 *)

val export_ty_var : Var.AtomIdMap.t -> ty_var -> Raw.ty_var

(*
 * Function:   free_ty_var
 * Summary:    collects free atoms
 * Parameters: an expression in internal form
 * Results:    the set of all atoms of sort var that occur free in the input expression
 *)

val free_ty_var : ty_var -> Var.AtomSet.t

(* The following functions operate over the expression type "ty". *)

(*
 * Function:   import_ty
 * Summary:    converts raw forms into internal forms
 * Parameters: a mapping of identifiers to atoms of sort var
 *             an expression in raw form
 * Results:    an expression in internal form
 *               (semantically equivalent to the input expression)
 *)

val import_ty : var Identifier.Map.t -> Raw.ty -> ty

(*
 * Function:   subst_ty
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *             an expression in internal form
 * Results:    an expression in internal form
 *               (the result of applying these substitutions to the input expression)
 *)

val subst_ty : Var.Subst.t -> ty -> ty

(*
 * Function:   export_ty
 * Summary:    converts internal forms to raw forms
 * Parameters: a mapping of atoms of sort var to identifiers
 *             an expression in internal form
 * Results:    an expression in raw form
 *               (semantically equivalent to the input expression)
 *               (raises [Foo.Atom.Unknown] if an unknown atom of sort "foo" is encountered
 *)

val export_ty : Var.AtomIdMap.t -> ty -> Raw.ty

(*
 * Function:   free_ty
 * Summary:    collects free atoms
 * Parameters: an expression in internal form
 * Results:    the set of all atoms of sort var that occur free in the input expression
 *)

val free_ty : ty -> Var.AtomSet.t

(* The following functions operate over the expression type "ty_constant". *)

(*
 * Function:   import_ty_constant
 * Summary:    converts raw forms into internal forms
 * Parameters: an expression in raw form
 * Results:    an expression in internal form
 *               (semantically equivalent to the input expression)
 *)

val import_ty_constant : unit -> Raw.ty_constant -> ty_constant

(*
 * Function:   subst_ty_constant
 * Summary:    substitutes atoms for atoms
 * Parameters: an expression in internal form
 * Results:    an expression in internal form
 *               (the result of applying these substitutions to the input expression)
 *)

val subst_ty_constant : unit -> ty_constant -> ty_constant

(*
 * Function:   export_ty_constant
 * Summary:    converts internal forms to raw forms
 * Parameters: an expression in internal form
 * Results:    an expression in raw form
 *               (semantically equivalent to the input expression)
 *               (raises [Foo.Atom.Unknown] if an unknown atom of sort "foo" is encountered
 *)

val export_ty_constant : unit -> ty_constant -> Raw.ty_constant

(*
 * Function:   free_ty_constant
 * Summary:    collects free atoms
 * Parameters: an expression in internal form
 * Results:    
 *)

val free_ty_constant : ty_constant -> unit

(* The following functions operate over the pattern type "clet_ty_abs". *)

(*
 * Function:   subst_clet_ty_abs
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort id
 *               (to be applied to sub-expressions in outer scope)
 *             a substitution of atoms for atoms at sort var
 *               (to be applied to sub-expressions in outer, neutral, or inner scope)
 *             a substitution of atoms for atoms at sort id
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_clet_ty_abs : Id.Subst.t * Var.Subst.t * Id.Subst.t -> clet_ty_abs -> clet_ty_abs

(*
 * Function:   bound_clet_ty_abs
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort id that occur in a binding position in the input pattern
 *)

val bound_clet_ty_abs : clet_ty_abs -> Id.AtomSet.t

(*
 * Function:   bound_free_clet_ty_abs
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort id that occur in a binding position in the input pattern
 *             the set of all atoms of sort id that occur free in a
 *               sub-expression that lies in inner scope in the input pattern 
 *             the set of all atoms of sort id that occur free in a
 *               sub-expression that lies in outer scope in the input pattern 
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in outer, neutral, or inner scope in the input pattern 
 *)

val bound_free_clet_ty_abs : clet_ty_abs -> Id.AtomSet.t * Id.AtomSet.t * Id.AtomSet.t * Var.AtomSet.t

(*
 * Function:   export_clet_ty_abs
 * Summary:    
 * Parameters: 
 * Results:    
 *)

val export_clet_ty_abs : Id.AtomIdMap.t * Var.AtomIdMap.t * Id.AtomIdMap.t -> clet_ty_abs -> Raw.clet_ty_abs

(*
 * Function:   create_clet_ty_abs
 * Summary:    creates an opaque abstraction
 * Parameters: a pattern in internal form
 *               (a transparent version of the abstraction)
 * Results:    an abstract data structure
 *               (an opaque version of the abstraction)
 *)

val create_clet_ty_abs : clet_ty_abs -> opaque_clet_ty_abs

(*
 * Function:   open_clet_ty_abs
 * Summary:    opens an opaque abstraction
 * Parameters: an abstract data structure
 *               (an opaque version of the abstraction)
 * Results:    a pattern in internal form
 *               (a transparent version of the abstraction, where all bound atoms are freshly renamed)
 *)

val open_clet_ty_abs : opaque_clet_ty_abs -> clet_ty_abs

(*
 * Function:   open2_clet_ty_abs
 * Summary:    opens two opaque abstractions, if they have the same structure
 * Parameters: two abstract data structures
 *               (two opaque versions of the abstraction)
 * Results:    the two patterns in internal form
 *               (two transparent versions of the abstraction,
 *              where all bound atoms are freshly renamed with the same names)
 *)

val open2_clet_ty_abs : opaque_clet_ty_abs -> opaque_clet_ty_abs -> clet_ty_abs * clet_ty_abs

(* The following functions operate over the pattern type "exists_abs". *)

(*
 * Function:   subst_exists_abs
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort id
 *               (to be applied to sub-expressions in outer, neutral, or inner scope)
 *             a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_exists_abs : Id.Subst.t * Var.Subst.t -> exists_abs -> exists_abs

(*
 * Function:   bound_exists_abs
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_exists_abs : exists_abs -> Var.AtomSet.t

(*
 * Function:   bound_free_exists_abs
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in inner scope in the input pattern 
 *             the set of all atoms of sort id that occur free in a
 *               sub-expression that lies in outer, neutral, or inner scope in the input pattern 
 *)

val bound_free_exists_abs : exists_abs -> Var.AtomSet.t * Var.AtomSet.t * Id.AtomSet.t

(*
 * Function:   export_exists_abs
 * Summary:    
 * Parameters: 
 * Results:    
 *)

val export_exists_abs : Id.AtomIdMap.t * Var.AtomIdMap.t -> exists_abs -> Raw.exists_abs

(*
 * Function:   create_exists_abs
 * Summary:    creates an opaque abstraction
 * Parameters: a pattern in internal form
 *               (a transparent version of the abstraction)
 * Results:    an abstract data structure
 *               (an opaque version of the abstraction)
 *)

val create_exists_abs : exists_abs -> opaque_exists_abs

(*
 * Function:   open_exists_abs
 * Summary:    opens an opaque abstraction
 * Parameters: an abstract data structure
 *               (an opaque version of the abstraction)
 * Results:    a pattern in internal form
 *               (a transparent version of the abstraction, where all bound atoms are freshly renamed)
 *)

val open_exists_abs : opaque_exists_abs -> exists_abs

(*
 * Function:   open2_exists_abs
 * Summary:    opens two opaque abstractions, if they have the same structure
 * Parameters: two abstract data structures
 *               (two opaque versions of the abstraction)
 * Results:    the two patterns in internal form
 *               (two transparent versions of the abstraction,
 *              where all bound atoms are freshly renamed with the same names)
 *)

val open2_exists_abs : opaque_exists_abs -> opaque_exists_abs -> exists_abs * exists_abs

(* The following functions operate over the pattern type "newconstant_abs". *)

(*
 * Function:   subst_newconstant_abs
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort id
 *               (to be applied to sub-expressions in outer, neutral, or inner scope)
 *             a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_newconstant_abs : Id.Subst.t * Var.Subst.t -> newconstant_abs -> newconstant_abs

(*
 * Function:   bound_newconstant_abs
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_newconstant_abs : newconstant_abs -> Var.AtomSet.t

(*
 * Function:   bound_free_newconstant_abs
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in inner scope in the input pattern 
 *             the set of all atoms of sort id that occur free in a
 *               sub-expression that lies in outer, neutral, or inner scope in the input pattern 
 *)

val bound_free_newconstant_abs : newconstant_abs -> Var.AtomSet.t * Var.AtomSet.t * Id.AtomSet.t

(*
 * Function:   export_newconstant_abs
 * Summary:    
 * Parameters: 
 * Results:    
 *)

val export_newconstant_abs : Id.AtomIdMap.t * Var.AtomIdMap.t -> newconstant_abs -> Raw.newconstant_abs

(*
 * Function:   create_newconstant_abs
 * Summary:    creates an opaque abstraction
 * Parameters: a pattern in internal form
 *               (a transparent version of the abstraction)
 * Results:    an abstract data structure
 *               (an opaque version of the abstraction)
 *)

val create_newconstant_abs : newconstant_abs -> opaque_newconstant_abs

(*
 * Function:   open_newconstant_abs
 * Summary:    opens an opaque abstraction
 * Parameters: an abstract data structure
 *               (an opaque version of the abstraction)
 * Results:    a pattern in internal form
 *               (a transparent version of the abstraction, where all bound atoms are freshly renamed)
 *)

val open_newconstant_abs : opaque_newconstant_abs -> newconstant_abs

(*
 * Function:   open2_newconstant_abs
 * Summary:    opens two opaque abstractions, if they have the same structure
 * Parameters: two abstract data structures
 *               (two opaque versions of the abstraction)
 * Results:    the two patterns in internal form
 *               (two transparent versions of the abstraction,
 *              where all bound atoms are freshly renamed with the same names)
 *)

val open2_newconstant_abs : opaque_newconstant_abs -> opaque_newconstant_abs -> newconstant_abs * newconstant_abs

(* The following functions operate over the pattern type "clet_scheme_abs". *)

(*
 * Function:   subst_clet_scheme_abs
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort id
 *               (to be applied to sub-expressions in outer, neutral, or inner scope)
 *             a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_clet_scheme_abs : Id.Subst.t * Var.Subst.t -> clet_scheme_abs -> clet_scheme_abs

(*
 * Function:   bound_clet_scheme_abs
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_clet_scheme_abs : clet_scheme_abs -> Var.AtomSet.t

(*
 * Function:   bound_free_clet_scheme_abs
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in inner scope in the input pattern 
 *             the set of all atoms of sort id that occur free in a
 *               sub-expression that lies in outer, neutral, or inner scope in the input pattern 
 *)

val bound_free_clet_scheme_abs : clet_scheme_abs -> Var.AtomSet.t * Var.AtomSet.t * Id.AtomSet.t

(*
 * Function:   export_clet_scheme_abs
 * Summary:    
 * Parameters: 
 * Results:    
 *)

val export_clet_scheme_abs : Id.AtomIdMap.t * Var.AtomIdMap.t -> clet_scheme_abs -> Raw.clet_scheme_abs

(*
 * Function:   create_clet_scheme_abs
 * Summary:    creates an opaque abstraction
 * Parameters: a pattern in internal form
 *               (a transparent version of the abstraction)
 * Results:    an abstract data structure
 *               (an opaque version of the abstraction)
 *)

val create_clet_scheme_abs : clet_scheme_abs -> opaque_clet_scheme_abs

(*
 * Function:   open_clet_scheme_abs
 * Summary:    opens an opaque abstraction
 * Parameters: an abstract data structure
 *               (an opaque version of the abstraction)
 * Results:    a pattern in internal form
 *               (a transparent version of the abstraction, where all bound atoms are freshly renamed)
 *)

val open_clet_scheme_abs : opaque_clet_scheme_abs -> clet_scheme_abs

(*
 * Function:   open2_clet_scheme_abs
 * Summary:    opens two opaque abstractions, if they have the same structure
 * Parameters: two abstract data structures
 *               (two opaque versions of the abstraction)
 * Results:    the two patterns in internal form
 *               (two transparent versions of the abstraction,
 *              where all bound atoms are freshly renamed with the same names)
 *)

val open2_clet_scheme_abs : opaque_clet_scheme_abs -> opaque_clet_scheme_abs -> clet_scheme_abs * clet_scheme_abs

(* The following class contains code that ``transforms'' a data structure.
   The methods provided in this class implement an identity transformation,
   that is, they traverse the data structure and produce a semantically
   equivalent copy of it. The intended use of this class is for the client
   to create a subclass and override one or several methods so as to obtain
   nontrivial behavior. *)

class map : object

  (*
   * Method:     ty_constraint
   * Summary:    transforms an expression
   * Parameters: an expression in internal form
   * Results:    an expression in internal form
   *               (the result of applying the transformation to the input expression)
   *)

  method ty_constraint : ty_constraint -> ty_constraint

  (*
   * Method:     ceq
   * Summary:    transforms under the data constructor "CEq"
   * Parameters: a tuple of the parameters to "CEq"
   * Results:    the result of applying "CEq" to the transformed parameters
   *)

  method ceq : ty * ty -> ty_constraint

  (*
   * Method:     cinst
   * Summary:    transforms under the data constructor "CInst"
   * Parameters: a tuple of the parameters to "CInst"
   * Results:    the result of applying "CInst" to the transformed parameters
   *)

  method cinst : id * ty -> ty_constraint

  (*
   * Method:     clet
   * Summary:    transforms under the data constructor "CLet"
   * Parameters: a tuple of the parameters to "CLet"
   * Results:    the result of applying "CLet" to the transformed parameters
   *)

  method clet : opaque_clet_ty_abs -> ty_constraint

  (*
   * Method:     cand
   * Summary:    transforms under the data constructor "CAnd"
   * Parameters: a tuple of the parameters to "CAnd"
   * Results:    the result of applying "CAnd" to the transformed parameters
   *)

  method cand : ty_constraint * ty_constraint -> ty_constraint

  (*
   * Method:     cexists
   * Summary:    transforms under the data constructor "CExists"
   * Parameters: a tuple of the parameters to "CExists"
   * Results:    the result of applying "CExists" to the transformed parameters
   *)

  method cexists : opaque_exists_abs -> ty_constraint

  (*
   * Method:     cnewconstant
   * Summary:    transforms under the data constructor "CNewConstant"
   * Parameters: a tuple of the parameters to "CNewConstant"
   * Results:    the result of applying "CNewConstant" to the transformed parameters
   *)

  method cnewconstant : opaque_newconstant_abs -> ty_constraint

  (*
   * Method:     ctrue
   * Summary:    transforms under the data constructor "CTrue"
   * Parameters: a tuple of the parameters to "CTrue"
   * Results:    the result of applying "CTrue" to the transformed parameters
   *)

  method ctrue : ty_constraint

  (*
   * Method:     cfalse
   * Summary:    transforms under the data constructor "CFalse"
   * Parameters: a tuple of the parameters to "CFalse"
   * Results:    the result of applying "CFalse" to the transformed parameters
   *)

  method cfalse : ty_constraint

  (*
   * Method:     cdump
   * Summary:    transforms under the data constructor "CDump"
   * Parameters: a tuple of the parameters to "CDump"
   * Results:    the result of applying "CDump" to the transformed parameters
   *)

  method cdump : ty_constraint

  (*
   * Method:     clet_scheme
   * Summary:    transforms an expression
   * Parameters: an expression in internal form
   * Results:    an expression in internal form
   *               (the result of applying the transformation to the input expression)
   *)

  method clet_scheme : clet_scheme -> clet_scheme

  (*
   * Method:     ex_var
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method ex_var : ex_var -> ex_var

  (*
   * Method:     ty_var
   * Summary:    transforms an expression
   * Parameters: an expression in internal form
   * Results:    an expression in internal form
   *               (the result of applying the transformation to the input expression)
   *)

  method ty_var : ty_var -> ty_var

  (*
   * Method:     ty
   * Summary:    transforms an expression
   * Parameters: an expression in internal form
   * Results:    an expression in internal form
   *               (the result of applying the transformation to the input expression)
   *)

  method ty : ty -> ty

  (*
   * Method:     ctyvar
   * Summary:    transforms under the data constructor "CTyVar"
   * Parameters: a tuple of the parameters to "CTyVar"
   * Results:    the result of applying "CTyVar" to the transformed parameters
   *)

  method ctyvar : var -> ty

  (*
   * Method:     ctyarrow
   * Summary:    transforms under the data constructor "CTyArrow"
   * Parameters: a tuple of the parameters to "CTyArrow"
   * Results:    the result of applying "CTyArrow" to the transformed parameters
   *)

  method ctyarrow : ty * ty -> ty

  (*
   * Method:     ctyapp
   * Summary:    transforms under the data constructor "CTyApp"
   * Parameters: a tuple of the parameters to "CTyApp"
   * Results:    the result of applying "CTyApp" to the transformed parameters
   *)

  method ctyapp : var * ty list -> ty

  (*
   * Method:     ctyconstant
   * Summary:    transforms under the data constructor "CTyConstant"
   * Parameters: a tuple of the parameters to "CTyConstant"
   * Results:    the result of applying "CTyConstant" to the transformed parameters
   *)

  method ctyconstant : ty_constant -> ty

  (*
   * Method:     ty_constant
   * Summary:    transforms an expression
   * Parameters: an expression in internal form
   * Results:    an expression in internal form
   *               (the result of applying the transformation to the input expression)
   *)

  method ty_constant : ty_constant -> ty_constant

  (*
   * Method:     ctyint
   * Summary:    transforms under the data constructor "CTyInt"
   * Parameters: a tuple of the parameters to "CTyInt"
   * Results:    the result of applying "CTyInt" to the transformed parameters
   *)

  method ctyint : ty_constant

  (*
   * Method:     ctystring
   * Summary:    transforms under the data constructor "CTyString"
   * Parameters: a tuple of the parameters to "CTyString"
   * Results:    the result of applying "CTyString" to the transformed parameters
   *)

  method ctystring : ty_constant

  (*
   * Method:     clet_ty_abs
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method clet_ty_abs : clet_ty_abs -> clet_ty_abs

  (*
   * Method:     exists_abs
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method exists_abs : exists_abs -> exists_abs

  (*
   * Method:     newconstant_abs
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method newconstant_abs : newconstant_abs -> newconstant_abs

  (*
   * Method:     clet_scheme_abs
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method clet_scheme_abs : clet_scheme_abs -> clet_scheme_abs

end

(* The following class contains code that iterates over a data structure
   while updating an accumulator. The methods provided in this class
   implement an identity over the accumulator, that is, they traverse the
   data structure and always return the initial accumulator. The intended
   use of this class is for the client to create a subclass and override
   one or several methods so as to obtain nontrivial behavior. *)

class [ 'accumulator ] fold : object

  (*
   * Method:     ty_constraint
   * Summary:    iterates over an expression
   * Parameters: an accumulator
   *             an expression in internal form
   * Results:    an updated accumulator, obtained by traversing the input expression
   *)

  method ty_constraint : 'accumulator -> ty_constraint -> 'accumulator

  (*
   * Method:     ceq
   * Summary:    iterates under the data constructor "CEq"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CEq"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ceq : 'accumulator -> ty * ty -> 'accumulator

  (*
   * Method:     cinst
   * Summary:    iterates under the data constructor "CInst"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CInst"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method cinst : 'accumulator -> id * ty -> 'accumulator

  (*
   * Method:     clet
   * Summary:    iterates under the data constructor "CLet"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CLet"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method clet : 'accumulator -> opaque_clet_ty_abs -> 'accumulator

  (*
   * Method:     cand
   * Summary:    iterates under the data constructor "CAnd"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CAnd"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method cand : 'accumulator -> ty_constraint * ty_constraint -> 'accumulator

  (*
   * Method:     cexists
   * Summary:    iterates under the data constructor "CExists"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CExists"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method cexists : 'accumulator -> opaque_exists_abs -> 'accumulator

  (*
   * Method:     cnewconstant
   * Summary:    iterates under the data constructor "CNewConstant"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CNewConstant"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method cnewconstant : 'accumulator -> opaque_newconstant_abs -> 'accumulator

  (*
   * Method:     ctrue
   * Summary:    iterates under the data constructor "CTrue"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CTrue"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ctrue : 'accumulator -> 'accumulator

  (*
   * Method:     cfalse
   * Summary:    iterates under the data constructor "CFalse"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CFalse"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method cfalse : 'accumulator -> 'accumulator

  (*
   * Method:     cdump
   * Summary:    iterates under the data constructor "CDump"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CDump"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method cdump : 'accumulator -> 'accumulator

  (*
   * Method:     clet_scheme
   * Summary:    iterates over an expression
   * Parameters: an accumulator
   *             an expression in internal form
   * Results:    an updated accumulator, obtained by traversing the input expression
   *)

  method clet_scheme : 'accumulator -> clet_scheme -> 'accumulator

  (*
   * Method:     ex_var
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method ex_var : 'accumulator -> ex_var -> 'accumulator

  (*
   * Method:     ty_var
   * Summary:    iterates over an expression
   * Parameters: an accumulator
   *             an expression in internal form
   * Results:    an updated accumulator, obtained by traversing the input expression
   *)

  method ty_var : 'accumulator -> ty_var -> 'accumulator

  (*
   * Method:     ty
   * Summary:    iterates over an expression
   * Parameters: an accumulator
   *             an expression in internal form
   * Results:    an updated accumulator, obtained by traversing the input expression
   *)

  method ty : 'accumulator -> ty -> 'accumulator

  (*
   * Method:     ctyvar
   * Summary:    iterates under the data constructor "CTyVar"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CTyVar"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ctyvar : 'accumulator -> var -> 'accumulator

  (*
   * Method:     ctyarrow
   * Summary:    iterates under the data constructor "CTyArrow"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CTyArrow"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ctyarrow : 'accumulator -> ty * ty -> 'accumulator

  (*
   * Method:     ctyapp
   * Summary:    iterates under the data constructor "CTyApp"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CTyApp"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ctyapp : 'accumulator -> var * ty list -> 'accumulator

  (*
   * Method:     ctyconstant
   * Summary:    iterates under the data constructor "CTyConstant"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CTyConstant"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ctyconstant : 'accumulator -> ty_constant -> 'accumulator

  (*
   * Method:     ty_constant
   * Summary:    iterates over an expression
   * Parameters: an accumulator
   *             an expression in internal form
   * Results:    an updated accumulator, obtained by traversing the input expression
   *)

  method ty_constant : 'accumulator -> ty_constant -> 'accumulator

  (*
   * Method:     ctyint
   * Summary:    iterates under the data constructor "CTyInt"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CTyInt"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ctyint : 'accumulator -> 'accumulator

  (*
   * Method:     ctystring
   * Summary:    iterates under the data constructor "CTyString"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CTyString"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ctystring : 'accumulator -> 'accumulator

  (*
   * Method:     clet_ty_abs
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method clet_ty_abs : 'accumulator -> clet_ty_abs -> 'accumulator

  (*
   * Method:     exists_abs
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method exists_abs : 'accumulator -> exists_abs -> 'accumulator

  (*
   * Method:     newconstant_abs
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method newconstant_abs : 'accumulator -> newconstant_abs -> 'accumulator

  (*
   * Method:     clet_scheme_abs
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method clet_scheme_abs : 'accumulator -> clet_scheme_abs -> 'accumulator

end

(* The following class contains code that iterates over a data structure
   while updating an accumulator. The methods provided in this class
   implement an identity over the accumulator, that is, they traverse the
   data structure and always return the initial accumulator. The intended
   use of this class is for the client to create a subclass and override
   one or several methods so as to obtain nontrivial behavior. *)

class [ 'accumulator ] fold2 : object

  (*
   * Method:     ty_constraint
   * Summary:    iterates over two alpha-equivalent expressions
   * Parameters: an accumulator
   *             two expressions in internal form
   * Results:    an updated accumulator, obtained by traversing the input expressions
   *)

  method ty_constraint : 'accumulator -> ty_constraint -> ty_constraint -> 'accumulator

  (*
   * Method:     ceq
   * Summary:    iterates under the data constructor "CEq"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CEq"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ceq : 'accumulator -> ty * ty -> ty * ty -> 'accumulator

  (*
   * Method:     cinst
   * Summary:    iterates under the data constructor "CInst"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CInst"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method cinst : 'accumulator -> id * ty -> id * ty -> 'accumulator

  (*
   * Method:     clet
   * Summary:    iterates under the data constructor "CLet"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CLet"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method clet : 'accumulator -> opaque_clet_ty_abs -> opaque_clet_ty_abs -> 'accumulator

  (*
   * Method:     cand
   * Summary:    iterates under the data constructor "CAnd"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CAnd"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method cand : 'accumulator -> ty_constraint * ty_constraint -> ty_constraint * ty_constraint -> 'accumulator

  (*
   * Method:     cexists
   * Summary:    iterates under the data constructor "CExists"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CExists"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method cexists : 'accumulator -> opaque_exists_abs -> opaque_exists_abs -> 'accumulator

  (*
   * Method:     cnewconstant
   * Summary:    iterates under the data constructor "CNewConstant"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CNewConstant"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method cnewconstant : 'accumulator -> opaque_newconstant_abs -> opaque_newconstant_abs -> 'accumulator

  (*
   * Method:     ctrue
   * Summary:    iterates under the data constructor "CTrue"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CTrue"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ctrue : 'accumulator -> 'accumulator

  (*
   * Method:     cfalse
   * Summary:    iterates under the data constructor "CFalse"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CFalse"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method cfalse : 'accumulator -> 'accumulator

  (*
   * Method:     cdump
   * Summary:    iterates under the data constructor "CDump"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CDump"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method cdump : 'accumulator -> 'accumulator

  (*
   * Method:     clet_scheme
   * Summary:    iterates over two alpha-equivalent expressions
   * Parameters: an accumulator
   *             two expressions in internal form
   * Results:    an updated accumulator, obtained by traversing the input expressions
   *)

  method clet_scheme : 'accumulator -> clet_scheme -> clet_scheme -> 'accumulator

  (*
   * Method:     ex_var
   * Summary:    iterates over two alpha-equivalent patterns
   * Parameters: an accumulator
   *             two patterns in internal form
   * Results:    an updated accumulator, obtained by traversing the input patterns
   *)

  method ex_var : 'accumulator -> ex_var -> ex_var -> 'accumulator

  (*
   * Method:     ty_var
   * Summary:    iterates over two alpha-equivalent expressions
   * Parameters: an accumulator
   *             two expressions in internal form
   * Results:    an updated accumulator, obtained by traversing the input expressions
   *)

  method ty_var : 'accumulator -> ty_var -> ty_var -> 'accumulator

  (*
   * Method:     ty
   * Summary:    iterates over two alpha-equivalent expressions
   * Parameters: an accumulator
   *             two expressions in internal form
   * Results:    an updated accumulator, obtained by traversing the input expressions
   *)

  method ty : 'accumulator -> ty -> ty -> 'accumulator

  (*
   * Method:     ctyvar
   * Summary:    iterates under the data constructor "CTyVar"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CTyVar"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ctyvar : 'accumulator -> var -> var -> 'accumulator

  (*
   * Method:     ctyarrow
   * Summary:    iterates under the data constructor "CTyArrow"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CTyArrow"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ctyarrow : 'accumulator -> ty * ty -> ty * ty -> 'accumulator

  (*
   * Method:     ctyapp
   * Summary:    iterates under the data constructor "CTyApp"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CTyApp"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ctyapp : 'accumulator -> var * ty list -> var * ty list -> 'accumulator

  (*
   * Method:     ctyconstant
   * Summary:    iterates under the data constructor "CTyConstant"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CTyConstant"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ctyconstant : 'accumulator -> ty_constant -> ty_constant -> 'accumulator

  (*
   * Method:     ty_constant
   * Summary:    iterates over two alpha-equivalent expressions
   * Parameters: an accumulator
   *             two expressions in internal form
   * Results:    an updated accumulator, obtained by traversing the input expressions
   *)

  method ty_constant : 'accumulator -> ty_constant -> ty_constant -> 'accumulator

  (*
   * Method:     ctyint
   * Summary:    iterates under the data constructor "CTyInt"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CTyInt"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ctyint : 'accumulator -> 'accumulator

  (*
   * Method:     ctystring
   * Summary:    iterates under the data constructor "CTyString"
   * Parameters: an accumulator
   *             a tuple of the parameters to "CTyString"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ctystring : 'accumulator -> 'accumulator

  (*
   * Method:     clet_ty_abs
   * Summary:    iterates over two alpha-equivalent patterns
   * Parameters: an accumulator
   *             two patterns in internal form
   * Results:    an updated accumulator, obtained by traversing the input patterns
   *)

  method clet_ty_abs : 'accumulator -> clet_ty_abs -> clet_ty_abs -> 'accumulator

  (*
   * Method:     exists_abs
   * Summary:    iterates over two alpha-equivalent patterns
   * Parameters: an accumulator
   *             two patterns in internal form
   * Results:    an updated accumulator, obtained by traversing the input patterns
   *)

  method exists_abs : 'accumulator -> exists_abs -> exists_abs -> 'accumulator

  (*
   * Method:     newconstant_abs
   * Summary:    iterates over two alpha-equivalent patterns
   * Parameters: an accumulator
   *             two patterns in internal form
   * Results:    an updated accumulator, obtained by traversing the input patterns
   *)

  method newconstant_abs : 'accumulator -> newconstant_abs -> newconstant_abs -> 'accumulator

  (*
   * Method:     clet_scheme_abs
   * Summary:    iterates over two alpha-equivalent patterns
   * Parameters: an accumulator
   *             two patterns in internal form
   * Results:    an updated accumulator, obtained by traversing the input patterns
   *)

  method clet_scheme_abs : 'accumulator -> clet_scheme_abs -> clet_scheme_abs -> 'accumulator

end

(*
 * Function:   eq_ty_constraint
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_ty_constraint : ty_constraint -> ty_constraint -> bool

(*
 * Function:   eq_clet_scheme
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_clet_scheme : clet_scheme -> clet_scheme -> bool

(*
 * Function:   eq_ex_var
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_ex_var : ex_var -> ex_var -> bool

(*
 * Function:   eq_ty_var
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_ty_var : ty_var -> ty_var -> bool

(*
 * Function:   eq_ty
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_ty : ty -> ty -> bool

(*
 * Function:   eq_ty_constant
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_ty_constant : ty_constant -> ty_constant -> bool

(*
 * Function:   eq_clet_ty_abs
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_clet_ty_abs : clet_ty_abs -> clet_ty_abs -> bool

(*
 * Function:   eq_exists_abs
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_exists_abs : exists_abs -> exists_abs -> bool

(*
 * Function:   eq_newconstant_abs
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_newconstant_abs : newconstant_abs -> newconstant_abs -> bool

(*
 * Function:   eq_clet_scheme_abs
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_clet_scheme_abs : clet_scheme_abs -> clet_scheme_abs -> bool
