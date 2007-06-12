(* This file was generated from syntax.mla. Do not edit! *)

(* Expose the module signatures defined in alphaLib. *)

open AlphaLib.Signatures

(* This module defines identifiers, that is,
   external representations for atoms. *)

module Identifier : Identifier with type t = AlphaLib.Atom.String.t

(* This exception is raised by the functions that convert raw forms
   into internal forms (see below) when an unbound identifier is
   encountered. *)

exception UnboundIdentifier of Identifier.t

(* This module defines atoms of sort var. *)

module Var : Atom with type identifier = Identifier.t

(* This module reflects the type definitions found in the source
   file, but in a raw form, that is, in a form where atoms are
   represented by identifiers and abstractions are transparent.
   Raw forms are usually produced by a parser and consumed by a
   pretty-printer. Functions that convert to and from raw forms
   are provided. *)

module Raw : sig

type var =
  Identifier.t

 and annotation = 
  ( Positions.t )

 and program = 
  | EmptyProgram
  | NewDefinition of annotation * toplevel_abs

 and toplevel_definition = 
  | TypeDefinition of var * param list * type_definition
  | ValDefinition of var * term
  | RecDefinitions of var * term

 and param = 
  var

 and term = 
  | EVar of annotation * var
  | EApp of annotation * ( Level.t ) * term * term
  | ELambda of annotation * lambda_abs
  | ELet of annotation * let_abs
  | ELetRec of annotation * rec_abs
  | EConstant of annotation * primitive
  | EMatch of annotation * term * clause list
  | EAnnot of annotation * term * ty

 and binder = 
  | AnnotatedBind of var * ty
  | Bind of var

 and clause = 
  clause_abs

 and pattern = 
  | PVar of annotation * var
  | PDataCon of annotation * constructor * pattern list

 and constructor = 
  var

 and primitive = 
  | Int of ( int )
  | String of ( string )

 and ty = 
  | TyArrow of annotation * ty * ty
  | TyApp of annotation * var * ty list
  | TyVar of annotation * var
  | TyConstant of annotation * ty_constant

 and ty_constant = 
  | TyInt
  | TyString

 and type_definition = 
  | AlgebraicDataType of datatype_def list

 and datatype_def = 
  | DataType of var * ty list

 and toplevel_abs = 
  toplevel_definition * program

 and lambda_abs = 
  binder * term

 and let_abs = 
  var * term * term

 and rec_abs = 
  var * term * term

 and clause_abs = 
  annotation * pattern * term

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

type var =
  Var.Atom.t

 and annotation = 
  ( Positions.t )

 and program = 
  | EmptyProgram
  | NewDefinition of annotation * opaque_toplevel_abs

 and toplevel_definition = 
  | TypeDefinition of var * param list * type_definition
  | ValDefinition of var * term
  | RecDefinitions of var * term

 and param = 
  var

 and term = 
  | EVar of annotation * var
  | EApp of annotation * ( Level.t ) * term * term
  | ELambda of annotation * opaque_lambda_abs
  | ELet of annotation * opaque_let_abs
  | ELetRec of annotation * opaque_rec_abs
  | EConstant of annotation * primitive
  | EMatch of annotation * term * clause list
  | EAnnot of annotation * term * ty

 and binder = 
  | AnnotatedBind of var * ty
  | Bind of var

 and clause = 
  opaque_clause_abs

 and pattern = 
  | PVar of annotation * var
  | PDataCon of annotation * constructor * pattern list

 and constructor = 
  var

 and primitive = 
  | Int of ( int )
  | String of ( string )

 and ty = 
  | TyArrow of annotation * ty * ty
  | TyApp of annotation * var * ty list
  | TyVar of annotation * var
  | TyConstant of annotation * ty_constant

 and ty_constant = 
  | TyInt
  | TyString

 and type_definition = 
  | AlgebraicDataType of datatype_def list

 and datatype_def = 
  | DataType of var * ty list

 and toplevel_abs = 
  toplevel_definition * program

 and opaque_toplevel_abs

 and lambda_abs = 
  binder * term

 and opaque_lambda_abs

 and let_abs = 
  var * term * term

 and opaque_let_abs

 and rec_abs = 
  var * term * term

 and opaque_rec_abs

 and clause_abs = 
  annotation * pattern * term

 and opaque_clause_abs

(* The following functions operate over the expression type "annotation". *)

(*
 * Function:   import_annotation
 * Summary:    converts raw forms into internal forms
 * Parameters: an expression in raw form
 * Results:    an expression in internal form
 *               (semantically equivalent to the input expression)
 *)

val import_annotation : unit -> Raw.annotation -> annotation

(*
 * Function:   subst_annotation
 * Summary:    substitutes atoms for atoms
 * Parameters: an expression in internal form
 * Results:    an expression in internal form
 *               (the result of applying these substitutions to the input expression)
 *)

val subst_annotation : unit -> annotation -> annotation

(*
 * Function:   export_annotation
 * Summary:    converts internal forms to raw forms
 * Parameters: an expression in internal form
 * Results:    an expression in raw form
 *               (semantically equivalent to the input expression)
 *               (raises [Foo.Atom.Unknown] if an unknown atom of sort "foo" is encountered
 *)

val export_annotation : unit -> annotation -> Raw.annotation

(*
 * Function:   free_annotation
 * Summary:    collects free atoms
 * Parameters: an expression in internal form
 * Results:    
 *)

val free_annotation : annotation -> unit

(* The following functions operate over the expression type "program". *)

(*
 * Function:   import_program
 * Summary:    converts raw forms into internal forms
 * Parameters: a mapping of identifiers to atoms of sort var
 *             an expression in raw form
 * Results:    an expression in internal form
 *               (semantically equivalent to the input expression)
 *)

val import_program : var Identifier.Map.t -> Raw.program -> program

(*
 * Function:   subst_program
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *             an expression in internal form
 * Results:    an expression in internal form
 *               (the result of applying these substitutions to the input expression)
 *)

val subst_program : Var.Subst.t -> program -> program

(*
 * Function:   export_program
 * Summary:    converts internal forms to raw forms
 * Parameters: a mapping of atoms of sort var to identifiers
 *             an expression in internal form
 * Results:    an expression in raw form
 *               (semantically equivalent to the input expression)
 *               (raises [Foo.Atom.Unknown] if an unknown atom of sort "foo" is encountered
 *)

val export_program : Var.AtomIdMap.t -> program -> Raw.program

(*
 * Function:   free_program
 * Summary:    collects free atoms
 * Parameters: an expression in internal form
 * Results:    the set of all atoms of sort var that occur free in the input expression
 *)

val free_program : program -> Var.AtomSet.t

(* The following functions operate over the pattern type "toplevel_definition". *)

(*
 * Function:   subst_toplevel_definition
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to sub-expressions in outer scope)
 *             a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_toplevel_definition : Var.Subst.t * Var.Subst.t -> toplevel_definition -> toplevel_definition

(*
 * Function:   bound_toplevel_definition
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_toplevel_definition : toplevel_definition -> Var.AtomSet.t

(*
 * Function:   bound_free_toplevel_definition
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in inner scope in the input pattern 
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in outer scope in the input pattern 
 *)

val bound_free_toplevel_definition : toplevel_definition -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t

(*
 * Function:   export_toplevel_definition
 * Summary:    
 * Parameters: 
 * Results:    
 *)

val export_toplevel_definition : Var.AtomIdMap.t * Var.AtomIdMap.t -> toplevel_definition -> Raw.toplevel_definition

(* The following functions operate over the pattern type "param". *)

(*
 * Function:   subst_param
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_param : Var.Subst.t -> param -> param

(*
 * Function:   bound_param
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_param : param -> Var.AtomSet.t

(*
 * Function:   bound_free_param
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_free_param : param -> Var.AtomSet.t

(*
 * Function:   export_param
 * Summary:    
 * Parameters: 
 * Results:    
 *)

val export_param : Var.AtomIdMap.t -> param -> Raw.param

(* The following functions operate over the expression type "term". *)

(*
 * Function:   import_term
 * Summary:    converts raw forms into internal forms
 * Parameters: a mapping of identifiers to atoms of sort var
 *             an expression in raw form
 * Results:    an expression in internal form
 *               (semantically equivalent to the input expression)
 *)

val import_term : var Identifier.Map.t -> Raw.term -> term

(*
 * Function:   subst_term
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *             an expression in internal form
 * Results:    an expression in internal form
 *               (the result of applying these substitutions to the input expression)
 *)

val subst_term : Var.Subst.t -> term -> term

(*
 * Function:   export_term
 * Summary:    converts internal forms to raw forms
 * Parameters: a mapping of atoms of sort var to identifiers
 *             an expression in internal form
 * Results:    an expression in raw form
 *               (semantically equivalent to the input expression)
 *               (raises [Foo.Atom.Unknown] if an unknown atom of sort "foo" is encountered
 *)

val export_term : Var.AtomIdMap.t -> term -> Raw.term

(*
 * Function:   free_term
 * Summary:    collects free atoms
 * Parameters: an expression in internal form
 * Results:    the set of all atoms of sort var that occur free in the input expression
 *)

val free_term : term -> Var.AtomSet.t

(* The following functions operate over the pattern type "binder". *)

(*
 * Function:   subst_binder
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to sub-expressions in outer scope)
 *             a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_binder : Var.Subst.t * Var.Subst.t -> binder -> binder

(*
 * Function:   bound_binder
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_binder : binder -> Var.AtomSet.t

(*
 * Function:   bound_free_binder
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in outer scope in the input pattern 
 *)

val bound_free_binder : binder -> Var.AtomSet.t * Var.AtomSet.t

(*
 * Function:   export_binder
 * Summary:    
 * Parameters: 
 * Results:    
 *)

val export_binder : Var.AtomIdMap.t * Var.AtomIdMap.t -> binder -> Raw.binder

(* The following functions operate over the expression type "clause". *)

(*
 * Function:   import_clause
 * Summary:    converts raw forms into internal forms
 * Parameters: a mapping of identifiers to atoms of sort var
 *             an expression in raw form
 * Results:    an expression in internal form
 *               (semantically equivalent to the input expression)
 *)

val import_clause : var Identifier.Map.t -> Raw.clause -> clause

(*
 * Function:   subst_clause
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *             an expression in internal form
 * Results:    an expression in internal form
 *               (the result of applying these substitutions to the input expression)
 *)

val subst_clause : Var.Subst.t -> clause -> clause

(*
 * Function:   export_clause
 * Summary:    converts internal forms to raw forms
 * Parameters: a mapping of atoms of sort var to identifiers
 *             an expression in internal form
 * Results:    an expression in raw form
 *               (semantically equivalent to the input expression)
 *               (raises [Foo.Atom.Unknown] if an unknown atom of sort "foo" is encountered
 *)

val export_clause : Var.AtomIdMap.t -> clause -> Raw.clause

(*
 * Function:   free_clause
 * Summary:    collects free atoms
 * Parameters: an expression in internal form
 * Results:    the set of all atoms of sort var that occur free in the input expression
 *)

val free_clause : clause -> Var.AtomSet.t

(* The following functions operate over the pattern type "pattern". *)

(*
 * Function:   subst_pattern
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to sub-expressions in outer scope)
 *             a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_pattern : Var.Subst.t * Var.Subst.t -> pattern -> pattern

(*
 * Function:   bound_pattern
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_pattern : pattern -> Var.AtomSet.t

(*
 * Function:   bound_free_pattern
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in outer scope in the input pattern 
 *)

val bound_free_pattern : pattern -> Var.AtomSet.t * Var.AtomSet.t

(*
 * Function:   export_pattern
 * Summary:    
 * Parameters: 
 * Results:    
 *)

val export_pattern : Var.AtomIdMap.t * Var.AtomIdMap.t -> pattern -> Raw.pattern

(* The following functions operate over the expression type "constructor". *)

(*
 * Function:   import_constructor
 * Summary:    converts raw forms into internal forms
 * Parameters: a mapping of identifiers to atoms of sort var
 *             an expression in raw form
 * Results:    an expression in internal form
 *               (semantically equivalent to the input expression)
 *)

val import_constructor : var Identifier.Map.t -> Raw.constructor -> constructor

(*
 * Function:   subst_constructor
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *             an expression in internal form
 * Results:    an expression in internal form
 *               (the result of applying these substitutions to the input expression)
 *)

val subst_constructor : Var.Subst.t -> constructor -> constructor

(*
 * Function:   export_constructor
 * Summary:    converts internal forms to raw forms
 * Parameters: a mapping of atoms of sort var to identifiers
 *             an expression in internal form
 * Results:    an expression in raw form
 *               (semantically equivalent to the input expression)
 *               (raises [Foo.Atom.Unknown] if an unknown atom of sort "foo" is encountered
 *)

val export_constructor : Var.AtomIdMap.t -> constructor -> Raw.constructor

(*
 * Function:   free_constructor
 * Summary:    collects free atoms
 * Parameters: an expression in internal form
 * Results:    the set of all atoms of sort var that occur free in the input expression
 *)

val free_constructor : constructor -> Var.AtomSet.t

(* The following functions operate over the expression type "primitive". *)

(*
 * Function:   import_primitive
 * Summary:    converts raw forms into internal forms
 * Parameters: an expression in raw form
 * Results:    an expression in internal form
 *               (semantically equivalent to the input expression)
 *)

val import_primitive : unit -> Raw.primitive -> primitive

(*
 * Function:   subst_primitive
 * Summary:    substitutes atoms for atoms
 * Parameters: an expression in internal form
 * Results:    an expression in internal form
 *               (the result of applying these substitutions to the input expression)
 *)

val subst_primitive : unit -> primitive -> primitive

(*
 * Function:   export_primitive
 * Summary:    converts internal forms to raw forms
 * Parameters: an expression in internal form
 * Results:    an expression in raw form
 *               (semantically equivalent to the input expression)
 *               (raises [Foo.Atom.Unknown] if an unknown atom of sort "foo" is encountered
 *)

val export_primitive : unit -> primitive -> Raw.primitive

(*
 * Function:   free_primitive
 * Summary:    collects free atoms
 * Parameters: an expression in internal form
 * Results:    
 *)

val free_primitive : primitive -> unit

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

(* The following functions operate over the pattern type "type_definition". *)

(*
 * Function:   subst_type_definition
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_type_definition : Var.Subst.t -> type_definition -> type_definition

(*
 * Function:   bound_type_definition
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_type_definition : type_definition -> Var.AtomSet.t

(*
 * Function:   bound_free_type_definition
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in inner scope in the input pattern 
 *)

val bound_free_type_definition : type_definition -> Var.AtomSet.t * Var.AtomSet.t

(*
 * Function:   export_type_definition
 * Summary:    
 * Parameters: 
 * Results:    
 *)

val export_type_definition : Var.AtomIdMap.t -> type_definition -> Raw.type_definition

(* The following functions operate over the pattern type "datatype_def". *)

(*
 * Function:   subst_datatype_def
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_datatype_def : Var.Subst.t -> datatype_def -> datatype_def

(*
 * Function:   bound_datatype_def
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_datatype_def : datatype_def -> Var.AtomSet.t

(*
 * Function:   bound_free_datatype_def
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in inner scope in the input pattern 
 *)

val bound_free_datatype_def : datatype_def -> Var.AtomSet.t * Var.AtomSet.t

(*
 * Function:   export_datatype_def
 * Summary:    
 * Parameters: 
 * Results:    
 *)

val export_datatype_def : Var.AtomIdMap.t -> datatype_def -> Raw.datatype_def

(* The following functions operate over the pattern type "toplevel_abs". *)

(*
 * Function:   subst_toplevel_abs
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to sub-expressions in outer scope)
 *             a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_toplevel_abs : Var.Subst.t * Var.Subst.t -> toplevel_abs -> toplevel_abs

(*
 * Function:   bound_toplevel_abs
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_toplevel_abs : toplevel_abs -> Var.AtomSet.t

(*
 * Function:   bound_free_toplevel_abs
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in inner scope in the input pattern 
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in outer scope in the input pattern 
 *)

val bound_free_toplevel_abs : toplevel_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t

(*
 * Function:   export_toplevel_abs
 * Summary:    
 * Parameters: 
 * Results:    
 *)

val export_toplevel_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> toplevel_abs -> Raw.toplevel_abs

(*
 * Function:   create_toplevel_abs
 * Summary:    creates an opaque abstraction
 * Parameters: a pattern in internal form
 *               (a transparent version of the abstraction)
 * Results:    an abstract data structure
 *               (an opaque version of the abstraction)
 *)

val create_toplevel_abs : toplevel_abs -> opaque_toplevel_abs

(*
 * Function:   open_toplevel_abs
 * Summary:    opens an opaque abstraction
 * Parameters: an abstract data structure
 *               (an opaque version of the abstraction)
 * Results:    a pattern in internal form
 *               (a transparent version of the abstraction, where all bound atoms are freshly renamed)
 *)

val open_toplevel_abs : opaque_toplevel_abs -> toplevel_abs

(*
 * Function:   open2_toplevel_abs
 * Summary:    opens two opaque abstractions, if they have the same structure
 * Parameters: two abstract data structures
 *               (two opaque versions of the abstraction)
 * Results:    the two patterns in internal form
 *               (two transparent versions of the abstraction,
 *              where all bound atoms are freshly renamed with the same names)
 *)

val open2_toplevel_abs : opaque_toplevel_abs -> opaque_toplevel_abs -> toplevel_abs * toplevel_abs

(* The following functions operate over the pattern type "lambda_abs". *)

(*
 * Function:   subst_lambda_abs
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to sub-expressions in outer scope)
 *             a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_lambda_abs : Var.Subst.t * Var.Subst.t -> lambda_abs -> lambda_abs

(*
 * Function:   bound_lambda_abs
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_lambda_abs : lambda_abs -> Var.AtomSet.t

(*
 * Function:   bound_free_lambda_abs
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in inner scope in the input pattern 
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in outer scope in the input pattern 
 *)

val bound_free_lambda_abs : lambda_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t

(*
 * Function:   export_lambda_abs
 * Summary:    
 * Parameters: 
 * Results:    
 *)

val export_lambda_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> lambda_abs -> Raw.lambda_abs

(*
 * Function:   create_lambda_abs
 * Summary:    creates an opaque abstraction
 * Parameters: a pattern in internal form
 *               (a transparent version of the abstraction)
 * Results:    an abstract data structure
 *               (an opaque version of the abstraction)
 *)

val create_lambda_abs : lambda_abs -> opaque_lambda_abs

(*
 * Function:   open_lambda_abs
 * Summary:    opens an opaque abstraction
 * Parameters: an abstract data structure
 *               (an opaque version of the abstraction)
 * Results:    a pattern in internal form
 *               (a transparent version of the abstraction, where all bound atoms are freshly renamed)
 *)

val open_lambda_abs : opaque_lambda_abs -> lambda_abs

(*
 * Function:   open2_lambda_abs
 * Summary:    opens two opaque abstractions, if they have the same structure
 * Parameters: two abstract data structures
 *               (two opaque versions of the abstraction)
 * Results:    the two patterns in internal form
 *               (two transparent versions of the abstraction,
 *              where all bound atoms are freshly renamed with the same names)
 *)

val open2_lambda_abs : opaque_lambda_abs -> opaque_lambda_abs -> lambda_abs * lambda_abs

(* The following functions operate over the pattern type "let_abs". *)

(*
 * Function:   subst_let_abs
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to sub-expressions in outer scope)
 *             a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_let_abs : Var.Subst.t * Var.Subst.t -> let_abs -> let_abs

(*
 * Function:   bound_let_abs
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_let_abs : let_abs -> Var.AtomSet.t

(*
 * Function:   bound_free_let_abs
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in inner scope in the input pattern 
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in outer scope in the input pattern 
 *)

val bound_free_let_abs : let_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t

(*
 * Function:   export_let_abs
 * Summary:    
 * Parameters: 
 * Results:    
 *)

val export_let_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> let_abs -> Raw.let_abs

(*
 * Function:   create_let_abs
 * Summary:    creates an opaque abstraction
 * Parameters: a pattern in internal form
 *               (a transparent version of the abstraction)
 * Results:    an abstract data structure
 *               (an opaque version of the abstraction)
 *)

val create_let_abs : let_abs -> opaque_let_abs

(*
 * Function:   open_let_abs
 * Summary:    opens an opaque abstraction
 * Parameters: an abstract data structure
 *               (an opaque version of the abstraction)
 * Results:    a pattern in internal form
 *               (a transparent version of the abstraction, where all bound atoms are freshly renamed)
 *)

val open_let_abs : opaque_let_abs -> let_abs

(*
 * Function:   open2_let_abs
 * Summary:    opens two opaque abstractions, if they have the same structure
 * Parameters: two abstract data structures
 *               (two opaque versions of the abstraction)
 * Results:    the two patterns in internal form
 *               (two transparent versions of the abstraction,
 *              where all bound atoms are freshly renamed with the same names)
 *)

val open2_let_abs : opaque_let_abs -> opaque_let_abs -> let_abs * let_abs

(* The following functions operate over the pattern type "rec_abs". *)

(*
 * Function:   subst_rec_abs
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_rec_abs : Var.Subst.t -> rec_abs -> rec_abs

(*
 * Function:   bound_rec_abs
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_rec_abs : rec_abs -> Var.AtomSet.t

(*
 * Function:   bound_free_rec_abs
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in inner scope in the input pattern 
 *)

val bound_free_rec_abs : rec_abs -> Var.AtomSet.t * Var.AtomSet.t

(*
 * Function:   export_rec_abs
 * Summary:    
 * Parameters: 
 * Results:    
 *)

val export_rec_abs : Var.AtomIdMap.t -> rec_abs -> Raw.rec_abs

(*
 * Function:   create_rec_abs
 * Summary:    creates an opaque abstraction
 * Parameters: a pattern in internal form
 *               (a transparent version of the abstraction)
 * Results:    an abstract data structure
 *               (an opaque version of the abstraction)
 *)

val create_rec_abs : rec_abs -> opaque_rec_abs

(*
 * Function:   open_rec_abs
 * Summary:    opens an opaque abstraction
 * Parameters: an abstract data structure
 *               (an opaque version of the abstraction)
 * Results:    a pattern in internal form
 *               (a transparent version of the abstraction, where all bound atoms are freshly renamed)
 *)

val open_rec_abs : opaque_rec_abs -> rec_abs

(*
 * Function:   open2_rec_abs
 * Summary:    opens two opaque abstractions, if they have the same structure
 * Parameters: two abstract data structures
 *               (two opaque versions of the abstraction)
 * Results:    the two patterns in internal form
 *               (two transparent versions of the abstraction,
 *              where all bound atoms are freshly renamed with the same names)
 *)

val open2_rec_abs : opaque_rec_abs -> opaque_rec_abs -> rec_abs * rec_abs

(* The following functions operate over the pattern type "clause_abs". *)

(*
 * Function:   subst_clause_abs
 * Summary:    substitutes atoms for atoms
 * Parameters: a substitution of atoms for atoms at sort var
 *               (to be applied to sub-expressions in outer scope)
 *             a substitution of atoms for atoms at sort var
 *               (to be applied to atoms in binding position and to sub-expressions in inner scope)
 *             a pattern in internal form
 * Results:    a pattern in internal form
 *               (the result of applying these substitutions to the input pattern)
 *)

val subst_clause_abs : Var.Subst.t * Var.Subst.t -> clause_abs -> clause_abs

(*
 * Function:   bound_clause_abs
 * Summary:    collects atoms in a binding position
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *)

val bound_clause_abs : clause_abs -> Var.AtomSet.t

(*
 * Function:   bound_free_clause_abs
 * Summary:    collects atoms in a binding position and free atoms inside sub-expressions
 * Parameters: a pattern in internal form
 * Results:    the set of all atoms of sort var that occur in a binding position in the input pattern
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in inner scope in the input pattern 
 *             the set of all atoms of sort var that occur free in a
 *               sub-expression that lies in outer scope in the input pattern 
 *)

val bound_free_clause_abs : clause_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t

(*
 * Function:   export_clause_abs
 * Summary:    
 * Parameters: 
 * Results:    
 *)

val export_clause_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> clause_abs -> Raw.clause_abs

(*
 * Function:   create_clause_abs
 * Summary:    creates an opaque abstraction
 * Parameters: a pattern in internal form
 *               (a transparent version of the abstraction)
 * Results:    an abstract data structure
 *               (an opaque version of the abstraction)
 *)

val create_clause_abs : clause_abs -> opaque_clause_abs

(*
 * Function:   open_clause_abs
 * Summary:    opens an opaque abstraction
 * Parameters: an abstract data structure
 *               (an opaque version of the abstraction)
 * Results:    a pattern in internal form
 *               (a transparent version of the abstraction, where all bound atoms are freshly renamed)
 *)

val open_clause_abs : opaque_clause_abs -> clause_abs

(*
 * Function:   open2_clause_abs
 * Summary:    opens two opaque abstractions, if they have the same structure
 * Parameters: two abstract data structures
 *               (two opaque versions of the abstraction)
 * Results:    the two patterns in internal form
 *               (two transparent versions of the abstraction,
 *              where all bound atoms are freshly renamed with the same names)
 *)

val open2_clause_abs : opaque_clause_abs -> opaque_clause_abs -> clause_abs * clause_abs

(* The following class contains code that ``transforms'' a data structure.
   The methods provided in this class implement an identity transformation,
   that is, they traverse the data structure and produce a semantically
   equivalent copy of it. The intended use of this class is for the client
   to create a subclass and override one or several methods so as to obtain
   nontrivial behavior. *)

class map : object

  (*
   * Method:     annotation
   * Summary:    transforms an expression
   * Parameters: an expression in internal form
   * Results:    an expression in internal form
   *               (the result of applying the transformation to the input expression)
   *)

  method annotation : annotation -> annotation

  (*
   * Method:     program
   * Summary:    transforms an expression
   * Parameters: an expression in internal form
   * Results:    an expression in internal form
   *               (the result of applying the transformation to the input expression)
   *)

  method program : program -> program

  (*
   * Method:     emptyprogram
   * Summary:    transforms under the data constructor "EmptyProgram"
   * Parameters: a tuple of the parameters to "EmptyProgram"
   * Results:    the result of applying "EmptyProgram" to the transformed parameters
   *)

  method emptyprogram : program

  (*
   * Method:     newdefinition
   * Summary:    transforms under the data constructor "NewDefinition"
   * Parameters: a tuple of the parameters to "NewDefinition"
   * Results:    the result of applying "NewDefinition" to the transformed parameters
   *)

  method newdefinition : annotation * opaque_toplevel_abs -> program

  (*
   * Method:     toplevel_definition
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method toplevel_definition : toplevel_definition -> toplevel_definition

  (*
   * Method:     typedefinition
   * Summary:    transforms under the data constructor "TypeDefinition"
   * Parameters: a tuple of the parameters to "TypeDefinition"
   * Results:    the result of applying "TypeDefinition" to the transformed parameters
   *)

  method typedefinition : var * param list * type_definition -> toplevel_definition

  (*
   * Method:     valdefinition
   * Summary:    transforms under the data constructor "ValDefinition"
   * Parameters: a tuple of the parameters to "ValDefinition"
   * Results:    the result of applying "ValDefinition" to the transformed parameters
   *)

  method valdefinition : var * term -> toplevel_definition

  (*
   * Method:     recdefinitions
   * Summary:    transforms under the data constructor "RecDefinitions"
   * Parameters: a tuple of the parameters to "RecDefinitions"
   * Results:    the result of applying "RecDefinitions" to the transformed parameters
   *)

  method recdefinitions : var * term -> toplevel_definition

  (*
   * Method:     param
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method param : param -> param

  (*
   * Method:     term
   * Summary:    transforms an expression
   * Parameters: an expression in internal form
   * Results:    an expression in internal form
   *               (the result of applying the transformation to the input expression)
   *)

  method term : term -> term

  (*
   * Method:     evar
   * Summary:    transforms under the data constructor "EVar"
   * Parameters: a tuple of the parameters to "EVar"
   * Results:    the result of applying "EVar" to the transformed parameters
   *)

  method evar : annotation * var -> term

  (*
   * Method:     eapp
   * Summary:    transforms under the data constructor "EApp"
   * Parameters: a tuple of the parameters to "EApp"
   * Results:    the result of applying "EApp" to the transformed parameters
   *)

  method eapp : annotation * ( Level.t ) * term * term -> term

  (*
   * Method:     elambda
   * Summary:    transforms under the data constructor "ELambda"
   * Parameters: a tuple of the parameters to "ELambda"
   * Results:    the result of applying "ELambda" to the transformed parameters
   *)

  method elambda : annotation * opaque_lambda_abs -> term

  (*
   * Method:     elet
   * Summary:    transforms under the data constructor "ELet"
   * Parameters: a tuple of the parameters to "ELet"
   * Results:    the result of applying "ELet" to the transformed parameters
   *)

  method elet : annotation * opaque_let_abs -> term

  (*
   * Method:     eletrec
   * Summary:    transforms under the data constructor "ELetRec"
   * Parameters: a tuple of the parameters to "ELetRec"
   * Results:    the result of applying "ELetRec" to the transformed parameters
   *)

  method eletrec : annotation * opaque_rec_abs -> term

  (*
   * Method:     econstant
   * Summary:    transforms under the data constructor "EConstant"
   * Parameters: a tuple of the parameters to "EConstant"
   * Results:    the result of applying "EConstant" to the transformed parameters
   *)

  method econstant : annotation * primitive -> term

  (*
   * Method:     ematch
   * Summary:    transforms under the data constructor "EMatch"
   * Parameters: a tuple of the parameters to "EMatch"
   * Results:    the result of applying "EMatch" to the transformed parameters
   *)

  method ematch : annotation * term * clause list -> term

  (*
   * Method:     eannot
   * Summary:    transforms under the data constructor "EAnnot"
   * Parameters: a tuple of the parameters to "EAnnot"
   * Results:    the result of applying "EAnnot" to the transformed parameters
   *)

  method eannot : annotation * term * ty -> term

  (*
   * Method:     binder
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method binder : binder -> binder

  (*
   * Method:     annotatedbind
   * Summary:    transforms under the data constructor "AnnotatedBind"
   * Parameters: a tuple of the parameters to "AnnotatedBind"
   * Results:    the result of applying "AnnotatedBind" to the transformed parameters
   *)

  method annotatedbind : var * ty -> binder

  (*
   * Method:     bind
   * Summary:    transforms under the data constructor "Bind"
   * Parameters: a tuple of the parameters to "Bind"
   * Results:    the result of applying "Bind" to the transformed parameters
   *)

  method bind : var -> binder

  (*
   * Method:     clause
   * Summary:    transforms an expression
   * Parameters: an expression in internal form
   * Results:    an expression in internal form
   *               (the result of applying the transformation to the input expression)
   *)

  method clause : clause -> clause

  (*
   * Method:     pattern
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method pattern : pattern -> pattern

  (*
   * Method:     pvar
   * Summary:    transforms under the data constructor "PVar"
   * Parameters: a tuple of the parameters to "PVar"
   * Results:    the result of applying "PVar" to the transformed parameters
   *)

  method pvar : annotation * var -> pattern

  (*
   * Method:     pdatacon
   * Summary:    transforms under the data constructor "PDataCon"
   * Parameters: a tuple of the parameters to "PDataCon"
   * Results:    the result of applying "PDataCon" to the transformed parameters
   *)

  method pdatacon : annotation * constructor * pattern list -> pattern

  (*
   * Method:     constructor
   * Summary:    transforms an expression
   * Parameters: an expression in internal form
   * Results:    an expression in internal form
   *               (the result of applying the transformation to the input expression)
   *)

  method constructor : constructor -> constructor

  (*
   * Method:     primitive
   * Summary:    transforms an expression
   * Parameters: an expression in internal form
   * Results:    an expression in internal form
   *               (the result of applying the transformation to the input expression)
   *)

  method primitive : primitive -> primitive

  (*
   * Method:     int
   * Summary:    transforms under the data constructor "Int"
   * Parameters: a tuple of the parameters to "Int"
   * Results:    the result of applying "Int" to the transformed parameters
   *)

  method int : ( int ) -> primitive

  (*
   * Method:     string
   * Summary:    transforms under the data constructor "String"
   * Parameters: a tuple of the parameters to "String"
   * Results:    the result of applying "String" to the transformed parameters
   *)

  method string : ( string ) -> primitive

  (*
   * Method:     ty
   * Summary:    transforms an expression
   * Parameters: an expression in internal form
   * Results:    an expression in internal form
   *               (the result of applying the transformation to the input expression)
   *)

  method ty : ty -> ty

  (*
   * Method:     tyarrow
   * Summary:    transforms under the data constructor "TyArrow"
   * Parameters: a tuple of the parameters to "TyArrow"
   * Results:    the result of applying "TyArrow" to the transformed parameters
   *)

  method tyarrow : annotation * ty * ty -> ty

  (*
   * Method:     tyapp
   * Summary:    transforms under the data constructor "TyApp"
   * Parameters: a tuple of the parameters to "TyApp"
   * Results:    the result of applying "TyApp" to the transformed parameters
   *)

  method tyapp : annotation * var * ty list -> ty

  (*
   * Method:     tyvar
   * Summary:    transforms under the data constructor "TyVar"
   * Parameters: a tuple of the parameters to "TyVar"
   * Results:    the result of applying "TyVar" to the transformed parameters
   *)

  method tyvar : annotation * var -> ty

  (*
   * Method:     tyconstant
   * Summary:    transforms under the data constructor "TyConstant"
   * Parameters: a tuple of the parameters to "TyConstant"
   * Results:    the result of applying "TyConstant" to the transformed parameters
   *)

  method tyconstant : annotation * ty_constant -> ty

  (*
   * Method:     ty_constant
   * Summary:    transforms an expression
   * Parameters: an expression in internal form
   * Results:    an expression in internal form
   *               (the result of applying the transformation to the input expression)
   *)

  method ty_constant : ty_constant -> ty_constant

  (*
   * Method:     tyint
   * Summary:    transforms under the data constructor "TyInt"
   * Parameters: a tuple of the parameters to "TyInt"
   * Results:    the result of applying "TyInt" to the transformed parameters
   *)

  method tyint : ty_constant

  (*
   * Method:     tystring
   * Summary:    transforms under the data constructor "TyString"
   * Parameters: a tuple of the parameters to "TyString"
   * Results:    the result of applying "TyString" to the transformed parameters
   *)

  method tystring : ty_constant

  (*
   * Method:     type_definition
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method type_definition : type_definition -> type_definition

  (*
   * Method:     algebraicdatatype
   * Summary:    transforms under the data constructor "AlgebraicDataType"
   * Parameters: a tuple of the parameters to "AlgebraicDataType"
   * Results:    the result of applying "AlgebraicDataType" to the transformed parameters
   *)

  method algebraicdatatype : datatype_def list -> type_definition

  (*
   * Method:     datatype_def
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method datatype_def : datatype_def -> datatype_def

  (*
   * Method:     datatype
   * Summary:    transforms under the data constructor "DataType"
   * Parameters: a tuple of the parameters to "DataType"
   * Results:    the result of applying "DataType" to the transformed parameters
   *)

  method datatype : var * ty list -> datatype_def

  (*
   * Method:     toplevel_abs
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method toplevel_abs : toplevel_abs -> toplevel_abs

  (*
   * Method:     lambda_abs
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method lambda_abs : lambda_abs -> lambda_abs

  (*
   * Method:     let_abs
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method let_abs : let_abs -> let_abs

  (*
   * Method:     rec_abs
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method rec_abs : rec_abs -> rec_abs

  (*
   * Method:     clause_abs
   * Summary:    transforms a pattern
   * Parameters: a pattern in internal form
   * Results:    a pattern in internal form
   *               (the result of applying the transformation to the input pattern)
   *)

  method clause_abs : clause_abs -> clause_abs

end

(* The following class contains code that iterates over a data structure
   while updating an accumulator. The methods provided in this class
   implement an identity over the accumulator, that is, they traverse the
   data structure and always return the initial accumulator. The intended
   use of this class is for the client to create a subclass and override
   one or several methods so as to obtain nontrivial behavior. *)

class [ 'accumulator ] fold : object

  (*
   * Method:     annotation
   * Summary:    iterates over an expression
   * Parameters: an accumulator
   *             an expression in internal form
   * Results:    an updated accumulator, obtained by traversing the input expression
   *)

  method annotation : 'accumulator -> annotation -> 'accumulator

  (*
   * Method:     program
   * Summary:    iterates over an expression
   * Parameters: an accumulator
   *             an expression in internal form
   * Results:    an updated accumulator, obtained by traversing the input expression
   *)

  method program : 'accumulator -> program -> 'accumulator

  (*
   * Method:     emptyprogram
   * Summary:    iterates under the data constructor "EmptyProgram"
   * Parameters: an accumulator
   *             a tuple of the parameters to "EmptyProgram"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method emptyprogram : 'accumulator -> 'accumulator

  (*
   * Method:     newdefinition
   * Summary:    iterates under the data constructor "NewDefinition"
   * Parameters: an accumulator
   *             a tuple of the parameters to "NewDefinition"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method newdefinition : 'accumulator -> annotation * opaque_toplevel_abs -> 'accumulator

  (*
   * Method:     toplevel_definition
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method toplevel_definition : 'accumulator -> toplevel_definition -> 'accumulator

  (*
   * Method:     typedefinition
   * Summary:    iterates under the data constructor "TypeDefinition"
   * Parameters: an accumulator
   *             a tuple of the parameters to "TypeDefinition"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method typedefinition : 'accumulator -> var * param list * type_definition -> 'accumulator

  (*
   * Method:     valdefinition
   * Summary:    iterates under the data constructor "ValDefinition"
   * Parameters: an accumulator
   *             a tuple of the parameters to "ValDefinition"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method valdefinition : 'accumulator -> var * term -> 'accumulator

  (*
   * Method:     recdefinitions
   * Summary:    iterates under the data constructor "RecDefinitions"
   * Parameters: an accumulator
   *             a tuple of the parameters to "RecDefinitions"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method recdefinitions : 'accumulator -> var * term -> 'accumulator

  (*
   * Method:     param
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method param : 'accumulator -> param -> 'accumulator

  (*
   * Method:     term
   * Summary:    iterates over an expression
   * Parameters: an accumulator
   *             an expression in internal form
   * Results:    an updated accumulator, obtained by traversing the input expression
   *)

  method term : 'accumulator -> term -> 'accumulator

  (*
   * Method:     evar
   * Summary:    iterates under the data constructor "EVar"
   * Parameters: an accumulator
   *             a tuple of the parameters to "EVar"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method evar : 'accumulator -> annotation * var -> 'accumulator

  (*
   * Method:     eapp
   * Summary:    iterates under the data constructor "EApp"
   * Parameters: an accumulator
   *             a tuple of the parameters to "EApp"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method eapp : 'accumulator -> annotation * ( Level.t ) * term * term -> 'accumulator

  (*
   * Method:     elambda
   * Summary:    iterates under the data constructor "ELambda"
   * Parameters: an accumulator
   *             a tuple of the parameters to "ELambda"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method elambda : 'accumulator -> annotation * opaque_lambda_abs -> 'accumulator

  (*
   * Method:     elet
   * Summary:    iterates under the data constructor "ELet"
   * Parameters: an accumulator
   *             a tuple of the parameters to "ELet"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method elet : 'accumulator -> annotation * opaque_let_abs -> 'accumulator

  (*
   * Method:     eletrec
   * Summary:    iterates under the data constructor "ELetRec"
   * Parameters: an accumulator
   *             a tuple of the parameters to "ELetRec"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method eletrec : 'accumulator -> annotation * opaque_rec_abs -> 'accumulator

  (*
   * Method:     econstant
   * Summary:    iterates under the data constructor "EConstant"
   * Parameters: an accumulator
   *             a tuple of the parameters to "EConstant"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method econstant : 'accumulator -> annotation * primitive -> 'accumulator

  (*
   * Method:     ematch
   * Summary:    iterates under the data constructor "EMatch"
   * Parameters: an accumulator
   *             a tuple of the parameters to "EMatch"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ematch : 'accumulator -> annotation * term * clause list -> 'accumulator

  (*
   * Method:     eannot
   * Summary:    iterates under the data constructor "EAnnot"
   * Parameters: an accumulator
   *             a tuple of the parameters to "EAnnot"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method eannot : 'accumulator -> annotation * term * ty -> 'accumulator

  (*
   * Method:     binder
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method binder : 'accumulator -> binder -> 'accumulator

  (*
   * Method:     annotatedbind
   * Summary:    iterates under the data constructor "AnnotatedBind"
   * Parameters: an accumulator
   *             a tuple of the parameters to "AnnotatedBind"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method annotatedbind : 'accumulator -> var * ty -> 'accumulator

  (*
   * Method:     bind
   * Summary:    iterates under the data constructor "Bind"
   * Parameters: an accumulator
   *             a tuple of the parameters to "Bind"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method bind : 'accumulator -> var -> 'accumulator

  (*
   * Method:     clause
   * Summary:    iterates over an expression
   * Parameters: an accumulator
   *             an expression in internal form
   * Results:    an updated accumulator, obtained by traversing the input expression
   *)

  method clause : 'accumulator -> clause -> 'accumulator

  (*
   * Method:     pattern
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method pattern : 'accumulator -> pattern -> 'accumulator

  (*
   * Method:     pvar
   * Summary:    iterates under the data constructor "PVar"
   * Parameters: an accumulator
   *             a tuple of the parameters to "PVar"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method pvar : 'accumulator -> annotation * var -> 'accumulator

  (*
   * Method:     pdatacon
   * Summary:    iterates under the data constructor "PDataCon"
   * Parameters: an accumulator
   *             a tuple of the parameters to "PDataCon"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method pdatacon : 'accumulator -> annotation * constructor * pattern list -> 'accumulator

  (*
   * Method:     constructor
   * Summary:    iterates over an expression
   * Parameters: an accumulator
   *             an expression in internal form
   * Results:    an updated accumulator, obtained by traversing the input expression
   *)

  method constructor : 'accumulator -> constructor -> 'accumulator

  (*
   * Method:     primitive
   * Summary:    iterates over an expression
   * Parameters: an accumulator
   *             an expression in internal form
   * Results:    an updated accumulator, obtained by traversing the input expression
   *)

  method primitive : 'accumulator -> primitive -> 'accumulator

  (*
   * Method:     int
   * Summary:    iterates under the data constructor "Int"
   * Parameters: an accumulator
   *             a tuple of the parameters to "Int"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method int : 'accumulator -> ( int ) -> 'accumulator

  (*
   * Method:     string
   * Summary:    iterates under the data constructor "String"
   * Parameters: an accumulator
   *             a tuple of the parameters to "String"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method string : 'accumulator -> ( string ) -> 'accumulator

  (*
   * Method:     ty
   * Summary:    iterates over an expression
   * Parameters: an accumulator
   *             an expression in internal form
   * Results:    an updated accumulator, obtained by traversing the input expression
   *)

  method ty : 'accumulator -> ty -> 'accumulator

  (*
   * Method:     tyarrow
   * Summary:    iterates under the data constructor "TyArrow"
   * Parameters: an accumulator
   *             a tuple of the parameters to "TyArrow"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method tyarrow : 'accumulator -> annotation * ty * ty -> 'accumulator

  (*
   * Method:     tyapp
   * Summary:    iterates under the data constructor "TyApp"
   * Parameters: an accumulator
   *             a tuple of the parameters to "TyApp"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method tyapp : 'accumulator -> annotation * var * ty list -> 'accumulator

  (*
   * Method:     tyvar
   * Summary:    iterates under the data constructor "TyVar"
   * Parameters: an accumulator
   *             a tuple of the parameters to "TyVar"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method tyvar : 'accumulator -> annotation * var -> 'accumulator

  (*
   * Method:     tyconstant
   * Summary:    iterates under the data constructor "TyConstant"
   * Parameters: an accumulator
   *             a tuple of the parameters to "TyConstant"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method tyconstant : 'accumulator -> annotation * ty_constant -> 'accumulator

  (*
   * Method:     ty_constant
   * Summary:    iterates over an expression
   * Parameters: an accumulator
   *             an expression in internal form
   * Results:    an updated accumulator, obtained by traversing the input expression
   *)

  method ty_constant : 'accumulator -> ty_constant -> 'accumulator

  (*
   * Method:     tyint
   * Summary:    iterates under the data constructor "TyInt"
   * Parameters: an accumulator
   *             a tuple of the parameters to "TyInt"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method tyint : 'accumulator -> 'accumulator

  (*
   * Method:     tystring
   * Summary:    iterates under the data constructor "TyString"
   * Parameters: an accumulator
   *             a tuple of the parameters to "TyString"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method tystring : 'accumulator -> 'accumulator

  (*
   * Method:     type_definition
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method type_definition : 'accumulator -> type_definition -> 'accumulator

  (*
   * Method:     algebraicdatatype
   * Summary:    iterates under the data constructor "AlgebraicDataType"
   * Parameters: an accumulator
   *             a tuple of the parameters to "AlgebraicDataType"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method algebraicdatatype : 'accumulator -> datatype_def list -> 'accumulator

  (*
   * Method:     datatype_def
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method datatype_def : 'accumulator -> datatype_def -> 'accumulator

  (*
   * Method:     datatype
   * Summary:    iterates under the data constructor "DataType"
   * Parameters: an accumulator
   *             a tuple of the parameters to "DataType"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method datatype : 'accumulator -> var * ty list -> 'accumulator

  (*
   * Method:     toplevel_abs
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method toplevel_abs : 'accumulator -> toplevel_abs -> 'accumulator

  (*
   * Method:     lambda_abs
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method lambda_abs : 'accumulator -> lambda_abs -> 'accumulator

  (*
   * Method:     let_abs
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method let_abs : 'accumulator -> let_abs -> 'accumulator

  (*
   * Method:     rec_abs
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method rec_abs : 'accumulator -> rec_abs -> 'accumulator

  (*
   * Method:     clause_abs
   * Summary:    iterates over a pattern
   * Parameters: an accumulator
   *             a pattern in internal form
   * Results:    an updated accumulator, obtained by traversing the input pattern
   *)

  method clause_abs : 'accumulator -> clause_abs -> 'accumulator

end

(* The following class contains code that iterates over a data structure
   while updating an accumulator. The methods provided in this class
   implement an identity over the accumulator, that is, they traverse the
   data structure and always return the initial accumulator. The intended
   use of this class is for the client to create a subclass and override
   one or several methods so as to obtain nontrivial behavior. *)

class [ 'accumulator ] fold2 : object

  (*
   * Method:     annotation
   * Summary:    iterates over two alpha-equivalent expressions
   * Parameters: an accumulator
   *             two expressions in internal form
   * Results:    an updated accumulator, obtained by traversing the input expressions
   *)

  method annotation : 'accumulator -> annotation -> annotation -> 'accumulator

  (*
   * Method:     program
   * Summary:    iterates over two alpha-equivalent expressions
   * Parameters: an accumulator
   *             two expressions in internal form
   * Results:    an updated accumulator, obtained by traversing the input expressions
   *)

  method program : 'accumulator -> program -> program -> 'accumulator

  (*
   * Method:     emptyprogram
   * Summary:    iterates under the data constructor "EmptyProgram"
   * Parameters: an accumulator
   *             a tuple of the parameters to "EmptyProgram"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method emptyprogram : 'accumulator -> 'accumulator

  (*
   * Method:     newdefinition
   * Summary:    iterates under the data constructor "NewDefinition"
   * Parameters: an accumulator
   *             a tuple of the parameters to "NewDefinition"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method newdefinition : 'accumulator -> annotation * opaque_toplevel_abs -> annotation * opaque_toplevel_abs -> 'accumulator

  (*
   * Method:     toplevel_definition
   * Summary:    iterates over two alpha-equivalent patterns
   * Parameters: an accumulator
   *             two patterns in internal form
   * Results:    an updated accumulator, obtained by traversing the input patterns
   *)

  method toplevel_definition : 'accumulator -> toplevel_definition -> toplevel_definition -> 'accumulator

  (*
   * Method:     typedefinition
   * Summary:    iterates under the data constructor "TypeDefinition"
   * Parameters: an accumulator
   *             a tuple of the parameters to "TypeDefinition"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method typedefinition : 'accumulator -> var * param list * type_definition -> var * param list * type_definition -> 'accumulator

  (*
   * Method:     valdefinition
   * Summary:    iterates under the data constructor "ValDefinition"
   * Parameters: an accumulator
   *             a tuple of the parameters to "ValDefinition"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method valdefinition : 'accumulator -> var * term -> var * term -> 'accumulator

  (*
   * Method:     recdefinitions
   * Summary:    iterates under the data constructor "RecDefinitions"
   * Parameters: an accumulator
   *             a tuple of the parameters to "RecDefinitions"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method recdefinitions : 'accumulator -> var * term -> var * term -> 'accumulator

  (*
   * Method:     param
   * Summary:    iterates over two alpha-equivalent patterns
   * Parameters: an accumulator
   *             two patterns in internal form
   * Results:    an updated accumulator, obtained by traversing the input patterns
   *)

  method param : 'accumulator -> param -> param -> 'accumulator

  (*
   * Method:     term
   * Summary:    iterates over two alpha-equivalent expressions
   * Parameters: an accumulator
   *             two expressions in internal form
   * Results:    an updated accumulator, obtained by traversing the input expressions
   *)

  method term : 'accumulator -> term -> term -> 'accumulator

  (*
   * Method:     evar
   * Summary:    iterates under the data constructor "EVar"
   * Parameters: an accumulator
   *             a tuple of the parameters to "EVar"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method evar : 'accumulator -> annotation * var -> annotation * var -> 'accumulator

  (*
   * Method:     eapp
   * Summary:    iterates under the data constructor "EApp"
   * Parameters: an accumulator
   *             a tuple of the parameters to "EApp"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method eapp : 'accumulator -> annotation * ( Level.t ) * term * term -> annotation * ( Level.t ) * term * term -> 'accumulator

  (*
   * Method:     elambda
   * Summary:    iterates under the data constructor "ELambda"
   * Parameters: an accumulator
   *             a tuple of the parameters to "ELambda"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method elambda : 'accumulator -> annotation * opaque_lambda_abs -> annotation * opaque_lambda_abs -> 'accumulator

  (*
   * Method:     elet
   * Summary:    iterates under the data constructor "ELet"
   * Parameters: an accumulator
   *             a tuple of the parameters to "ELet"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method elet : 'accumulator -> annotation * opaque_let_abs -> annotation * opaque_let_abs -> 'accumulator

  (*
   * Method:     eletrec
   * Summary:    iterates under the data constructor "ELetRec"
   * Parameters: an accumulator
   *             a tuple of the parameters to "ELetRec"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method eletrec : 'accumulator -> annotation * opaque_rec_abs -> annotation * opaque_rec_abs -> 'accumulator

  (*
   * Method:     econstant
   * Summary:    iterates under the data constructor "EConstant"
   * Parameters: an accumulator
   *             a tuple of the parameters to "EConstant"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method econstant : 'accumulator -> annotation * primitive -> annotation * primitive -> 'accumulator

  (*
   * Method:     ematch
   * Summary:    iterates under the data constructor "EMatch"
   * Parameters: an accumulator
   *             a tuple of the parameters to "EMatch"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method ematch : 'accumulator -> annotation * term * clause list -> annotation * term * clause list -> 'accumulator

  (*
   * Method:     eannot
   * Summary:    iterates under the data constructor "EAnnot"
   * Parameters: an accumulator
   *             a tuple of the parameters to "EAnnot"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method eannot : 'accumulator -> annotation * term * ty -> annotation * term * ty -> 'accumulator

  (*
   * Method:     binder
   * Summary:    iterates over two alpha-equivalent patterns
   * Parameters: an accumulator
   *             two patterns in internal form
   * Results:    an updated accumulator, obtained by traversing the input patterns
   *)

  method binder : 'accumulator -> binder -> binder -> 'accumulator

  (*
   * Method:     annotatedbind
   * Summary:    iterates under the data constructor "AnnotatedBind"
   * Parameters: an accumulator
   *             a tuple of the parameters to "AnnotatedBind"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method annotatedbind : 'accumulator -> var * ty -> var * ty -> 'accumulator

  (*
   * Method:     bind
   * Summary:    iterates under the data constructor "Bind"
   * Parameters: an accumulator
   *             a tuple of the parameters to "Bind"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method bind : 'accumulator -> var -> var -> 'accumulator

  (*
   * Method:     clause
   * Summary:    iterates over two alpha-equivalent expressions
   * Parameters: an accumulator
   *             two expressions in internal form
   * Results:    an updated accumulator, obtained by traversing the input expressions
   *)

  method clause : 'accumulator -> clause -> clause -> 'accumulator

  (*
   * Method:     pattern
   * Summary:    iterates over two alpha-equivalent patterns
   * Parameters: an accumulator
   *             two patterns in internal form
   * Results:    an updated accumulator, obtained by traversing the input patterns
   *)

  method pattern : 'accumulator -> pattern -> pattern -> 'accumulator

  (*
   * Method:     pvar
   * Summary:    iterates under the data constructor "PVar"
   * Parameters: an accumulator
   *             a tuple of the parameters to "PVar"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method pvar : 'accumulator -> annotation * var -> annotation * var -> 'accumulator

  (*
   * Method:     pdatacon
   * Summary:    iterates under the data constructor "PDataCon"
   * Parameters: an accumulator
   *             a tuple of the parameters to "PDataCon"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method pdatacon : 'accumulator -> annotation * constructor * pattern list -> annotation * constructor * pattern list -> 'accumulator

  (*
   * Method:     constructor
   * Summary:    iterates over two alpha-equivalent expressions
   * Parameters: an accumulator
   *             two expressions in internal form
   * Results:    an updated accumulator, obtained by traversing the input expressions
   *)

  method constructor : 'accumulator -> constructor -> constructor -> 'accumulator

  (*
   * Method:     primitive
   * Summary:    iterates over two alpha-equivalent expressions
   * Parameters: an accumulator
   *             two expressions in internal form
   * Results:    an updated accumulator, obtained by traversing the input expressions
   *)

  method primitive : 'accumulator -> primitive -> primitive -> 'accumulator

  (*
   * Method:     int
   * Summary:    iterates under the data constructor "Int"
   * Parameters: an accumulator
   *             a tuple of the parameters to "Int"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method int : 'accumulator -> ( int ) -> ( int ) -> 'accumulator

  (*
   * Method:     string
   * Summary:    iterates under the data constructor "String"
   * Parameters: an accumulator
   *             a tuple of the parameters to "String"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method string : 'accumulator -> ( string ) -> ( string ) -> 'accumulator

  (*
   * Method:     ty
   * Summary:    iterates over two alpha-equivalent expressions
   * Parameters: an accumulator
   *             two expressions in internal form
   * Results:    an updated accumulator, obtained by traversing the input expressions
   *)

  method ty : 'accumulator -> ty -> ty -> 'accumulator

  (*
   * Method:     tyarrow
   * Summary:    iterates under the data constructor "TyArrow"
   * Parameters: an accumulator
   *             a tuple of the parameters to "TyArrow"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method tyarrow : 'accumulator -> annotation * ty * ty -> annotation * ty * ty -> 'accumulator

  (*
   * Method:     tyapp
   * Summary:    iterates under the data constructor "TyApp"
   * Parameters: an accumulator
   *             a tuple of the parameters to "TyApp"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method tyapp : 'accumulator -> annotation * var * ty list -> annotation * var * ty list -> 'accumulator

  (*
   * Method:     tyvar
   * Summary:    iterates under the data constructor "TyVar"
   * Parameters: an accumulator
   *             a tuple of the parameters to "TyVar"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method tyvar : 'accumulator -> annotation * var -> annotation * var -> 'accumulator

  (*
   * Method:     tyconstant
   * Summary:    iterates under the data constructor "TyConstant"
   * Parameters: an accumulator
   *             a tuple of the parameters to "TyConstant"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method tyconstant : 'accumulator -> annotation * ty_constant -> annotation * ty_constant -> 'accumulator

  (*
   * Method:     ty_constant
   * Summary:    iterates over two alpha-equivalent expressions
   * Parameters: an accumulator
   *             two expressions in internal form
   * Results:    an updated accumulator, obtained by traversing the input expressions
   *)

  method ty_constant : 'accumulator -> ty_constant -> ty_constant -> 'accumulator

  (*
   * Method:     tyint
   * Summary:    iterates under the data constructor "TyInt"
   * Parameters: an accumulator
   *             a tuple of the parameters to "TyInt"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method tyint : 'accumulator -> 'accumulator

  (*
   * Method:     tystring
   * Summary:    iterates under the data constructor "TyString"
   * Parameters: an accumulator
   *             a tuple of the parameters to "TyString"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method tystring : 'accumulator -> 'accumulator

  (*
   * Method:     type_definition
   * Summary:    iterates over two alpha-equivalent patterns
   * Parameters: an accumulator
   *             two patterns in internal form
   * Results:    an updated accumulator, obtained by traversing the input patterns
   *)

  method type_definition : 'accumulator -> type_definition -> type_definition -> 'accumulator

  (*
   * Method:     algebraicdatatype
   * Summary:    iterates under the data constructor "AlgebraicDataType"
   * Parameters: an accumulator
   *             a tuple of the parameters to "AlgebraicDataType"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method algebraicdatatype : 'accumulator -> datatype_def list -> datatype_def list -> 'accumulator

  (*
   * Method:     datatype_def
   * Summary:    iterates over two alpha-equivalent patterns
   * Parameters: an accumulator
   *             two patterns in internal form
   * Results:    an updated accumulator, obtained by traversing the input patterns
   *)

  method datatype_def : 'accumulator -> datatype_def -> datatype_def -> 'accumulator

  (*
   * Method:     datatype
   * Summary:    iterates under the data constructor "DataType"
   * Parameters: an accumulator
   *             a tuple of the parameters to "DataType"
   * Results:    an updated accumulator, obtained by traversing the parameters
   *)

  method datatype : 'accumulator -> var * ty list -> var * ty list -> 'accumulator

  (*
   * Method:     toplevel_abs
   * Summary:    iterates over two alpha-equivalent patterns
   * Parameters: an accumulator
   *             two patterns in internal form
   * Results:    an updated accumulator, obtained by traversing the input patterns
   *)

  method toplevel_abs : 'accumulator -> toplevel_abs -> toplevel_abs -> 'accumulator

  (*
   * Method:     lambda_abs
   * Summary:    iterates over two alpha-equivalent patterns
   * Parameters: an accumulator
   *             two patterns in internal form
   * Results:    an updated accumulator, obtained by traversing the input patterns
   *)

  method lambda_abs : 'accumulator -> lambda_abs -> lambda_abs -> 'accumulator

  (*
   * Method:     let_abs
   * Summary:    iterates over two alpha-equivalent patterns
   * Parameters: an accumulator
   *             two patterns in internal form
   * Results:    an updated accumulator, obtained by traversing the input patterns
   *)

  method let_abs : 'accumulator -> let_abs -> let_abs -> 'accumulator

  (*
   * Method:     rec_abs
   * Summary:    iterates over two alpha-equivalent patterns
   * Parameters: an accumulator
   *             two patterns in internal form
   * Results:    an updated accumulator, obtained by traversing the input patterns
   *)

  method rec_abs : 'accumulator -> rec_abs -> rec_abs -> 'accumulator

  (*
   * Method:     clause_abs
   * Summary:    iterates over two alpha-equivalent patterns
   * Parameters: an accumulator
   *             two patterns in internal form
   * Results:    an updated accumulator, obtained by traversing the input patterns
   *)

  method clause_abs : 'accumulator -> clause_abs -> clause_abs -> 'accumulator

end

(*
 * Function:   eq_annotation
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_annotation : annotation -> annotation -> bool

(*
 * Function:   eq_program
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_program : program -> program -> bool

(*
 * Function:   eq_toplevel_definition
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_toplevel_definition : toplevel_definition -> toplevel_definition -> bool

(*
 * Function:   eq_param
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_param : param -> param -> bool

(*
 * Function:   eq_term
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_term : term -> term -> bool

(*
 * Function:   eq_binder
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_binder : binder -> binder -> bool

(*
 * Function:   eq_clause
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_clause : clause -> clause -> bool

(*
 * Function:   eq_pattern
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_pattern : pattern -> pattern -> bool

(*
 * Function:   eq_constructor
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_constructor : constructor -> constructor -> bool

(*
 * Function:   eq_primitive
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_primitive : primitive -> primitive -> bool

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
 * Function:   eq_type_definition
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_type_definition : type_definition -> type_definition -> bool

(*
 * Function:   eq_datatype_def
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_datatype_def : datatype_def -> datatype_def -> bool

(*
 * Function:   eq_toplevel_abs
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_toplevel_abs : toplevel_abs -> toplevel_abs -> bool

(*
 * Function:   eq_lambda_abs
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_lambda_abs : lambda_abs -> lambda_abs -> bool

(*
 * Function:   eq_let_abs
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_let_abs : let_abs -> let_abs -> bool

(*
 * Function:   eq_rec_abs
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_rec_abs : rec_abs -> rec_abs -> bool

(*
 * Function:   eq_clause_abs
 * Summary:    tests if two data structures are equal modulo alpha
 * Parameters: two data structures
 * Results:    a boolean
 *)

val eq_clause_abs : clause_abs -> clause_abs -> bool
