(* This file was generated from syntax.mla. Do not edit! *)

module Identifier = AlphaLib.Atom.String

exception IncompatibleStructures
exception UnboundIdentifier of Identifier.t

module Var = AlphaLib.Atom.Make(Identifier)

module Raw = struct

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

 and opaque_toplevel_abs = {
    mutable toplevel_abs_delayed: Var.Subst.t;
    mutable toplevel_abs: toplevel_abs
  }

 and lambda_abs = 
  binder * term

 and opaque_lambda_abs = {
    mutable lambda_abs_delayed: Var.Subst.t;
    mutable lambda_abs: lambda_abs
  }

 and let_abs = 
  var * term * term

 and opaque_let_abs = {
    mutable let_abs_delayed: Var.Subst.t;
    mutable let_abs: let_abs
  }

 and rec_abs = 
  var * term * term

 and opaque_rec_abs = {
    mutable rec_abs_delayed: Var.Subst.t;
    mutable rec_abs: rec_abs
  }

 and clause_abs = 
  annotation * pattern * term

 and opaque_clause_abs = {
    mutable clause_abs_delayed: Var.Subst.t;
    mutable clause_abs: clause_abs
  }

let option_map f = function
  | None ->
      None
  | Some x ->
      Some (f x)

let option_fold f accu = function
  | None ->
      accu
  | Some x ->
      f accu x

let rec id_atom_env_find id env =
  try
    Identifier.Map.find id env
  with Not_found ->
    raise (UnboundIdentifier id)

and idmap_override freshb bv env =
  Identifier.Map.fold (fun id () env ->
    Identifier.Map.add id (freshb id) env
  ) bv env

and import_annotation : unit -> Raw.annotation -> annotation = fun () -> function
  (x0) ->
    (x0)

and subst_annotation : unit -> annotation -> annotation = fun () -> function
  (x0) ->
    (x0)

and export_annotation : unit -> annotation -> Raw.annotation = fun () -> function
  (x0) ->
    (x0)

and free_annotation : annotation -> unit = 
  function annotation -> free_accu_annotation () annotation

and free_accu_annotation = fun () -> function
  (x0) ->
      ()

and import_program : var Identifier.Map.t -> Raw.program -> program = fun (var_env) -> function
  | Raw.EmptyProgram ->
      EmptyProgram
  | Raw.NewDefinition (annotation1, toplevel_abs0) ->
      let (var_bvars) = bvi_toplevel_abs toplevel_abs0 in 
      let var_ienv = idmap_override Var.Atom.freshb var_bvars var_env in
      let toplevel_abs0 = import_toplevel_abs (var_env, var_ienv) toplevel_abs0 in
      NewDefinition ((import_annotation ()) annotation1, create_toplevel_abs toplevel_abs0)

and subst_program : Var.Subst.t -> program -> program = fun (var_env) -> function
  | EmptyProgram ->
      EmptyProgram
  | NewDefinition (annotation1, toplevel_abs0) ->
      NewDefinition ((subst_annotation ()) annotation1, apply_toplevel_abs (var_env) toplevel_abs0)

and export_program : Var.AtomIdMap.t -> program -> Raw.program = fun (var_m) -> function
  | EmptyProgram ->
      Raw.EmptyProgram
  | NewDefinition (annotation1, toplevel_abs0) ->
      let toplevel_abs = open_toplevel_abs toplevel_abs0 in
      let (var_bvars) = bound_toplevel_abs toplevel_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.NewDefinition ((export_annotation ()) annotation1, export_toplevel_abs (var_m, var_im) toplevel_abs)

and free_program : program -> Var.AtomSet.t = 
  function program -> free_accu_program (Var.AtomSet.empty) program

and free_accu_program = fun (var_fvars) -> function
  | EmptyProgram ->
      (var_fvars)
  | NewDefinition (annotation1, toplevel_abs0) ->
      let () = free_accu_annotation () annotation1 in 
      let toplevel_abs = open_toplevel_abs toplevel_abs0 in
      let (var_bvars, var_ifvars, var_ofvars) = bound_free_accu_toplevel_abs (Var.AtomSet.empty, Var.AtomSet.empty, var_fvars) toplevel_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)

and subst_toplevel_definition : Var.Subst.t * Var.Subst.t -> toplevel_definition -> toplevel_definition = fun (var_oenv, var_ienv) -> function
  | TypeDefinition (var2, params1, type_definition0) ->
      TypeDefinition (Var.Subst.lookup var2 var_ienv, List.map (subst_param (var_ienv)) params1, (subst_type_definition (var_ienv)) type_definition0)
  | ValDefinition (var1, term0) ->
      ValDefinition (Var.Subst.lookup var1 var_ienv, (subst_term (var_oenv)) term0)
  | RecDefinitions (var1, term0) ->
      RecDefinitions (Var.Subst.lookup var1 var_ienv, (subst_term (var_ienv)) term0)

and bound_toplevel_definition : toplevel_definition -> Var.AtomSet.t = 
  function toplevel_definition -> bound_accu_toplevel_definition (Var.AtomSet.empty) toplevel_definition

and bound_free_toplevel_definition : toplevel_definition -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t = 
  function toplevel_definition -> bound_free_accu_toplevel_definition (Var.AtomSet.empty, Var.AtomSet.empty, Var.AtomSet.empty) toplevel_definition

and export_toplevel_definition : Var.AtomIdMap.t * Var.AtomIdMap.t -> toplevel_definition -> Raw.toplevel_definition = fun (var_om, var_im) -> function
  | TypeDefinition (var2, params1, type_definition0) ->
      Raw.TypeDefinition (Var.AtomIdMap.lookup var2 var_im, List.map (export_param (var_im)) params1, (export_type_definition (var_im)) type_definition0)
  | ValDefinition (var1, term0) ->
      Raw.ValDefinition (Var.AtomIdMap.lookup var1 var_im, (export_term (var_om)) term0)
  | RecDefinitions (var1, term0) ->
      Raw.RecDefinitions (Var.AtomIdMap.lookup var1 var_im, (export_term (var_im)) term0)

and import_toplevel_definition = fun (var_oenv, var_ienv) -> function
  | Raw.TypeDefinition (var2, params1, type_definition0) ->
      TypeDefinition (id_atom_env_find var2 var_ienv, List.map (import_param (var_ienv)) params1, (import_type_definition (var_ienv)) type_definition0)
  | Raw.ValDefinition (var1, term0) ->
      ValDefinition (id_atom_env_find var1 var_ienv, (import_term (var_oenv)) term0)
  | Raw.RecDefinitions (var1, term0) ->
      RecDefinitions (id_atom_env_find var1 var_ienv, (import_term (var_ienv)) term0)

and bvi_accu_toplevel_definition = fun (var_bvars) -> function
  | Raw.TypeDefinition (var2, params1, type_definition0) ->
      let var_bvars = Identifier.Map.add var2 () var_bvars in
      let (var_bvars) = List.fold_left bvi_accu_param (var_bvars) params1 in
      let (var_bvars) = bvi_accu_type_definition (var_bvars) type_definition0 in
      (var_bvars)
  | Raw.ValDefinition (var1, term0) ->
      let var_bvars = Identifier.Map.add var1 () var_bvars in
      (var_bvars)
  | Raw.RecDefinitions (var1, term0) ->
      let var_bvars = Identifier.Map.add var1 () var_bvars in
      (var_bvars)

and bvi_toplevel_definition = 
  function toplevel_definition -> bvi_accu_toplevel_definition (Identifier.Map.empty) toplevel_definition

and bound_accu_toplevel_definition = fun (var_bvars) -> function
  | TypeDefinition (var2, params1, type_definition0) ->
      let var_bvars = Var.AtomSet.add var2 var_bvars in
      let (var_bvars) = List.fold_left bound_accu_param (var_bvars) params1 in
      let (var_bvars) = bound_accu_type_definition (var_bvars) type_definition0 in
      (var_bvars)
  | ValDefinition (var1, term0) ->
      let var_bvars = Var.AtomSet.add var1 var_bvars in
      (var_bvars)
  | RecDefinitions (var1, term0) ->
      let var_bvars = Var.AtomSet.add var1 var_bvars in
      (var_bvars)

and cmp_toplevel_definition : (Var.Subst.t) * (Var.Subst.t) -> toplevel_definition -> toplevel_definition -> (Var.Subst.t) * (Var.Subst.t) = fun ((var_env1), (var_env2)) -> fun a_cmp_toplevel_definition1 a_cmp_toplevel_definition2 -> match a_cmp_toplevel_definition1, a_cmp_toplevel_definition2 with
  | TypeDefinition (var2, params1, type_definition0), TypeDefinition (var5, params4, type_definition3) ->
let var_env1 =
    let imvar1 = Var.Subst.lookup var2 var_env1 in
      if Var.Atom.equal imvar1 var5 then
        var_env1
      else if Var.Atom.equal imvar1 var2 then
        Var.Subst.add var2 var5 var_env1
      else raise IncompatibleStructures
  and var_env2 =
    let imvar2 = Var.Subst.lookup var5 var_env2 in
      if Var.Atom.equal imvar2 var2 then
        var_env2
      else if Var.Atom.equal imvar2 var5 then
        Var.Subst.add var5 var2 var_env2
      else raise IncompatibleStructures
  in 
  let ((var_env1), (var_env2)) = List.fold_left2  cmp_param ((var_env1), (var_env2)) params1 params4 in
  let ((var_env1), (var_env2)) =  cmp_type_definition ((var_env1), (var_env2)) type_definition0 type_definition3 in
    ((var_env1), (var_env2))
  | ValDefinition (var1, term0), ValDefinition (var3, term2) ->
let var_env1 =
    let imvar1 = Var.Subst.lookup var1 var_env1 in
      if Var.Atom.equal imvar1 var3 then
        var_env1
      else if Var.Atom.equal imvar1 var1 then
        Var.Subst.add var1 var3 var_env1
      else raise IncompatibleStructures
  and var_env2 =
    let imvar2 = Var.Subst.lookup var3 var_env2 in
      if Var.Atom.equal imvar2 var1 then
        var_env2
      else if Var.Atom.equal imvar2 var3 then
        Var.Subst.add var3 var1 var_env2
      else raise IncompatibleStructures
  in 
    ((var_env1), (var_env2))
  | RecDefinitions (var1, term0), RecDefinitions (var3, term2) ->
let var_env1 =
    let imvar1 = Var.Subst.lookup var1 var_env1 in
      if Var.Atom.equal imvar1 var3 then
        var_env1
      else if Var.Atom.equal imvar1 var1 then
        Var.Subst.add var1 var3 var_env1
      else raise IncompatibleStructures
  and var_env2 =
    let imvar2 = Var.Subst.lookup var3 var_env2 in
      if Var.Atom.equal imvar2 var1 then
        var_env2
      else if Var.Atom.equal imvar2 var3 then
        Var.Subst.add var3 var1 var_env2
      else raise IncompatibleStructures
  in 
    ((var_env1), (var_env2))
  | _ -> raise IncompatibleStructures

and bound_free_accu_toplevel_definition = fun (var_bvars, var_ifvars, var_ofvars) -> function
  | TypeDefinition (var2, params1, type_definition0) ->
      let var_bvars = Var.AtomSet.add var2 var_bvars in
      let (var_bvars) = List.fold_left bound_free_accu_param (var_bvars) params1 in
      let (var_bvars, var_ifvars) = bound_free_accu_type_definition (var_bvars, var_ifvars) type_definition0 in
      (var_bvars, var_ifvars, var_ofvars)
  | ValDefinition (var1, term0) ->
      let var_bvars = Var.AtomSet.add var1 var_bvars in
      let (var_ofvars) = free_accu_term (var_ofvars) term0 in
      (var_bvars, var_ifvars, var_ofvars)
  | RecDefinitions (var1, term0) ->
      let var_bvars = Var.AtomSet.add var1 var_bvars in
      let (var_ifvars) = free_accu_term (var_ifvars) term0 in
      (var_bvars, var_ifvars, var_ofvars)

and subst_param : Var.Subst.t -> param -> param = fun (var_ienv) -> function
  (var0) ->
    (Var.Subst.lookup var0 var_ienv)

and bound_param : param -> Var.AtomSet.t = 
  function param -> bound_accu_param (Var.AtomSet.empty) param

and bound_free_param : param -> Var.AtomSet.t = 
  function param -> bound_free_accu_param (Var.AtomSet.empty) param

and export_param : Var.AtomIdMap.t -> param -> Raw.param = fun (var_im) -> function
  (var0) ->
    (Var.AtomIdMap.lookup var0 var_im)

and import_param = fun (var_ienv) -> function
  (var0) ->
    (id_atom_env_find var0 var_ienv)

and bvi_accu_param = fun (var_bvars) -> function
  (var0) ->
      let var_bvars = Identifier.Map.add var0 () var_bvars in
      (var_bvars)

and bvi_param = 
  function param -> bvi_accu_param (Identifier.Map.empty) param

and bound_accu_param = fun (var_bvars) -> function
  (var0) ->
      let var_bvars = Var.AtomSet.add var0 var_bvars in
      (var_bvars)

and cmp_param : (Var.Subst.t) * (Var.Subst.t) -> param -> param -> (Var.Subst.t) * (Var.Subst.t) = fun ((var_env1), (var_env2)) -> fun a_cmp_param1 a_cmp_param2 -> match a_cmp_param1, a_cmp_param2 with
  (var0), (var1) ->
let var_env1 =
    let imvar1 = Var.Subst.lookup var0 var_env1 in
      if Var.Atom.equal imvar1 var1 then
        var_env1
      else if Var.Atom.equal imvar1 var0 then
        Var.Subst.add var0 var1 var_env1
      else raise IncompatibleStructures
  and var_env2 =
    let imvar2 = Var.Subst.lookup var1 var_env2 in
      if Var.Atom.equal imvar2 var0 then
        var_env2
      else if Var.Atom.equal imvar2 var1 then
        Var.Subst.add var1 var0 var_env2
      else raise IncompatibleStructures
  in 
    ((var_env1), (var_env2))

and bound_free_accu_param = fun (var_bvars) -> function
  (var0) ->
      let var_bvars = Var.AtomSet.add var0 var_bvars in
      (var_bvars)

and import_term : var Identifier.Map.t -> Raw.term -> term = fun (var_env) -> function
  | Raw.EVar (annotation1, var0) ->
      EVar ((import_annotation ()) annotation1, id_atom_env_find var0 var_env)
  | Raw.EApp (annotation3, x2, term1, term0) ->
      EApp ((import_annotation ()) annotation3, x2, (import_term (var_env)) term1, (import_term (var_env)) term0)
  | Raw.ELambda (annotation1, lambda_abs0) ->
      let (var_bvars) = bvi_lambda_abs lambda_abs0 in 
      let var_ienv = idmap_override Var.Atom.freshb var_bvars var_env in
      let lambda_abs0 = import_lambda_abs (var_env, var_ienv) lambda_abs0 in
      ELambda ((import_annotation ()) annotation1, create_lambda_abs lambda_abs0)
  | Raw.ELet (annotation1, let_abs0) ->
      let (var_bvars) = bvi_let_abs let_abs0 in 
      let var_ienv = idmap_override Var.Atom.freshb var_bvars var_env in
      let let_abs0 = import_let_abs (var_env, var_ienv) let_abs0 in
      ELet ((import_annotation ()) annotation1, create_let_abs let_abs0)
  | Raw.ELetRec (annotation1, rec_abs0) ->
      let (var_bvars) = bvi_rec_abs rec_abs0 in 
      let var_ienv = idmap_override Var.Atom.freshb var_bvars var_env in
      let rec_abs0 = import_rec_abs (var_ienv) rec_abs0 in
      ELetRec ((import_annotation ()) annotation1, create_rec_abs rec_abs0)
  | Raw.EConstant (annotation1, primitive0) ->
      EConstant ((import_annotation ()) annotation1, (import_primitive ()) primitive0)
  | Raw.EMatch (annotation2, term1, clauses0) ->
      EMatch ((import_annotation ()) annotation2, (import_term (var_env)) term1, List.map (import_clause (var_env)) clauses0)
  | Raw.EAnnot (annotation2, term1, ty0) ->
      EAnnot ((import_annotation ()) annotation2, (import_term (var_env)) term1, (import_ty (var_env)) ty0)

and subst_term : Var.Subst.t -> term -> term = fun (var_env) -> function
  | EVar (annotation1, var0) ->
      EVar ((subst_annotation ()) annotation1, Var.Subst.lookup var0 var_env)
  | EApp (annotation3, x2, term1, term0) ->
      EApp ((subst_annotation ()) annotation3, x2, (subst_term (var_env)) term1, (subst_term (var_env)) term0)
  | ELambda (annotation1, lambda_abs0) ->
      ELambda ((subst_annotation ()) annotation1, apply_lambda_abs (var_env) lambda_abs0)
  | ELet (annotation1, let_abs0) ->
      ELet ((subst_annotation ()) annotation1, apply_let_abs (var_env) let_abs0)
  | ELetRec (annotation1, rec_abs0) ->
      ELetRec ((subst_annotation ()) annotation1, apply_rec_abs (var_env) rec_abs0)
  | EConstant (annotation1, primitive0) ->
      EConstant ((subst_annotation ()) annotation1, (subst_primitive ()) primitive0)
  | EMatch (annotation2, term1, clauses0) ->
      EMatch ((subst_annotation ()) annotation2, (subst_term (var_env)) term1, List.map (subst_clause (var_env)) clauses0)
  | EAnnot (annotation2, term1, ty0) ->
      EAnnot ((subst_annotation ()) annotation2, (subst_term (var_env)) term1, (subst_ty (var_env)) ty0)

and export_term : Var.AtomIdMap.t -> term -> Raw.term = fun (var_m) -> function
  | EVar (annotation1, var0) ->
      Raw.EVar ((export_annotation ()) annotation1, Var.AtomIdMap.lookup var0 var_m)
  | EApp (annotation3, x2, term1, term0) ->
      Raw.EApp ((export_annotation ()) annotation3, x2, (export_term (var_m)) term1, (export_term (var_m)) term0)
  | ELambda (annotation1, lambda_abs0) ->
      let lambda_abs = open_lambda_abs lambda_abs0 in
      let (var_bvars) = bound_lambda_abs lambda_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.ELambda ((export_annotation ()) annotation1, export_lambda_abs (var_m, var_im) lambda_abs)
  | ELet (annotation1, let_abs0) ->
      let let_abs = open_let_abs let_abs0 in
      let (var_bvars) = bound_let_abs let_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.ELet ((export_annotation ()) annotation1, export_let_abs (var_m, var_im) let_abs)
  | ELetRec (annotation1, rec_abs0) ->
      let rec_abs = open_rec_abs rec_abs0 in
      let (var_bvars) = bound_rec_abs rec_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.ELetRec ((export_annotation ()) annotation1, export_rec_abs (var_im) rec_abs)
  | EConstant (annotation1, primitive0) ->
      Raw.EConstant ((export_annotation ()) annotation1, (export_primitive ()) primitive0)
  | EMatch (annotation2, term1, clauses0) ->
      Raw.EMatch ((export_annotation ()) annotation2, (export_term (var_m)) term1, List.map (export_clause (var_m)) clauses0)
  | EAnnot (annotation2, term1, ty0) ->
      Raw.EAnnot ((export_annotation ()) annotation2, (export_term (var_m)) term1, (export_ty (var_m)) ty0)

and free_term : term -> Var.AtomSet.t = 
  function term -> free_accu_term (Var.AtomSet.empty) term

and free_accu_term = fun (var_fvars) -> function
  | EVar (annotation1, var0) ->
      let () = free_accu_annotation () annotation1 in 
      let var_fvars = Var.AtomSet.add var0 var_fvars in
      (var_fvars)
  | EApp (annotation3, x2, term1, term0) ->
      let () = free_accu_annotation () annotation3 in 
      let (var_fvars) = free_accu_term (var_fvars) term1 in 
      let (var_fvars) = free_accu_term (var_fvars) term0 in 
      (var_fvars)
  | ELambda (annotation1, lambda_abs0) ->
      let () = free_accu_annotation () annotation1 in 
      let lambda_abs = open_lambda_abs lambda_abs0 in
      let (var_bvars, var_ifvars, var_ofvars) = bound_free_accu_lambda_abs (Var.AtomSet.empty, Var.AtomSet.empty, var_fvars) lambda_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)
  | ELet (annotation1, let_abs0) ->
      let () = free_accu_annotation () annotation1 in 
      let let_abs = open_let_abs let_abs0 in
      let (var_bvars, var_ifvars, var_ofvars) = bound_free_accu_let_abs (Var.AtomSet.empty, Var.AtomSet.empty, var_fvars) let_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)
  | ELetRec (annotation1, rec_abs0) ->
      let () = free_accu_annotation () annotation1 in 
      let rec_abs = open_rec_abs rec_abs0 in
      let (var_bvars, var_ifvars) = bound_free_accu_rec_abs (Var.AtomSet.empty, Var.AtomSet.empty) rec_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)
  | EConstant (annotation1, primitive0) ->
      let () = free_accu_annotation () annotation1 in 
      let () = free_accu_primitive () primitive0 in 
      (var_fvars)
  | EMatch (annotation2, term1, clauses0) ->
      let () = free_accu_annotation () annotation2 in 
      let (var_fvars) = free_accu_term (var_fvars) term1 in 
      let (var_fvars) = List.fold_left free_accu_clause (var_fvars) clauses0 in 
      (var_fvars)
  | EAnnot (annotation2, term1, ty0) ->
      let () = free_accu_annotation () annotation2 in 
      let (var_fvars) = free_accu_term (var_fvars) term1 in 
      let (var_fvars) = free_accu_ty (var_fvars) ty0 in 
      (var_fvars)

and subst_binder : Var.Subst.t * Var.Subst.t -> binder -> binder = fun (var_oenv, var_ienv) -> function
  | AnnotatedBind (var1, ty0) ->
      AnnotatedBind (Var.Subst.lookup var1 var_ienv, (subst_ty (var_oenv)) ty0)
  | Bind (var0) ->
      Bind (Var.Subst.lookup var0 var_ienv)

and bound_binder : binder -> Var.AtomSet.t = 
  function binder -> bound_accu_binder (Var.AtomSet.empty) binder

and bound_free_binder : binder -> Var.AtomSet.t * Var.AtomSet.t = 
  function binder -> bound_free_accu_binder (Var.AtomSet.empty, Var.AtomSet.empty) binder

and export_binder : Var.AtomIdMap.t * Var.AtomIdMap.t -> binder -> Raw.binder = fun (var_om, var_im) -> function
  | AnnotatedBind (var1, ty0) ->
      Raw.AnnotatedBind (Var.AtomIdMap.lookup var1 var_im, (export_ty (var_om)) ty0)
  | Bind (var0) ->
      Raw.Bind (Var.AtomIdMap.lookup var0 var_im)

and import_binder = fun (var_oenv, var_ienv) -> function
  | Raw.AnnotatedBind (var1, ty0) ->
      AnnotatedBind (id_atom_env_find var1 var_ienv, (import_ty (var_oenv)) ty0)
  | Raw.Bind (var0) ->
      Bind (id_atom_env_find var0 var_ienv)

and bvi_accu_binder = fun (var_bvars) -> function
  | Raw.AnnotatedBind (var1, ty0) ->
      let var_bvars = Identifier.Map.add var1 () var_bvars in
      (var_bvars)
  | Raw.Bind (var0) ->
      let var_bvars = Identifier.Map.add var0 () var_bvars in
      (var_bvars)

and bvi_binder = 
  function binder -> bvi_accu_binder (Identifier.Map.empty) binder

and bound_accu_binder = fun (var_bvars) -> function
  | AnnotatedBind (var1, ty0) ->
      let var_bvars = Var.AtomSet.add var1 var_bvars in
      (var_bvars)
  | Bind (var0) ->
      let var_bvars = Var.AtomSet.add var0 var_bvars in
      (var_bvars)

and cmp_binder : (Var.Subst.t) * (Var.Subst.t) -> binder -> binder -> (Var.Subst.t) * (Var.Subst.t) = fun ((var_env1), (var_env2)) -> fun a_cmp_binder1 a_cmp_binder2 -> match a_cmp_binder1, a_cmp_binder2 with
  | AnnotatedBind (var1, ty0), AnnotatedBind (var3, ty2) ->
let var_env1 =
    let imvar1 = Var.Subst.lookup var1 var_env1 in
      if Var.Atom.equal imvar1 var3 then
        var_env1
      else if Var.Atom.equal imvar1 var1 then
        Var.Subst.add var1 var3 var_env1
      else raise IncompatibleStructures
  and var_env2 =
    let imvar2 = Var.Subst.lookup var3 var_env2 in
      if Var.Atom.equal imvar2 var1 then
        var_env2
      else if Var.Atom.equal imvar2 var3 then
        Var.Subst.add var3 var1 var_env2
      else raise IncompatibleStructures
  in 
    ((var_env1), (var_env2))
  | Bind (var0), Bind (var1) ->
let var_env1 =
    let imvar1 = Var.Subst.lookup var0 var_env1 in
      if Var.Atom.equal imvar1 var1 then
        var_env1
      else if Var.Atom.equal imvar1 var0 then
        Var.Subst.add var0 var1 var_env1
      else raise IncompatibleStructures
  and var_env2 =
    let imvar2 = Var.Subst.lookup var1 var_env2 in
      if Var.Atom.equal imvar2 var0 then
        var_env2
      else if Var.Atom.equal imvar2 var1 then
        Var.Subst.add var1 var0 var_env2
      else raise IncompatibleStructures
  in 
    ((var_env1), (var_env2))
  | _ -> raise IncompatibleStructures

and bound_free_accu_binder = fun (var_bvars, var_ofvars) -> function
  | AnnotatedBind (var1, ty0) ->
      let var_bvars = Var.AtomSet.add var1 var_bvars in
      let (var_ofvars) = free_accu_ty (var_ofvars) ty0 in
      (var_bvars, var_ofvars)
  | Bind (var0) ->
      let var_bvars = Var.AtomSet.add var0 var_bvars in
      (var_bvars, var_ofvars)

and import_clause : var Identifier.Map.t -> Raw.clause -> clause = fun (var_env) -> function
  (clause_abs0) ->
      let (var_bvars) = bvi_clause_abs clause_abs0 in 
      let var_ienv = idmap_override Var.Atom.freshb var_bvars var_env in
      let clause_abs0 = import_clause_abs (var_env, var_ienv) clause_abs0 in
    (create_clause_abs clause_abs0)

and subst_clause : Var.Subst.t -> clause -> clause = fun (var_env) -> function
  (clause_abs0) ->
    (apply_clause_abs (var_env) clause_abs0)

and export_clause : Var.AtomIdMap.t -> clause -> Raw.clause = fun (var_m) -> function
  (clause_abs0) ->
      let clause_abs = open_clause_abs clause_abs0 in
      let (var_bvars) = bound_clause_abs clause_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_clause_abs (var_m, var_im) clause_abs)

and free_clause : clause -> Var.AtomSet.t = 
  function clause -> free_accu_clause (Var.AtomSet.empty) clause

and free_accu_clause = fun (var_fvars) -> function
  (clause_abs0) ->
      let clause_abs = open_clause_abs clause_abs0 in
      let (var_bvars, var_ifvars, var_ofvars) = bound_free_accu_clause_abs (Var.AtomSet.empty, Var.AtomSet.empty, var_fvars) clause_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)

and subst_pattern : Var.Subst.t * Var.Subst.t -> pattern -> pattern = fun (var_oenv, var_ienv) -> function
  | PVar (annotation1, var0) ->
      PVar ((subst_annotation ()) annotation1, Var.Subst.lookup var0 var_ienv)
  | PDataCon (annotation2, constructor1, patterns0) ->
      PDataCon ((subst_annotation ()) annotation2, (subst_constructor (var_oenv)) constructor1, List.map (subst_pattern (var_oenv, var_ienv)) patterns0)

and bound_pattern : pattern -> Var.AtomSet.t = 
  function pattern -> bound_accu_pattern (Var.AtomSet.empty) pattern

and bound_free_pattern : pattern -> Var.AtomSet.t * Var.AtomSet.t = 
  function pattern -> bound_free_accu_pattern (Var.AtomSet.empty, Var.AtomSet.empty) pattern

and export_pattern : Var.AtomIdMap.t * Var.AtomIdMap.t -> pattern -> Raw.pattern = fun (var_om, var_im) -> function
  | PVar (annotation1, var0) ->
      Raw.PVar ((export_annotation ()) annotation1, Var.AtomIdMap.lookup var0 var_im)
  | PDataCon (annotation2, constructor1, patterns0) ->
      Raw.PDataCon ((export_annotation ()) annotation2, (export_constructor (var_om)) constructor1, List.map (export_pattern (var_om, var_im)) patterns0)

and import_pattern = fun (var_oenv, var_ienv) -> function
  | Raw.PVar (annotation1, var0) ->
      PVar ((import_annotation ()) annotation1, id_atom_env_find var0 var_ienv)
  | Raw.PDataCon (annotation2, constructor1, patterns0) ->
      PDataCon ((import_annotation ()) annotation2, (import_constructor (var_oenv)) constructor1, List.map (import_pattern (var_oenv, var_ienv)) patterns0)

and bvi_accu_pattern = fun (var_bvars) -> function
  | Raw.PVar (annotation1, var0) ->
      let var_bvars = Identifier.Map.add var0 () var_bvars in
      (var_bvars)
  | Raw.PDataCon (annotation2, constructor1, patterns0) ->
      let (var_bvars) = List.fold_left bvi_accu_pattern (var_bvars) patterns0 in
      (var_bvars)

and bvi_pattern = 
  function pattern -> bvi_accu_pattern (Identifier.Map.empty) pattern

and bound_accu_pattern = fun (var_bvars) -> function
  | PVar (annotation1, var0) ->
      let var_bvars = Var.AtomSet.add var0 var_bvars in
      (var_bvars)
  | PDataCon (annotation2, constructor1, patterns0) ->
      let (var_bvars) = List.fold_left bound_accu_pattern (var_bvars) patterns0 in
      (var_bvars)

and cmp_pattern : (Var.Subst.t) * (Var.Subst.t) -> pattern -> pattern -> (Var.Subst.t) * (Var.Subst.t) = fun ((var_env1), (var_env2)) -> fun a_cmp_pattern1 a_cmp_pattern2 -> match a_cmp_pattern1, a_cmp_pattern2 with
  | PVar (annotation1, var0), PVar (annotation3, var2) ->
let var_env1 =
    let imvar1 = Var.Subst.lookup var0 var_env1 in
      if Var.Atom.equal imvar1 var2 then
        var_env1
      else if Var.Atom.equal imvar1 var0 then
        Var.Subst.add var0 var2 var_env1
      else raise IncompatibleStructures
  and var_env2 =
    let imvar2 = Var.Subst.lookup var2 var_env2 in
      if Var.Atom.equal imvar2 var0 then
        var_env2
      else if Var.Atom.equal imvar2 var2 then
        Var.Subst.add var2 var0 var_env2
      else raise IncompatibleStructures
  in 
    ((var_env1), (var_env2))
  | PDataCon (annotation2, constructor1, patterns0), PDataCon (annotation5, constructor4, patterns3) ->
  let ((var_env1), (var_env2)) = List.fold_left2  cmp_pattern ((var_env1), (var_env2)) patterns0 patterns3 in
    ((var_env1), (var_env2))
  | _ -> raise IncompatibleStructures

and bound_free_accu_pattern = fun (var_bvars, var_ofvars) -> function
  | PVar (annotation1, var0) ->
      let () = free_accu_annotation () annotation1 in
      let var_bvars = Var.AtomSet.add var0 var_bvars in
      (var_bvars, var_ofvars)
  | PDataCon (annotation2, constructor1, patterns0) ->
      let () = free_accu_annotation () annotation2 in
      let (var_ofvars) = free_accu_constructor (var_ofvars) constructor1 in
      let (var_bvars, var_ofvars) = List.fold_left bound_free_accu_pattern (var_bvars, var_ofvars) patterns0 in
      (var_bvars, var_ofvars)

and import_constructor : var Identifier.Map.t -> Raw.constructor -> constructor = fun (var_env) -> function
  (var0) ->
    (id_atom_env_find var0 var_env)

and subst_constructor : Var.Subst.t -> constructor -> constructor = fun (var_env) -> function
  (var0) ->
    (Var.Subst.lookup var0 var_env)

and export_constructor : Var.AtomIdMap.t -> constructor -> Raw.constructor = fun (var_m) -> function
  (var0) ->
    (Var.AtomIdMap.lookup var0 var_m)

and free_constructor : constructor -> Var.AtomSet.t = 
  function constructor -> free_accu_constructor (Var.AtomSet.empty) constructor

and free_accu_constructor = fun (var_fvars) -> function
  (var0) ->
      let var_fvars = Var.AtomSet.add var0 var_fvars in
      (var_fvars)

and import_primitive : unit -> Raw.primitive -> primitive = fun () -> function
  | Raw.Int (x0) ->
      Int (x0)
  | Raw.String (x0) ->
      String (x0)

and subst_primitive : unit -> primitive -> primitive = fun () -> function
  | Int (x0) ->
      Int (x0)
  | String (x0) ->
      String (x0)

and export_primitive : unit -> primitive -> Raw.primitive = fun () -> function
  | Int (x0) ->
      Raw.Int (x0)
  | String (x0) ->
      Raw.String (x0)

and free_primitive : primitive -> unit = 
  function primitive -> free_accu_primitive () primitive

and free_accu_primitive = fun () -> function
  | Int (x0) ->
      ()
  | String (x0) ->
      ()

and import_ty : var Identifier.Map.t -> Raw.ty -> ty = fun (var_env) -> function
  | Raw.TyArrow (annotation2, ty1, ty0) ->
      TyArrow ((import_annotation ()) annotation2, (import_ty (var_env)) ty1, (import_ty (var_env)) ty0)
  | Raw.TyApp (annotation2, var1, tys0) ->
      TyApp ((import_annotation ()) annotation2, id_atom_env_find var1 var_env, List.map (import_ty (var_env)) tys0)
  | Raw.TyVar (annotation1, var0) ->
      TyVar ((import_annotation ()) annotation1, id_atom_env_find var0 var_env)
  | Raw.TyConstant (annotation1, ty_constant0) ->
      TyConstant ((import_annotation ()) annotation1, (import_ty_constant ()) ty_constant0)

and subst_ty : Var.Subst.t -> ty -> ty = fun (var_env) -> function
  | TyArrow (annotation2, ty1, ty0) ->
      TyArrow ((subst_annotation ()) annotation2, (subst_ty (var_env)) ty1, (subst_ty (var_env)) ty0)
  | TyApp (annotation2, var1, tys0) ->
      TyApp ((subst_annotation ()) annotation2, Var.Subst.lookup var1 var_env, List.map (subst_ty (var_env)) tys0)
  | TyVar (annotation1, var0) ->
      TyVar ((subst_annotation ()) annotation1, Var.Subst.lookup var0 var_env)
  | TyConstant (annotation1, ty_constant0) ->
      TyConstant ((subst_annotation ()) annotation1, (subst_ty_constant ()) ty_constant0)

and export_ty : Var.AtomIdMap.t -> ty -> Raw.ty = fun (var_m) -> function
  | TyArrow (annotation2, ty1, ty0) ->
      Raw.TyArrow ((export_annotation ()) annotation2, (export_ty (var_m)) ty1, (export_ty (var_m)) ty0)
  | TyApp (annotation2, var1, tys0) ->
      Raw.TyApp ((export_annotation ()) annotation2, Var.AtomIdMap.lookup var1 var_m, List.map (export_ty (var_m)) tys0)
  | TyVar (annotation1, var0) ->
      Raw.TyVar ((export_annotation ()) annotation1, Var.AtomIdMap.lookup var0 var_m)
  | TyConstant (annotation1, ty_constant0) ->
      Raw.TyConstant ((export_annotation ()) annotation1, (export_ty_constant ()) ty_constant0)

and free_ty : ty -> Var.AtomSet.t = 
  function ty -> free_accu_ty (Var.AtomSet.empty) ty

and free_accu_ty = fun (var_fvars) -> function
  | TyArrow (annotation2, ty1, ty0) ->
      let () = free_accu_annotation () annotation2 in 
      let (var_fvars) = free_accu_ty (var_fvars) ty1 in 
      let (var_fvars) = free_accu_ty (var_fvars) ty0 in 
      (var_fvars)
  | TyApp (annotation2, var1, tys0) ->
      let () = free_accu_annotation () annotation2 in 
      let var_fvars = Var.AtomSet.add var1 var_fvars in
      let (var_fvars) = List.fold_left free_accu_ty (var_fvars) tys0 in 
      (var_fvars)
  | TyVar (annotation1, var0) ->
      let () = free_accu_annotation () annotation1 in 
      let var_fvars = Var.AtomSet.add var0 var_fvars in
      (var_fvars)
  | TyConstant (annotation1, ty_constant0) ->
      let () = free_accu_annotation () annotation1 in 
      let () = free_accu_ty_constant () ty_constant0 in 
      (var_fvars)

and import_ty_constant : unit -> Raw.ty_constant -> ty_constant = fun () -> function
  | Raw.TyInt ->
      TyInt
  | Raw.TyString ->
      TyString

and subst_ty_constant : unit -> ty_constant -> ty_constant = fun () -> function
  | TyInt ->
      TyInt
  | TyString ->
      TyString

and export_ty_constant : unit -> ty_constant -> Raw.ty_constant = fun () -> function
  | TyInt ->
      Raw.TyInt
  | TyString ->
      Raw.TyString

and free_ty_constant : ty_constant -> unit = 
  function ty_constant -> free_accu_ty_constant () ty_constant

and free_accu_ty_constant = fun () -> function
  | TyInt ->
      ()
  | TyString ->
      ()

and subst_type_definition : Var.Subst.t -> type_definition -> type_definition = fun (var_ienv) -> function
  | AlgebraicDataType (datatype_defs0) ->
      AlgebraicDataType (List.map (subst_datatype_def (var_ienv)) datatype_defs0)

and bound_type_definition : type_definition -> Var.AtomSet.t = 
  function type_definition -> bound_accu_type_definition (Var.AtomSet.empty) type_definition

and bound_free_type_definition : type_definition -> Var.AtomSet.t * Var.AtomSet.t = 
  function type_definition -> bound_free_accu_type_definition (Var.AtomSet.empty, Var.AtomSet.empty) type_definition

and export_type_definition : Var.AtomIdMap.t -> type_definition -> Raw.type_definition = fun (var_im) -> function
  | AlgebraicDataType (datatype_defs0) ->
      Raw.AlgebraicDataType (List.map (export_datatype_def (var_im)) datatype_defs0)

and import_type_definition = fun (var_ienv) -> function
  | Raw.AlgebraicDataType (datatype_defs0) ->
      AlgebraicDataType (List.map (import_datatype_def (var_ienv)) datatype_defs0)

and bvi_accu_type_definition = fun (var_bvars) -> function
  | Raw.AlgebraicDataType (datatype_defs0) ->
      let (var_bvars) = List.fold_left bvi_accu_datatype_def (var_bvars) datatype_defs0 in
      (var_bvars)

and bvi_type_definition = 
  function type_definition -> bvi_accu_type_definition (Identifier.Map.empty) type_definition

and bound_accu_type_definition = fun (var_bvars) -> function
  | AlgebraicDataType (datatype_defs0) ->
      let (var_bvars) = List.fold_left bound_accu_datatype_def (var_bvars) datatype_defs0 in
      (var_bvars)

and cmp_type_definition : (Var.Subst.t) * (Var.Subst.t) -> type_definition -> type_definition -> (Var.Subst.t) * (Var.Subst.t) = fun ((var_env1), (var_env2)) -> fun a_cmp_type_definition1 a_cmp_type_definition2 -> match a_cmp_type_definition1, a_cmp_type_definition2 with
  | AlgebraicDataType (datatype_defs0), AlgebraicDataType (datatype_defs1) ->
  let ((var_env1), (var_env2)) = List.fold_left2  cmp_datatype_def ((var_env1), (var_env2)) datatype_defs0 datatype_defs1 in
    ((var_env1), (var_env2))

and bound_free_accu_type_definition = fun (var_bvars, var_ifvars) -> function
  | AlgebraicDataType (datatype_defs0) ->
      let (var_bvars, var_ifvars) = List.fold_left bound_free_accu_datatype_def (var_bvars, var_ifvars) datatype_defs0 in
      (var_bvars, var_ifvars)

and subst_datatype_def : Var.Subst.t -> datatype_def -> datatype_def = fun (var_ienv) -> function
  | DataType (var1, tys0) ->
      DataType (Var.Subst.lookup var1 var_ienv, List.map (subst_ty (var_ienv)) tys0)

and bound_datatype_def : datatype_def -> Var.AtomSet.t = 
  function datatype_def -> bound_accu_datatype_def (Var.AtomSet.empty) datatype_def

and bound_free_datatype_def : datatype_def -> Var.AtomSet.t * Var.AtomSet.t = 
  function datatype_def -> bound_free_accu_datatype_def (Var.AtomSet.empty, Var.AtomSet.empty) datatype_def

and export_datatype_def : Var.AtomIdMap.t -> datatype_def -> Raw.datatype_def = fun (var_im) -> function
  | DataType (var1, tys0) ->
      Raw.DataType (Var.AtomIdMap.lookup var1 var_im, List.map (export_ty (var_im)) tys0)

and import_datatype_def = fun (var_ienv) -> function
  | Raw.DataType (var1, tys0) ->
      DataType (id_atom_env_find var1 var_ienv, List.map (import_ty (var_ienv)) tys0)

and bvi_accu_datatype_def = fun (var_bvars) -> function
  | Raw.DataType (var1, tys0) ->
      let var_bvars = Identifier.Map.add var1 () var_bvars in
      (var_bvars)

and bvi_datatype_def = 
  function datatype_def -> bvi_accu_datatype_def (Identifier.Map.empty) datatype_def

and bound_accu_datatype_def = fun (var_bvars) -> function
  | DataType (var1, tys0) ->
      let var_bvars = Var.AtomSet.add var1 var_bvars in
      (var_bvars)

and cmp_datatype_def : (Var.Subst.t) * (Var.Subst.t) -> datatype_def -> datatype_def -> (Var.Subst.t) * (Var.Subst.t) = fun ((var_env1), (var_env2)) -> fun a_cmp_datatype_def1 a_cmp_datatype_def2 -> match a_cmp_datatype_def1, a_cmp_datatype_def2 with
  | DataType (var1, tys0), DataType (var3, tys2) ->
let var_env1 =
    let imvar1 = Var.Subst.lookup var1 var_env1 in
      if Var.Atom.equal imvar1 var3 then
        var_env1
      else if Var.Atom.equal imvar1 var1 then
        Var.Subst.add var1 var3 var_env1
      else raise IncompatibleStructures
  and var_env2 =
    let imvar2 = Var.Subst.lookup var3 var_env2 in
      if Var.Atom.equal imvar2 var1 then
        var_env2
      else if Var.Atom.equal imvar2 var3 then
        Var.Subst.add var3 var1 var_env2
      else raise IncompatibleStructures
  in 
    ((var_env1), (var_env2))

and bound_free_accu_datatype_def = fun (var_bvars, var_ifvars) -> function
  | DataType (var1, tys0) ->
      let var_bvars = Var.AtomSet.add var1 var_bvars in
      let (var_ifvars) = List.fold_left free_accu_ty (var_ifvars) tys0 in
      (var_bvars, var_ifvars)

and subst_toplevel_abs : Var.Subst.t * Var.Subst.t -> toplevel_abs -> toplevel_abs = fun (var_oenv, var_ienv) -> function
  (toplevel_definition1, program0) ->
    ((subst_toplevel_definition (var_oenv, var_ienv)) toplevel_definition1, (subst_program (var_ienv)) program0)

and bound_toplevel_abs : toplevel_abs -> Var.AtomSet.t = 
  function toplevel_abs -> bound_accu_toplevel_abs (Var.AtomSet.empty) toplevel_abs

and bound_free_toplevel_abs : toplevel_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t = 
  function toplevel_abs -> bound_free_accu_toplevel_abs (Var.AtomSet.empty, Var.AtomSet.empty, Var.AtomSet.empty) toplevel_abs

and export_toplevel_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> toplevel_abs -> Raw.toplevel_abs = fun (var_om, var_im) -> function
  (toplevel_definition1, program0) ->
    ((export_toplevel_definition (var_om, var_im)) toplevel_definition1, (export_program (var_im)) program0)

and import_toplevel_abs = fun (var_oenv, var_ienv) -> function
  (toplevel_definition1, program0) ->
    ((import_toplevel_definition (var_oenv, var_ienv)) toplevel_definition1, (import_program (var_ienv)) program0)

and bvi_accu_toplevel_abs = fun (var_bvars) -> function
  (toplevel_definition1, program0) ->
      let (var_bvars) = bvi_accu_toplevel_definition (var_bvars) toplevel_definition1 in
      (var_bvars)

and bvi_toplevel_abs = 
  function toplevel_abs -> bvi_accu_toplevel_abs (Identifier.Map.empty) toplevel_abs

and bound_accu_toplevel_abs = fun (var_bvars) -> function
  (toplevel_definition1, program0) ->
      let (var_bvars) = bound_accu_toplevel_definition (var_bvars) toplevel_definition1 in
      (var_bvars)

and cmp_toplevel_abs : (Var.Subst.t) * (Var.Subst.t) -> toplevel_abs -> toplevel_abs -> (Var.Subst.t) * (Var.Subst.t) = fun ((var_env1), (var_env2)) -> fun a_cmp_toplevel_abs1 a_cmp_toplevel_abs2 -> match a_cmp_toplevel_abs1, a_cmp_toplevel_abs2 with
  (toplevel_definition1, program0), (toplevel_definition3, program2) ->
  let ((var_env1), (var_env2)) =  cmp_toplevel_definition ((var_env1), (var_env2)) toplevel_definition1 toplevel_definition3 in
    ((var_env1), (var_env2))

and bound_free_accu_toplevel_abs = fun (var_bvars, var_ifvars, var_ofvars) -> function
  (toplevel_definition1, program0) ->
      let (var_bvars, var_ifvars, var_ofvars) = bound_free_accu_toplevel_definition (var_bvars, var_ifvars, var_ofvars) toplevel_definition1 in
      let (var_ifvars) = free_accu_program (var_ifvars) program0 in
      (var_bvars, var_ifvars, var_ofvars)

and create_toplevel_abs : toplevel_abs -> opaque_toplevel_abs = 
  function body -> {
    toplevel_abs_delayed = (Var.Subst.id);
    toplevel_abs = body
  }

and open_toplevel_abs : opaque_toplevel_abs -> toplevel_abs = function abstraction ->
  let (var_delayed) = abstraction.toplevel_abs_delayed in
  let body = abstraction.toplevel_abs in
  let (var_bvars) = bound_toplevel_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_toplevel_abs (var_delayed, var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.toplevel_abs_delayed <- (Var.Subst.id);
    abstraction.toplevel_abs <- body
  end;
  body

and open2_toplevel_abs : opaque_toplevel_abs -> opaque_toplevel_abs -> toplevel_abs * toplevel_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.toplevel_abs_delayed in
  let body1 = abstraction1.toplevel_abs in
  let (var_delayed2) = abstraction2.toplevel_abs_delayed in
  let body2 = abstraction2.toplevel_abs in
  let (var_rho1), (var_rho2) = cmp_toplevel_abs ((Var.Subst.id), (Var.Subst.id)) body1 body2 in
  let var_subst1 = Var.Subst.map Var.Atom.fresha var_rho1 in
  let var_subst2 = Var.Subst.compose var_subst1 var_rho2 in
  let var_subst1 = Var.Subst.union var_subst1 var_delayed1 in
  let var_subst2 = Var.Subst.union var_subst2 var_delayed2 in

  let body1 = subst_toplevel_abs (var_delayed1, var_subst1) body1 in
  let body2 = subst_toplevel_abs (var_delayed2, var_subst2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.toplevel_abs_delayed <- (Var.Subst.id);
    abstraction1.toplevel_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.toplevel_abs_delayed <- (Var.Subst.id);
    abstraction2.toplevel_abs <- body2
  end;
  (body1, body2)

and apply_toplevel_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.toplevel_abs_delayed in {
      abstraction with toplevel_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_lambda_abs : Var.Subst.t * Var.Subst.t -> lambda_abs -> lambda_abs = fun (var_oenv, var_ienv) -> function
  (binder1, term0) ->
    ((subst_binder (var_oenv, var_ienv)) binder1, (subst_term (var_ienv)) term0)

and bound_lambda_abs : lambda_abs -> Var.AtomSet.t = 
  function lambda_abs -> bound_accu_lambda_abs (Var.AtomSet.empty) lambda_abs

and bound_free_lambda_abs : lambda_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t = 
  function lambda_abs -> bound_free_accu_lambda_abs (Var.AtomSet.empty, Var.AtomSet.empty, Var.AtomSet.empty) lambda_abs

and export_lambda_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> lambda_abs -> Raw.lambda_abs = fun (var_om, var_im) -> function
  (binder1, term0) ->
    ((export_binder (var_om, var_im)) binder1, (export_term (var_im)) term0)

and import_lambda_abs = fun (var_oenv, var_ienv) -> function
  (binder1, term0) ->
    ((import_binder (var_oenv, var_ienv)) binder1, (import_term (var_ienv)) term0)

and bvi_accu_lambda_abs = fun (var_bvars) -> function
  (binder1, term0) ->
      let (var_bvars) = bvi_accu_binder (var_bvars) binder1 in
      (var_bvars)

and bvi_lambda_abs = 
  function lambda_abs -> bvi_accu_lambda_abs (Identifier.Map.empty) lambda_abs

and bound_accu_lambda_abs = fun (var_bvars) -> function
  (binder1, term0) ->
      let (var_bvars) = bound_accu_binder (var_bvars) binder1 in
      (var_bvars)

and cmp_lambda_abs : (Var.Subst.t) * (Var.Subst.t) -> lambda_abs -> lambda_abs -> (Var.Subst.t) * (Var.Subst.t) = fun ((var_env1), (var_env2)) -> fun a_cmp_lambda_abs1 a_cmp_lambda_abs2 -> match a_cmp_lambda_abs1, a_cmp_lambda_abs2 with
  (binder1, term0), (binder3, term2) ->
  let ((var_env1), (var_env2)) =  cmp_binder ((var_env1), (var_env2)) binder1 binder3 in
    ((var_env1), (var_env2))

and bound_free_accu_lambda_abs = fun (var_bvars, var_ifvars, var_ofvars) -> function
  (binder1, term0) ->
      let (var_bvars, var_ofvars) = bound_free_accu_binder (var_bvars, var_ofvars) binder1 in
      let (var_ifvars) = free_accu_term (var_ifvars) term0 in
      (var_bvars, var_ifvars, var_ofvars)

and create_lambda_abs : lambda_abs -> opaque_lambda_abs = 
  function body -> {
    lambda_abs_delayed = (Var.Subst.id);
    lambda_abs = body
  }

and open_lambda_abs : opaque_lambda_abs -> lambda_abs = function abstraction ->
  let (var_delayed) = abstraction.lambda_abs_delayed in
  let body = abstraction.lambda_abs in
  let (var_bvars) = bound_lambda_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_lambda_abs (var_delayed, var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.lambda_abs_delayed <- (Var.Subst.id);
    abstraction.lambda_abs <- body
  end;
  body

and open2_lambda_abs : opaque_lambda_abs -> opaque_lambda_abs -> lambda_abs * lambda_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.lambda_abs_delayed in
  let body1 = abstraction1.lambda_abs in
  let (var_delayed2) = abstraction2.lambda_abs_delayed in
  let body2 = abstraction2.lambda_abs in
  let (var_rho1), (var_rho2) = cmp_lambda_abs ((Var.Subst.id), (Var.Subst.id)) body1 body2 in
  let var_subst1 = Var.Subst.map Var.Atom.fresha var_rho1 in
  let var_subst2 = Var.Subst.compose var_subst1 var_rho2 in
  let var_subst1 = Var.Subst.union var_subst1 var_delayed1 in
  let var_subst2 = Var.Subst.union var_subst2 var_delayed2 in

  let body1 = subst_lambda_abs (var_delayed1, var_subst1) body1 in
  let body2 = subst_lambda_abs (var_delayed2, var_subst2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.lambda_abs_delayed <- (Var.Subst.id);
    abstraction1.lambda_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.lambda_abs_delayed <- (Var.Subst.id);
    abstraction2.lambda_abs <- body2
  end;
  (body1, body2)

and apply_lambda_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.lambda_abs_delayed in {
      abstraction with lambda_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_let_abs : Var.Subst.t * Var.Subst.t -> let_abs -> let_abs = fun (var_oenv, var_ienv) -> function
  (var2, term1, term0) ->
    (Var.Subst.lookup var2 var_ienv, (subst_term (var_oenv)) term1, (subst_term (var_ienv)) term0)

and bound_let_abs : let_abs -> Var.AtomSet.t = 
  function let_abs -> bound_accu_let_abs (Var.AtomSet.empty) let_abs

and bound_free_let_abs : let_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t = 
  function let_abs -> bound_free_accu_let_abs (Var.AtomSet.empty, Var.AtomSet.empty, Var.AtomSet.empty) let_abs

and export_let_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> let_abs -> Raw.let_abs = fun (var_om, var_im) -> function
  (var2, term1, term0) ->
    (Var.AtomIdMap.lookup var2 var_im, (export_term (var_om)) term1, (export_term (var_im)) term0)

and import_let_abs = fun (var_oenv, var_ienv) -> function
  (var2, term1, term0) ->
    (id_atom_env_find var2 var_ienv, (import_term (var_oenv)) term1, (import_term (var_ienv)) term0)

and bvi_accu_let_abs = fun (var_bvars) -> function
  (var2, term1, term0) ->
      let var_bvars = Identifier.Map.add var2 () var_bvars in
      (var_bvars)

and bvi_let_abs = 
  function let_abs -> bvi_accu_let_abs (Identifier.Map.empty) let_abs

and bound_accu_let_abs = fun (var_bvars) -> function
  (var2, term1, term0) ->
      let var_bvars = Var.AtomSet.add var2 var_bvars in
      (var_bvars)

and cmp_let_abs : (Var.Subst.t) * (Var.Subst.t) -> let_abs -> let_abs -> (Var.Subst.t) * (Var.Subst.t) = fun ((var_env1), (var_env2)) -> fun a_cmp_let_abs1 a_cmp_let_abs2 -> match a_cmp_let_abs1, a_cmp_let_abs2 with
  (var2, term1, term0), (var5, term4, term3) ->
let var_env1 =
    let imvar1 = Var.Subst.lookup var2 var_env1 in
      if Var.Atom.equal imvar1 var5 then
        var_env1
      else if Var.Atom.equal imvar1 var2 then
        Var.Subst.add var2 var5 var_env1
      else raise IncompatibleStructures
  and var_env2 =
    let imvar2 = Var.Subst.lookup var5 var_env2 in
      if Var.Atom.equal imvar2 var2 then
        var_env2
      else if Var.Atom.equal imvar2 var5 then
        Var.Subst.add var5 var2 var_env2
      else raise IncompatibleStructures
  in 
    ((var_env1), (var_env2))

and bound_free_accu_let_abs = fun (var_bvars, var_ifvars, var_ofvars) -> function
  (var2, term1, term0) ->
      let var_bvars = Var.AtomSet.add var2 var_bvars in
      let (var_ofvars) = free_accu_term (var_ofvars) term1 in
      let (var_ifvars) = free_accu_term (var_ifvars) term0 in
      (var_bvars, var_ifvars, var_ofvars)

and create_let_abs : let_abs -> opaque_let_abs = 
  function body -> {
    let_abs_delayed = (Var.Subst.id);
    let_abs = body
  }

and open_let_abs : opaque_let_abs -> let_abs = function abstraction ->
  let (var_delayed) = abstraction.let_abs_delayed in
  let body = abstraction.let_abs in
  let (var_bvars) = bound_let_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_let_abs (var_delayed, var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.let_abs_delayed <- (Var.Subst.id);
    abstraction.let_abs <- body
  end;
  body

and open2_let_abs : opaque_let_abs -> opaque_let_abs -> let_abs * let_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.let_abs_delayed in
  let body1 = abstraction1.let_abs in
  let (var_delayed2) = abstraction2.let_abs_delayed in
  let body2 = abstraction2.let_abs in
  let (var_rho1), (var_rho2) = cmp_let_abs ((Var.Subst.id), (Var.Subst.id)) body1 body2 in
  let var_subst1 = Var.Subst.map Var.Atom.fresha var_rho1 in
  let var_subst2 = Var.Subst.compose var_subst1 var_rho2 in
  let var_subst1 = Var.Subst.union var_subst1 var_delayed1 in
  let var_subst2 = Var.Subst.union var_subst2 var_delayed2 in

  let body1 = subst_let_abs (var_delayed1, var_subst1) body1 in
  let body2 = subst_let_abs (var_delayed2, var_subst2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.let_abs_delayed <- (Var.Subst.id);
    abstraction1.let_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.let_abs_delayed <- (Var.Subst.id);
    abstraction2.let_abs <- body2
  end;
  (body1, body2)

and apply_let_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.let_abs_delayed in {
      abstraction with let_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_rec_abs : Var.Subst.t -> rec_abs -> rec_abs = fun (var_ienv) -> function
  (var2, term1, term0) ->
    (Var.Subst.lookup var2 var_ienv, (subst_term (var_ienv)) term1, (subst_term (var_ienv)) term0)

and bound_rec_abs : rec_abs -> Var.AtomSet.t = 
  function rec_abs -> bound_accu_rec_abs (Var.AtomSet.empty) rec_abs

and bound_free_rec_abs : rec_abs -> Var.AtomSet.t * Var.AtomSet.t = 
  function rec_abs -> bound_free_accu_rec_abs (Var.AtomSet.empty, Var.AtomSet.empty) rec_abs

and export_rec_abs : Var.AtomIdMap.t -> rec_abs -> Raw.rec_abs = fun (var_im) -> function
  (var2, term1, term0) ->
    (Var.AtomIdMap.lookup var2 var_im, (export_term (var_im)) term1, (export_term (var_im)) term0)

and import_rec_abs = fun (var_ienv) -> function
  (var2, term1, term0) ->
    (id_atom_env_find var2 var_ienv, (import_term (var_ienv)) term1, (import_term (var_ienv)) term0)

and bvi_accu_rec_abs = fun (var_bvars) -> function
  (var2, term1, term0) ->
      let var_bvars = Identifier.Map.add var2 () var_bvars in
      (var_bvars)

and bvi_rec_abs = 
  function rec_abs -> bvi_accu_rec_abs (Identifier.Map.empty) rec_abs

and bound_accu_rec_abs = fun (var_bvars) -> function
  (var2, term1, term0) ->
      let var_bvars = Var.AtomSet.add var2 var_bvars in
      (var_bvars)

and cmp_rec_abs : (Var.Subst.t) * (Var.Subst.t) -> rec_abs -> rec_abs -> (Var.Subst.t) * (Var.Subst.t) = fun ((var_env1), (var_env2)) -> fun a_cmp_rec_abs1 a_cmp_rec_abs2 -> match a_cmp_rec_abs1, a_cmp_rec_abs2 with
  (var2, term1, term0), (var5, term4, term3) ->
let var_env1 =
    let imvar1 = Var.Subst.lookup var2 var_env1 in
      if Var.Atom.equal imvar1 var5 then
        var_env1
      else if Var.Atom.equal imvar1 var2 then
        Var.Subst.add var2 var5 var_env1
      else raise IncompatibleStructures
  and var_env2 =
    let imvar2 = Var.Subst.lookup var5 var_env2 in
      if Var.Atom.equal imvar2 var2 then
        var_env2
      else if Var.Atom.equal imvar2 var5 then
        Var.Subst.add var5 var2 var_env2
      else raise IncompatibleStructures
  in 
    ((var_env1), (var_env2))

and bound_free_accu_rec_abs = fun (var_bvars, var_ifvars) -> function
  (var2, term1, term0) ->
      let var_bvars = Var.AtomSet.add var2 var_bvars in
      let (var_ifvars) = free_accu_term (var_ifvars) term1 in
      let (var_ifvars) = free_accu_term (var_ifvars) term0 in
      (var_bvars, var_ifvars)

and create_rec_abs : rec_abs -> opaque_rec_abs = 
  function body -> {
    rec_abs_delayed = (Var.Subst.id);
    rec_abs = body
  }

and open_rec_abs : opaque_rec_abs -> rec_abs = function abstraction ->
  let (var_delayed) = abstraction.rec_abs_delayed in
  let body = abstraction.rec_abs in
  let (var_bvars) = bound_rec_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_rec_abs (var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.rec_abs_delayed <- (Var.Subst.id);
    abstraction.rec_abs <- body
  end;
  body

and open2_rec_abs : opaque_rec_abs -> opaque_rec_abs -> rec_abs * rec_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.rec_abs_delayed in
  let body1 = abstraction1.rec_abs in
  let (var_delayed2) = abstraction2.rec_abs_delayed in
  let body2 = abstraction2.rec_abs in
  let (var_rho1), (var_rho2) = cmp_rec_abs ((Var.Subst.id), (Var.Subst.id)) body1 body2 in
  let var_subst1 = Var.Subst.map Var.Atom.fresha var_rho1 in
  let var_subst2 = Var.Subst.compose var_subst1 var_rho2 in
  let var_subst1 = Var.Subst.union var_subst1 var_delayed1 in
  let var_subst2 = Var.Subst.union var_subst2 var_delayed2 in

  let body1 = subst_rec_abs (var_subst1) body1 in
  let body2 = subst_rec_abs (var_subst2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.rec_abs_delayed <- (Var.Subst.id);
    abstraction1.rec_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.rec_abs_delayed <- (Var.Subst.id);
    abstraction2.rec_abs <- body2
  end;
  (body1, body2)

and apply_rec_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.rec_abs_delayed in {
      abstraction with rec_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_clause_abs : Var.Subst.t * Var.Subst.t -> clause_abs -> clause_abs = fun (var_oenv, var_ienv) -> function
  (annotation2, pattern1, term0) ->
    ((subst_annotation ()) annotation2, (subst_pattern (var_oenv, var_ienv)) pattern1, (subst_term (var_ienv)) term0)

and bound_clause_abs : clause_abs -> Var.AtomSet.t = 
  function clause_abs -> bound_accu_clause_abs (Var.AtomSet.empty) clause_abs

and bound_free_clause_abs : clause_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t = 
  function clause_abs -> bound_free_accu_clause_abs (Var.AtomSet.empty, Var.AtomSet.empty, Var.AtomSet.empty) clause_abs

and export_clause_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> clause_abs -> Raw.clause_abs = fun (var_om, var_im) -> function
  (annotation2, pattern1, term0) ->
    ((export_annotation ()) annotation2, (export_pattern (var_om, var_im)) pattern1, (export_term (var_im)) term0)

and import_clause_abs = fun (var_oenv, var_ienv) -> function
  (annotation2, pattern1, term0) ->
    ((import_annotation ()) annotation2, (import_pattern (var_oenv, var_ienv)) pattern1, (import_term (var_ienv)) term0)

and bvi_accu_clause_abs = fun (var_bvars) -> function
  (annotation2, pattern1, term0) ->
      let (var_bvars) = bvi_accu_pattern (var_bvars) pattern1 in
      (var_bvars)

and bvi_clause_abs = 
  function clause_abs -> bvi_accu_clause_abs (Identifier.Map.empty) clause_abs

and bound_accu_clause_abs = fun (var_bvars) -> function
  (annotation2, pattern1, term0) ->
      let (var_bvars) = bound_accu_pattern (var_bvars) pattern1 in
      (var_bvars)

and cmp_clause_abs : (Var.Subst.t) * (Var.Subst.t) -> clause_abs -> clause_abs -> (Var.Subst.t) * (Var.Subst.t) = fun ((var_env1), (var_env2)) -> fun a_cmp_clause_abs1 a_cmp_clause_abs2 -> match a_cmp_clause_abs1, a_cmp_clause_abs2 with
  (annotation2, pattern1, term0), (annotation5, pattern4, term3) ->
  let ((var_env1), (var_env2)) =  cmp_pattern ((var_env1), (var_env2)) pattern1 pattern4 in
    ((var_env1), (var_env2))

and bound_free_accu_clause_abs = fun (var_bvars, var_ifvars, var_ofvars) -> function
  (annotation2, pattern1, term0) ->
      let () = free_accu_annotation () annotation2 in
      let (var_bvars, var_ofvars) = bound_free_accu_pattern (var_bvars, var_ofvars) pattern1 in
      let (var_ifvars) = free_accu_term (var_ifvars) term0 in
      (var_bvars, var_ifvars, var_ofvars)

and create_clause_abs : clause_abs -> opaque_clause_abs = 
  function body -> {
    clause_abs_delayed = (Var.Subst.id);
    clause_abs = body
  }

and open_clause_abs : opaque_clause_abs -> clause_abs = function abstraction ->
  let (var_delayed) = abstraction.clause_abs_delayed in
  let body = abstraction.clause_abs in
  let (var_bvars) = bound_clause_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_clause_abs (var_delayed, var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.clause_abs_delayed <- (Var.Subst.id);
    abstraction.clause_abs <- body
  end;
  body

and open2_clause_abs : opaque_clause_abs -> opaque_clause_abs -> clause_abs * clause_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.clause_abs_delayed in
  let body1 = abstraction1.clause_abs in
  let (var_delayed2) = abstraction2.clause_abs_delayed in
  let body2 = abstraction2.clause_abs in
  let (var_rho1), (var_rho2) = cmp_clause_abs ((Var.Subst.id), (Var.Subst.id)) body1 body2 in
  let var_subst1 = Var.Subst.map Var.Atom.fresha var_rho1 in
  let var_subst2 = Var.Subst.compose var_subst1 var_rho2 in
  let var_subst1 = Var.Subst.union var_subst1 var_delayed1 in
  let var_subst2 = Var.Subst.union var_subst2 var_delayed2 in

  let body1 = subst_clause_abs (var_delayed1, var_subst1) body1 in
  let body2 = subst_clause_abs (var_delayed2, var_subst2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.clause_abs_delayed <- (Var.Subst.id);
    abstraction1.clause_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.clause_abs_delayed <- (Var.Subst.id);
    abstraction2.clause_abs <- body2
  end;
  (body1, body2)

and apply_clause_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.clause_abs_delayed in {
      abstraction with clause_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

class [ 'accumulator ] fold2 = object(self)

  method annotation : 'accumulator -> annotation -> annotation -> 'accumulator = fun accu -> fun a_annotation1 a_annotation2 -> match a_annotation1, a_annotation2 with
  (x0), (x1) ->
    accu

  method program : 'accumulator -> program -> program -> 'accumulator = fun accu -> fun a_program1 a_program2 -> match a_program1, a_program2 with
  | EmptyProgram, EmptyProgram ->
    accu
  | NewDefinition (annotation1, toplevel_abs0), NewDefinition (annotation3, toplevel_abs2) ->
      let accu = (self#annotation) accu annotation1 annotation3 in
      let toplevel_abs0, toplevel_abs2 = open2_toplevel_abs toplevel_abs0 toplevel_abs2 in
      let accu = self#toplevel_abs accu toplevel_abs0 toplevel_abs2 in
    accu
  | _ -> raise IncompatibleStructures

  method emptyprogram : 'accumulator -> 'accumulator = fun accu -> 
 
      accu

  method newdefinition : 'accumulator -> annotation * opaque_toplevel_abs -> annotation * opaque_toplevel_abs -> 'accumulator = fun accu -> 
fun (annotation1, toplevel_abs0)  (annotation3, toplevel_abs2) -> 
      let accu = (self#annotation) accu annotation1 annotation3 in
      let toplevel_abs0, toplevel_abs2 = open2_toplevel_abs toplevel_abs0 toplevel_abs2 in
      let accu = self#toplevel_abs accu toplevel_abs0 toplevel_abs2 in
      accu

  method toplevel_definition : 'accumulator -> toplevel_definition -> toplevel_definition -> 'accumulator = fun accu -> fun a_toplevel_definition1 a_toplevel_definition2 -> match a_toplevel_definition1, a_toplevel_definition2 with
  | TypeDefinition (var2, params1, type_definition0), TypeDefinition (var5, params4, type_definition3) ->
assert (Var.Atom.equal var2 var5);
      let accu = List.fold_left2 (self#param) accu params1 params4 in
      let accu = (self#type_definition) accu type_definition0 type_definition3 in
    accu
  | ValDefinition (var1, term0), ValDefinition (var3, term2) ->
assert (Var.Atom.equal var1 var3);
      let accu = (self#term) accu term0 term2 in
    accu
  | RecDefinitions (var1, term0), RecDefinitions (var3, term2) ->
assert (Var.Atom.equal var1 var3);
      let accu = (self#term) accu term0 term2 in
    accu
  | _ -> raise IncompatibleStructures

  method typedefinition : 'accumulator -> var * param list * type_definition -> var * param list * type_definition -> 'accumulator = fun accu -> 
fun (var2, params1, type_definition0)  (var5, params4, type_definition3) -> 
assert (Var.Atom.equal var2 var5);
      let accu = List.fold_left2 (self#param) accu params1 params4 in
      let accu = (self#type_definition) accu type_definition0 type_definition3 in
      accu

  method valdefinition : 'accumulator -> var * term -> var * term -> 'accumulator = fun accu -> 
fun (var1, term0)  (var3, term2) -> 
assert (Var.Atom.equal var1 var3);
      let accu = (self#term) accu term0 term2 in
      accu

  method recdefinitions : 'accumulator -> var * term -> var * term -> 'accumulator = fun accu -> 
fun (var1, term0)  (var3, term2) -> 
assert (Var.Atom.equal var1 var3);
      let accu = (self#term) accu term0 term2 in
      accu

  method param : 'accumulator -> param -> param -> 'accumulator = fun accu -> fun a_param1 a_param2 -> match a_param1, a_param2 with
  (var0), (var1) ->
assert (Var.Atom.equal var0 var1);
    accu

  method term : 'accumulator -> term -> term -> 'accumulator = fun accu -> fun a_term1 a_term2 -> match a_term1, a_term2 with
  | EVar (annotation1, var0), EVar (annotation3, var2) ->
      let accu = (self#annotation) accu annotation1 annotation3 in
if not (Var.Atom.equal var0 var2) then raise IncompatibleStructures;
    accu
  | EApp (annotation3, x2, term1, term0), EApp (annotation7, x6, term5, term4) ->
      let accu = (self#annotation) accu annotation3 annotation7 in
      let accu = (self#term) accu term1 term5 in
      let accu = (self#term) accu term0 term4 in
    accu
  | ELambda (annotation1, lambda_abs0), ELambda (annotation3, lambda_abs2) ->
      let accu = (self#annotation) accu annotation1 annotation3 in
      let lambda_abs0, lambda_abs2 = open2_lambda_abs lambda_abs0 lambda_abs2 in
      let accu = self#lambda_abs accu lambda_abs0 lambda_abs2 in
    accu
  | ELet (annotation1, let_abs0), ELet (annotation3, let_abs2) ->
      let accu = (self#annotation) accu annotation1 annotation3 in
      let let_abs0, let_abs2 = open2_let_abs let_abs0 let_abs2 in
      let accu = self#let_abs accu let_abs0 let_abs2 in
    accu
  | ELetRec (annotation1, rec_abs0), ELetRec (annotation3, rec_abs2) ->
      let accu = (self#annotation) accu annotation1 annotation3 in
      let rec_abs0, rec_abs2 = open2_rec_abs rec_abs0 rec_abs2 in
      let accu = self#rec_abs accu rec_abs0 rec_abs2 in
    accu
  | EConstant (annotation1, primitive0), EConstant (annotation3, primitive2) ->
      let accu = (self#annotation) accu annotation1 annotation3 in
      let accu = (self#primitive) accu primitive0 primitive2 in
    accu
  | EMatch (annotation2, term1, clauses0), EMatch (annotation5, term4, clauses3) ->
      let accu = (self#annotation) accu annotation2 annotation5 in
      let accu = (self#term) accu term1 term4 in
      let accu = List.fold_left2 (self#clause) accu clauses0 clauses3 in
    accu
  | EAnnot (annotation2, term1, ty0), EAnnot (annotation5, term4, ty3) ->
      let accu = (self#annotation) accu annotation2 annotation5 in
      let accu = (self#term) accu term1 term4 in
      let accu = (self#ty) accu ty0 ty3 in
    accu
  | _ -> raise IncompatibleStructures

  method evar : 'accumulator -> annotation * var -> annotation * var -> 'accumulator = fun accu -> 
fun (annotation1, var0)  (annotation3, var2) -> 
      let accu = (self#annotation) accu annotation1 annotation3 in
if not (Var.Atom.equal var0 var2) then raise IncompatibleStructures;
      accu

  method eapp : 'accumulator -> annotation * ( Level.t ) * term * term -> annotation * ( Level.t ) * term * term -> 'accumulator = fun accu -> 
fun (annotation3, x2, term1, term0)  (annotation7, x6, term5, term4) -> 
      let accu = (self#annotation) accu annotation3 annotation7 in
      let accu = (self#term) accu term1 term5 in
      let accu = (self#term) accu term0 term4 in
      accu

  method elambda : 'accumulator -> annotation * opaque_lambda_abs -> annotation * opaque_lambda_abs -> 'accumulator = fun accu -> 
fun (annotation1, lambda_abs0)  (annotation3, lambda_abs2) -> 
      let accu = (self#annotation) accu annotation1 annotation3 in
      let lambda_abs0, lambda_abs2 = open2_lambda_abs lambda_abs0 lambda_abs2 in
      let accu = self#lambda_abs accu lambda_abs0 lambda_abs2 in
      accu

  method elet : 'accumulator -> annotation * opaque_let_abs -> annotation * opaque_let_abs -> 'accumulator = fun accu -> 
fun (annotation1, let_abs0)  (annotation3, let_abs2) -> 
      let accu = (self#annotation) accu annotation1 annotation3 in
      let let_abs0, let_abs2 = open2_let_abs let_abs0 let_abs2 in
      let accu = self#let_abs accu let_abs0 let_abs2 in
      accu

  method eletrec : 'accumulator -> annotation * opaque_rec_abs -> annotation * opaque_rec_abs -> 'accumulator = fun accu -> 
fun (annotation1, rec_abs0)  (annotation3, rec_abs2) -> 
      let accu = (self#annotation) accu annotation1 annotation3 in
      let rec_abs0, rec_abs2 = open2_rec_abs rec_abs0 rec_abs2 in
      let accu = self#rec_abs accu rec_abs0 rec_abs2 in
      accu

  method econstant : 'accumulator -> annotation * primitive -> annotation * primitive -> 'accumulator = fun accu -> 
fun (annotation1, primitive0)  (annotation3, primitive2) -> 
      let accu = (self#annotation) accu annotation1 annotation3 in
      let accu = (self#primitive) accu primitive0 primitive2 in
      accu

  method ematch : 'accumulator -> annotation * term * clause list -> annotation * term * clause list -> 'accumulator = fun accu -> 
fun (annotation2, term1, clauses0)  (annotation5, term4, clauses3) -> 
      let accu = (self#annotation) accu annotation2 annotation5 in
      let accu = (self#term) accu term1 term4 in
      let accu = List.fold_left2 (self#clause) accu clauses0 clauses3 in
      accu

  method eannot : 'accumulator -> annotation * term * ty -> annotation * term * ty -> 'accumulator = fun accu -> 
fun (annotation2, term1, ty0)  (annotation5, term4, ty3) -> 
      let accu = (self#annotation) accu annotation2 annotation5 in
      let accu = (self#term) accu term1 term4 in
      let accu = (self#ty) accu ty0 ty3 in
      accu

  method binder : 'accumulator -> binder -> binder -> 'accumulator = fun accu -> fun a_binder1 a_binder2 -> match a_binder1, a_binder2 with
  | AnnotatedBind (var1, ty0), AnnotatedBind (var3, ty2) ->
assert (Var.Atom.equal var1 var3);
      let accu = (self#ty) accu ty0 ty2 in
    accu
  | Bind (var0), Bind (var1) ->
assert (Var.Atom.equal var0 var1);
    accu
  | _ -> raise IncompatibleStructures

  method annotatedbind : 'accumulator -> var * ty -> var * ty -> 'accumulator = fun accu -> 
fun (var1, ty0)  (var3, ty2) -> 
assert (Var.Atom.equal var1 var3);
      let accu = (self#ty) accu ty0 ty2 in
      accu

  method bind : 'accumulator -> var -> var -> 'accumulator = fun accu -> 
fun (var0)  (var1) -> 
assert (Var.Atom.equal var0 var1);
      accu

  method clause : 'accumulator -> clause -> clause -> 'accumulator = fun accu -> fun a_clause1 a_clause2 -> match a_clause1, a_clause2 with
  (clause_abs0), (clause_abs1) ->
      let clause_abs0, clause_abs1 = open2_clause_abs clause_abs0 clause_abs1 in
      let accu = self#clause_abs accu clause_abs0 clause_abs1 in
    accu

  method pattern : 'accumulator -> pattern -> pattern -> 'accumulator = fun accu -> fun a_pattern1 a_pattern2 -> match a_pattern1, a_pattern2 with
  | PVar (annotation1, var0), PVar (annotation3, var2) ->
      let accu = (self#annotation) accu annotation1 annotation3 in
assert (Var.Atom.equal var0 var2);
    accu
  | PDataCon (annotation2, constructor1, patterns0), PDataCon (annotation5, constructor4, patterns3) ->
      let accu = (self#annotation) accu annotation2 annotation5 in
      let accu = (self#constructor) accu constructor1 constructor4 in
      let accu = List.fold_left2 (self#pattern) accu patterns0 patterns3 in
    accu
  | _ -> raise IncompatibleStructures

  method pvar : 'accumulator -> annotation * var -> annotation * var -> 'accumulator = fun accu -> 
fun (annotation1, var0)  (annotation3, var2) -> 
      let accu = (self#annotation) accu annotation1 annotation3 in
assert (Var.Atom.equal var0 var2);
      accu

  method pdatacon : 'accumulator -> annotation * constructor * pattern list -> annotation * constructor * pattern list -> 'accumulator = fun accu -> 
fun (annotation2, constructor1, patterns0)  (annotation5, constructor4, patterns3) -> 
      let accu = (self#annotation) accu annotation2 annotation5 in
      let accu = (self#constructor) accu constructor1 constructor4 in
      let accu = List.fold_left2 (self#pattern) accu patterns0 patterns3 in
      accu

  method constructor : 'accumulator -> constructor -> constructor -> 'accumulator = fun accu -> fun a_constructor1 a_constructor2 -> match a_constructor1, a_constructor2 with
  (var0), (var1) ->
if not (Var.Atom.equal var0 var1) then raise IncompatibleStructures;
    accu

  method primitive : 'accumulator -> primitive -> primitive -> 'accumulator = fun accu -> fun a_primitive1 a_primitive2 -> match a_primitive1, a_primitive2 with
  | Int (x0), Int (x1) ->
    accu
  | String (x0), String (x1) ->
    accu
  | _ -> raise IncompatibleStructures

  method int : 'accumulator -> ( int ) -> ( int ) -> 'accumulator = fun accu -> 
fun (x0)  (x1) -> 
      accu

  method string : 'accumulator -> ( string ) -> ( string ) -> 'accumulator = fun accu -> 
fun (x0)  (x1) -> 
      accu

  method ty : 'accumulator -> ty -> ty -> 'accumulator = fun accu -> fun a_ty1 a_ty2 -> match a_ty1, a_ty2 with
  | TyArrow (annotation2, ty1, ty0), TyArrow (annotation5, ty4, ty3) ->
      let accu = (self#annotation) accu annotation2 annotation5 in
      let accu = (self#ty) accu ty1 ty4 in
      let accu = (self#ty) accu ty0 ty3 in
    accu
  | TyApp (annotation2, var1, tys0), TyApp (annotation5, var4, tys3) ->
      let accu = (self#annotation) accu annotation2 annotation5 in
if not (Var.Atom.equal var1 var4) then raise IncompatibleStructures;
      let accu = List.fold_left2 (self#ty) accu tys0 tys3 in
    accu
  | TyVar (annotation1, var0), TyVar (annotation3, var2) ->
      let accu = (self#annotation) accu annotation1 annotation3 in
if not (Var.Atom.equal var0 var2) then raise IncompatibleStructures;
    accu
  | TyConstant (annotation1, ty_constant0), TyConstant (annotation3, ty_constant2) ->
      let accu = (self#annotation) accu annotation1 annotation3 in
      let accu = (self#ty_constant) accu ty_constant0 ty_constant2 in
    accu
  | _ -> raise IncompatibleStructures

  method tyarrow : 'accumulator -> annotation * ty * ty -> annotation * ty * ty -> 'accumulator = fun accu -> 
fun (annotation2, ty1, ty0)  (annotation5, ty4, ty3) -> 
      let accu = (self#annotation) accu annotation2 annotation5 in
      let accu = (self#ty) accu ty1 ty4 in
      let accu = (self#ty) accu ty0 ty3 in
      accu

  method tyapp : 'accumulator -> annotation * var * ty list -> annotation * var * ty list -> 'accumulator = fun accu -> 
fun (annotation2, var1, tys0)  (annotation5, var4, tys3) -> 
      let accu = (self#annotation) accu annotation2 annotation5 in
if not (Var.Atom.equal var1 var4) then raise IncompatibleStructures;
      let accu = List.fold_left2 (self#ty) accu tys0 tys3 in
      accu

  method tyvar : 'accumulator -> annotation * var -> annotation * var -> 'accumulator = fun accu -> 
fun (annotation1, var0)  (annotation3, var2) -> 
      let accu = (self#annotation) accu annotation1 annotation3 in
if not (Var.Atom.equal var0 var2) then raise IncompatibleStructures;
      accu

  method tyconstant : 'accumulator -> annotation * ty_constant -> annotation * ty_constant -> 'accumulator = fun accu -> 
fun (annotation1, ty_constant0)  (annotation3, ty_constant2) -> 
      let accu = (self#annotation) accu annotation1 annotation3 in
      let accu = (self#ty_constant) accu ty_constant0 ty_constant2 in
      accu

  method ty_constant : 'accumulator -> ty_constant -> ty_constant -> 'accumulator = fun accu -> fun a_ty_constant1 a_ty_constant2 -> match a_ty_constant1, a_ty_constant2 with
  | TyInt, TyInt ->
    accu
  | TyString, TyString ->
    accu
  | _ -> raise IncompatibleStructures

  method tyint : 'accumulator -> 'accumulator = fun accu -> 
 
      accu

  method tystring : 'accumulator -> 'accumulator = fun accu -> 
 
      accu

  method type_definition : 'accumulator -> type_definition -> type_definition -> 'accumulator = fun accu -> fun a_type_definition1 a_type_definition2 -> match a_type_definition1, a_type_definition2 with
  | AlgebraicDataType (datatype_defs0), AlgebraicDataType (datatype_defs1) ->
      let accu = List.fold_left2 (self#datatype_def) accu datatype_defs0 datatype_defs1 in
    accu

  method algebraicdatatype : 'accumulator -> datatype_def list -> datatype_def list -> 'accumulator = fun accu -> 
fun (datatype_defs0)  (datatype_defs1) -> 
      let accu = List.fold_left2 (self#datatype_def) accu datatype_defs0 datatype_defs1 in
      accu

  method datatype_def : 'accumulator -> datatype_def -> datatype_def -> 'accumulator = fun accu -> fun a_datatype_def1 a_datatype_def2 -> match a_datatype_def1, a_datatype_def2 with
  | DataType (var1, tys0), DataType (var3, tys2) ->
assert (Var.Atom.equal var1 var3);
      let accu = List.fold_left2 (self#ty) accu tys0 tys2 in
    accu

  method datatype : 'accumulator -> var * ty list -> var * ty list -> 'accumulator = fun accu -> 
fun (var1, tys0)  (var3, tys2) -> 
assert (Var.Atom.equal var1 var3);
      let accu = List.fold_left2 (self#ty) accu tys0 tys2 in
      accu

  method toplevel_abs : 'accumulator -> toplevel_abs -> toplevel_abs -> 'accumulator = fun accu -> fun a_toplevel_abs1 a_toplevel_abs2 -> match a_toplevel_abs1, a_toplevel_abs2 with
  (toplevel_definition1, program0), (toplevel_definition3, program2) ->
      let accu = (self#toplevel_definition) accu toplevel_definition1 toplevel_definition3 in
      let accu = (self#program) accu program0 program2 in
    accu

  method lambda_abs : 'accumulator -> lambda_abs -> lambda_abs -> 'accumulator = fun accu -> fun a_lambda_abs1 a_lambda_abs2 -> match a_lambda_abs1, a_lambda_abs2 with
  (binder1, term0), (binder3, term2) ->
      let accu = (self#binder) accu binder1 binder3 in
      let accu = (self#term) accu term0 term2 in
    accu

  method let_abs : 'accumulator -> let_abs -> let_abs -> 'accumulator = fun accu -> fun a_let_abs1 a_let_abs2 -> match a_let_abs1, a_let_abs2 with
  (var2, term1, term0), (var5, term4, term3) ->
assert (Var.Atom.equal var2 var5);
      let accu = (self#term) accu term1 term4 in
      let accu = (self#term) accu term0 term3 in
    accu

  method rec_abs : 'accumulator -> rec_abs -> rec_abs -> 'accumulator = fun accu -> fun a_rec_abs1 a_rec_abs2 -> match a_rec_abs1, a_rec_abs2 with
  (var2, term1, term0), (var5, term4, term3) ->
assert (Var.Atom.equal var2 var5);
      let accu = (self#term) accu term1 term4 in
      let accu = (self#term) accu term0 term3 in
    accu

  method clause_abs : 'accumulator -> clause_abs -> clause_abs -> 'accumulator = fun accu -> fun a_clause_abs1 a_clause_abs2 -> match a_clause_abs1, a_clause_abs2 with
  (annotation2, pattern1, term0), (annotation5, pattern4, term3) ->
      let accu = (self#annotation) accu annotation2 annotation5 in
      let accu = (self#pattern) accu pattern1 pattern4 in
      let accu = (self#term) accu term0 term3 in
    accu

end
let rec f x = x

and eq_annotation : annotation -> annotation -> bool = fun p1 p2 ->
   try ((new fold2)#annotation true p1 p2 = true) with IncompatibleStructures -> false

and eq_program : program -> program -> bool = fun p1 p2 ->
   try ((new fold2)#program true p1 p2 = true) with IncompatibleStructures -> false

and eq_toplevel_definition : toplevel_definition -> toplevel_definition -> bool = fun p1 p2 ->
   try ((new fold2)#toplevel_definition true p1 p2 = true) with IncompatibleStructures -> false

and eq_param : param -> param -> bool = fun p1 p2 ->
   try ((new fold2)#param true p1 p2 = true) with IncompatibleStructures -> false

and eq_term : term -> term -> bool = fun p1 p2 ->
   try ((new fold2)#term true p1 p2 = true) with IncompatibleStructures -> false

and eq_binder : binder -> binder -> bool = fun p1 p2 ->
   try ((new fold2)#binder true p1 p2 = true) with IncompatibleStructures -> false

and eq_clause : clause -> clause -> bool = fun p1 p2 ->
   try ((new fold2)#clause true p1 p2 = true) with IncompatibleStructures -> false

and eq_pattern : pattern -> pattern -> bool = fun p1 p2 ->
   try ((new fold2)#pattern true p1 p2 = true) with IncompatibleStructures -> false

and eq_constructor : constructor -> constructor -> bool = fun p1 p2 ->
   try ((new fold2)#constructor true p1 p2 = true) with IncompatibleStructures -> false

and eq_primitive : primitive -> primitive -> bool = fun p1 p2 ->
   try ((new fold2)#primitive true p1 p2 = true) with IncompatibleStructures -> false

and eq_ty : ty -> ty -> bool = fun p1 p2 ->
   try ((new fold2)#ty true p1 p2 = true) with IncompatibleStructures -> false

and eq_ty_constant : ty_constant -> ty_constant -> bool = fun p1 p2 ->
   try ((new fold2)#ty_constant true p1 p2 = true) with IncompatibleStructures -> false

and eq_type_definition : type_definition -> type_definition -> bool = fun p1 p2 ->
   try ((new fold2)#type_definition true p1 p2 = true) with IncompatibleStructures -> false

and eq_datatype_def : datatype_def -> datatype_def -> bool = fun p1 p2 ->
   try ((new fold2)#datatype_def true p1 p2 = true) with IncompatibleStructures -> false

and eq_toplevel_abs : toplevel_abs -> toplevel_abs -> bool = fun p1 p2 ->
   try ((new fold2)#toplevel_abs true p1 p2 = true) with IncompatibleStructures -> false

and eq_lambda_abs : lambda_abs -> lambda_abs -> bool = fun p1 p2 ->
   try ((new fold2)#lambda_abs true p1 p2 = true) with IncompatibleStructures -> false

and eq_let_abs : let_abs -> let_abs -> bool = fun p1 p2 ->
   try ((new fold2)#let_abs true p1 p2 = true) with IncompatibleStructures -> false

and eq_rec_abs : rec_abs -> rec_abs -> bool = fun p1 p2 ->
   try ((new fold2)#rec_abs true p1 p2 = true) with IncompatibleStructures -> false

and eq_clause_abs : clause_abs -> clause_abs -> bool = fun p1 p2 ->
   try ((new fold2)#clause_abs true p1 p2 = true) with IncompatibleStructures -> false

class map = object(self)

  method annotation : annotation -> annotation = function
  (x0) ->
    (x0)

  method program : program -> program = function
  | EmptyProgram ->
      self#emptyprogram
  | NewDefinition (annotation1, toplevel_abs0) ->
      self#newdefinition (annotation1, toplevel_abs0)

  method emptyprogram : program = EmptyProgram

  method newdefinition : annotation * opaque_toplevel_abs -> program = 
  function (annotation1, toplevel_abs0) -> 
      NewDefinition ((self#annotation) annotation1, create_toplevel_abs (self#toplevel_abs (open_toplevel_abs toplevel_abs0)))

  method toplevel_definition : toplevel_definition -> toplevel_definition = function
  | TypeDefinition (var2, params1, type_definition0) ->
      self#typedefinition (var2, params1, type_definition0)
  | ValDefinition (var1, term0) ->
      self#valdefinition (var1, term0)
  | RecDefinitions (var1, term0) ->
      self#recdefinitions (var1, term0)

  method typedefinition : var * param list * type_definition -> toplevel_definition = 
  function (var2, params1, type_definition0) -> 
      TypeDefinition (var2, List.map (self#param) params1, (self#type_definition) type_definition0)

  method valdefinition : var * term -> toplevel_definition = 
  function (var1, term0) -> 
      ValDefinition (var1, (self#term) term0)

  method recdefinitions : var * term -> toplevel_definition = 
  function (var1, term0) -> 
      RecDefinitions (var1, (self#term) term0)

  method param : param -> param = function
  (var0) ->
    (var0)

  method term : term -> term = function
  | EVar (annotation1, var0) ->
      self#evar (annotation1, var0)
  | EApp (annotation3, x2, term1, term0) ->
      self#eapp (annotation3, x2, term1, term0)
  | ELambda (annotation1, lambda_abs0) ->
      self#elambda (annotation1, lambda_abs0)
  | ELet (annotation1, let_abs0) ->
      self#elet (annotation1, let_abs0)
  | ELetRec (annotation1, rec_abs0) ->
      self#eletrec (annotation1, rec_abs0)
  | EConstant (annotation1, primitive0) ->
      self#econstant (annotation1, primitive0)
  | EMatch (annotation2, term1, clauses0) ->
      self#ematch (annotation2, term1, clauses0)
  | EAnnot (annotation2, term1, ty0) ->
      self#eannot (annotation2, term1, ty0)

  method evar : annotation * var -> term = 
  function (annotation1, var0) -> 
      EVar ((self#annotation) annotation1, var0)

  method eapp : annotation * ( Level.t ) * term * term -> term = 
  function (annotation3, x2, term1, term0) -> 
      EApp ((self#annotation) annotation3, x2, (self#term) term1, (self#term) term0)

  method elambda : annotation * opaque_lambda_abs -> term = 
  function (annotation1, lambda_abs0) -> 
      ELambda ((self#annotation) annotation1, create_lambda_abs (self#lambda_abs (open_lambda_abs lambda_abs0)))

  method elet : annotation * opaque_let_abs -> term = 
  function (annotation1, let_abs0) -> 
      ELet ((self#annotation) annotation1, create_let_abs (self#let_abs (open_let_abs let_abs0)))

  method eletrec : annotation * opaque_rec_abs -> term = 
  function (annotation1, rec_abs0) -> 
      ELetRec ((self#annotation) annotation1, create_rec_abs (self#rec_abs (open_rec_abs rec_abs0)))

  method econstant : annotation * primitive -> term = 
  function (annotation1, primitive0) -> 
      EConstant ((self#annotation) annotation1, (self#primitive) primitive0)

  method ematch : annotation * term * clause list -> term = 
  function (annotation2, term1, clauses0) -> 
      EMatch ((self#annotation) annotation2, (self#term) term1, List.map (self#clause) clauses0)

  method eannot : annotation * term * ty -> term = 
  function (annotation2, term1, ty0) -> 
      EAnnot ((self#annotation) annotation2, (self#term) term1, (self#ty) ty0)

  method binder : binder -> binder = function
  | AnnotatedBind (var1, ty0) ->
      self#annotatedbind (var1, ty0)
  | Bind (var0) ->
      self#bind (var0)

  method annotatedbind : var * ty -> binder = 
  function (var1, ty0) -> 
      AnnotatedBind (var1, (self#ty) ty0)

  method bind : var -> binder = 
  function (var0) -> 
      Bind (var0)

  method clause : clause -> clause = function
  (clause_abs0) ->
    (create_clause_abs (self#clause_abs (open_clause_abs clause_abs0)))

  method pattern : pattern -> pattern = function
  | PVar (annotation1, var0) ->
      self#pvar (annotation1, var0)
  | PDataCon (annotation2, constructor1, patterns0) ->
      self#pdatacon (annotation2, constructor1, patterns0)

  method pvar : annotation * var -> pattern = 
  function (annotation1, var0) -> 
      PVar ((self#annotation) annotation1, var0)

  method pdatacon : annotation * constructor * pattern list -> pattern = 
  function (annotation2, constructor1, patterns0) -> 
      PDataCon ((self#annotation) annotation2, (self#constructor) constructor1, List.map (self#pattern) patterns0)

  method constructor : constructor -> constructor = function
  (var0) ->
    (var0)

  method primitive : primitive -> primitive = function
  | Int (x0) ->
      self#int (x0)
  | String (x0) ->
      self#string (x0)

  method int : ( int ) -> primitive = 
  function (x0) -> 
      Int (x0)

  method string : ( string ) -> primitive = 
  function (x0) -> 
      String (x0)

  method ty : ty -> ty = function
  | TyArrow (annotation2, ty1, ty0) ->
      self#tyarrow (annotation2, ty1, ty0)
  | TyApp (annotation2, var1, tys0) ->
      self#tyapp (annotation2, var1, tys0)
  | TyVar (annotation1, var0) ->
      self#tyvar (annotation1, var0)
  | TyConstant (annotation1, ty_constant0) ->
      self#tyconstant (annotation1, ty_constant0)

  method tyarrow : annotation * ty * ty -> ty = 
  function (annotation2, ty1, ty0) -> 
      TyArrow ((self#annotation) annotation2, (self#ty) ty1, (self#ty) ty0)

  method tyapp : annotation * var * ty list -> ty = 
  function (annotation2, var1, tys0) -> 
      TyApp ((self#annotation) annotation2, var1, List.map (self#ty) tys0)

  method tyvar : annotation * var -> ty = 
  function (annotation1, var0) -> 
      TyVar ((self#annotation) annotation1, var0)

  method tyconstant : annotation * ty_constant -> ty = 
  function (annotation1, ty_constant0) -> 
      TyConstant ((self#annotation) annotation1, (self#ty_constant) ty_constant0)

  method ty_constant : ty_constant -> ty_constant = function
  | TyInt ->
      self#tyint
  | TyString ->
      self#tystring

  method tyint : ty_constant = TyInt

  method tystring : ty_constant = TyString

  method type_definition : type_definition -> type_definition = function
  | AlgebraicDataType (datatype_defs0) ->
      self#algebraicdatatype (datatype_defs0)

  method algebraicdatatype : datatype_def list -> type_definition = 
  function (datatype_defs0) -> 
      AlgebraicDataType (List.map (self#datatype_def) datatype_defs0)

  method datatype_def : datatype_def -> datatype_def = function
  | DataType (var1, tys0) ->
      self#datatype (var1, tys0)

  method datatype : var * ty list -> datatype_def = 
  function (var1, tys0) -> 
      DataType (var1, List.map (self#ty) tys0)

  method toplevel_abs : toplevel_abs -> toplevel_abs = function
  (toplevel_definition1, program0) ->
    ((self#toplevel_definition) toplevel_definition1, (self#program) program0)

  method lambda_abs : lambda_abs -> lambda_abs = function
  (binder1, term0) ->
    ((self#binder) binder1, (self#term) term0)

  method let_abs : let_abs -> let_abs = function
  (var2, term1, term0) ->
    (var2, (self#term) term1, (self#term) term0)

  method rec_abs : rec_abs -> rec_abs = function
  (var2, term1, term0) ->
    (var2, (self#term) term1, (self#term) term0)

  method clause_abs : clause_abs -> clause_abs = function
  (annotation2, pattern1, term0) ->
    ((self#annotation) annotation2, (self#pattern) pattern1, (self#term) term0)

end

class [ 'accumulator ] fold = object(self)

  method annotation : 'accumulator -> annotation -> 'accumulator = fun accu -> function
  (x0) ->
      accu

  method program : 'accumulator -> program -> 'accumulator = fun accu -> function
  | EmptyProgram ->
      self#emptyprogram accu
  | NewDefinition (annotation1, toplevel_abs0) ->
      self#newdefinition accu (annotation1, toplevel_abs0)

  method emptyprogram : 'accumulator -> 'accumulator = fun accu ->      accu

  method newdefinition : 'accumulator -> annotation * opaque_toplevel_abs -> 'accumulator = fun accu -> 
  function (annotation1, toplevel_abs0) -> 
      let accu = (self#annotation) accu annotation1 in
      let accu = self#toplevel_abs accu (open_toplevel_abs toplevel_abs0) in
     accu

  method toplevel_definition : 'accumulator -> toplevel_definition -> 'accumulator = fun accu -> function
  | TypeDefinition (var2, params1, type_definition0) ->
      self#typedefinition accu (var2, params1, type_definition0)
  | ValDefinition (var1, term0) ->
      self#valdefinition accu (var1, term0)
  | RecDefinitions (var1, term0) ->
      self#recdefinitions accu (var1, term0)

  method typedefinition : 'accumulator -> var * param list * type_definition -> 'accumulator = fun accu -> 
  function (var2, params1, type_definition0) -> 
      let accu = List.fold_left (self#param) accu params1 in
      let accu = (self#type_definition) accu type_definition0 in
     accu

  method valdefinition : 'accumulator -> var * term -> 'accumulator = fun accu -> 
  function (var1, term0) -> 
      let accu = (self#term) accu term0 in
     accu

  method recdefinitions : 'accumulator -> var * term -> 'accumulator = fun accu -> 
  function (var1, term0) -> 
      let accu = (self#term) accu term0 in
     accu

  method param : 'accumulator -> param -> 'accumulator = fun accu -> function
  (var0) ->
      accu

  method term : 'accumulator -> term -> 'accumulator = fun accu -> function
  | EVar (annotation1, var0) ->
      self#evar accu (annotation1, var0)
  | EApp (annotation3, x2, term1, term0) ->
      self#eapp accu (annotation3, x2, term1, term0)
  | ELambda (annotation1, lambda_abs0) ->
      self#elambda accu (annotation1, lambda_abs0)
  | ELet (annotation1, let_abs0) ->
      self#elet accu (annotation1, let_abs0)
  | ELetRec (annotation1, rec_abs0) ->
      self#eletrec accu (annotation1, rec_abs0)
  | EConstant (annotation1, primitive0) ->
      self#econstant accu (annotation1, primitive0)
  | EMatch (annotation2, term1, clauses0) ->
      self#ematch accu (annotation2, term1, clauses0)
  | EAnnot (annotation2, term1, ty0) ->
      self#eannot accu (annotation2, term1, ty0)

  method evar : 'accumulator -> annotation * var -> 'accumulator = fun accu -> 
  function (annotation1, var0) -> 
      let accu = (self#annotation) accu annotation1 in
     accu

  method eapp : 'accumulator -> annotation * ( Level.t ) * term * term -> 'accumulator = fun accu -> 
  function (annotation3, x2, term1, term0) -> 
      let accu = (self#annotation) accu annotation3 in
      let accu = (self#term) accu term1 in
      let accu = (self#term) accu term0 in
     accu

  method elambda : 'accumulator -> annotation * opaque_lambda_abs -> 'accumulator = fun accu -> 
  function (annotation1, lambda_abs0) -> 
      let accu = (self#annotation) accu annotation1 in
      let accu = self#lambda_abs accu (open_lambda_abs lambda_abs0) in
     accu

  method elet : 'accumulator -> annotation * opaque_let_abs -> 'accumulator = fun accu -> 
  function (annotation1, let_abs0) -> 
      let accu = (self#annotation) accu annotation1 in
      let accu = self#let_abs accu (open_let_abs let_abs0) in
     accu

  method eletrec : 'accumulator -> annotation * opaque_rec_abs -> 'accumulator = fun accu -> 
  function (annotation1, rec_abs0) -> 
      let accu = (self#annotation) accu annotation1 in
      let accu = self#rec_abs accu (open_rec_abs rec_abs0) in
     accu

  method econstant : 'accumulator -> annotation * primitive -> 'accumulator = fun accu -> 
  function (annotation1, primitive0) -> 
      let accu = (self#annotation) accu annotation1 in
      let accu = (self#primitive) accu primitive0 in
     accu

  method ematch : 'accumulator -> annotation * term * clause list -> 'accumulator = fun accu -> 
  function (annotation2, term1, clauses0) -> 
      let accu = (self#annotation) accu annotation2 in
      let accu = (self#term) accu term1 in
      let accu = List.fold_left (self#clause) accu clauses0 in
     accu

  method eannot : 'accumulator -> annotation * term * ty -> 'accumulator = fun accu -> 
  function (annotation2, term1, ty0) -> 
      let accu = (self#annotation) accu annotation2 in
      let accu = (self#term) accu term1 in
      let accu = (self#ty) accu ty0 in
     accu

  method binder : 'accumulator -> binder -> 'accumulator = fun accu -> function
  | AnnotatedBind (var1, ty0) ->
      self#annotatedbind accu (var1, ty0)
  | Bind (var0) ->
      self#bind accu (var0)

  method annotatedbind : 'accumulator -> var * ty -> 'accumulator = fun accu -> 
  function (var1, ty0) -> 
      let accu = (self#ty) accu ty0 in
     accu

  method bind : 'accumulator -> var -> 'accumulator = fun accu -> 
  function (var0) -> 
     accu

  method clause : 'accumulator -> clause -> 'accumulator = fun accu -> function
  (clause_abs0) ->
      let accu = self#clause_abs accu (open_clause_abs clause_abs0) in
      accu

  method pattern : 'accumulator -> pattern -> 'accumulator = fun accu -> function
  | PVar (annotation1, var0) ->
      self#pvar accu (annotation1, var0)
  | PDataCon (annotation2, constructor1, patterns0) ->
      self#pdatacon accu (annotation2, constructor1, patterns0)

  method pvar : 'accumulator -> annotation * var -> 'accumulator = fun accu -> 
  function (annotation1, var0) -> 
      let accu = (self#annotation) accu annotation1 in
     accu

  method pdatacon : 'accumulator -> annotation * constructor * pattern list -> 'accumulator = fun accu -> 
  function (annotation2, constructor1, patterns0) -> 
      let accu = (self#annotation) accu annotation2 in
      let accu = (self#constructor) accu constructor1 in
      let accu = List.fold_left (self#pattern) accu patterns0 in
     accu

  method constructor : 'accumulator -> constructor -> 'accumulator = fun accu -> function
  (var0) ->
      accu

  method primitive : 'accumulator -> primitive -> 'accumulator = fun accu -> function
  | Int (x0) ->
      self#int accu (x0)
  | String (x0) ->
      self#string accu (x0)

  method int : 'accumulator -> ( int ) -> 'accumulator = fun accu -> 
  function (x0) -> 
     accu

  method string : 'accumulator -> ( string ) -> 'accumulator = fun accu -> 
  function (x0) -> 
     accu

  method ty : 'accumulator -> ty -> 'accumulator = fun accu -> function
  | TyArrow (annotation2, ty1, ty0) ->
      self#tyarrow accu (annotation2, ty1, ty0)
  | TyApp (annotation2, var1, tys0) ->
      self#tyapp accu (annotation2, var1, tys0)
  | TyVar (annotation1, var0) ->
      self#tyvar accu (annotation1, var0)
  | TyConstant (annotation1, ty_constant0) ->
      self#tyconstant accu (annotation1, ty_constant0)

  method tyarrow : 'accumulator -> annotation * ty * ty -> 'accumulator = fun accu -> 
  function (annotation2, ty1, ty0) -> 
      let accu = (self#annotation) accu annotation2 in
      let accu = (self#ty) accu ty1 in
      let accu = (self#ty) accu ty0 in
     accu

  method tyapp : 'accumulator -> annotation * var * ty list -> 'accumulator = fun accu -> 
  function (annotation2, var1, tys0) -> 
      let accu = (self#annotation) accu annotation2 in
      let accu = List.fold_left (self#ty) accu tys0 in
     accu

  method tyvar : 'accumulator -> annotation * var -> 'accumulator = fun accu -> 
  function (annotation1, var0) -> 
      let accu = (self#annotation) accu annotation1 in
     accu

  method tyconstant : 'accumulator -> annotation * ty_constant -> 'accumulator = fun accu -> 
  function (annotation1, ty_constant0) -> 
      let accu = (self#annotation) accu annotation1 in
      let accu = (self#ty_constant) accu ty_constant0 in
     accu

  method ty_constant : 'accumulator -> ty_constant -> 'accumulator = fun accu -> function
  | TyInt ->
      self#tyint accu
  | TyString ->
      self#tystring accu

  method tyint : 'accumulator -> 'accumulator = fun accu ->      accu

  method tystring : 'accumulator -> 'accumulator = fun accu ->      accu

  method type_definition : 'accumulator -> type_definition -> 'accumulator = fun accu -> function
  | AlgebraicDataType (datatype_defs0) ->
      self#algebraicdatatype accu (datatype_defs0)

  method algebraicdatatype : 'accumulator -> datatype_def list -> 'accumulator = fun accu -> 
  function (datatype_defs0) -> 
      let accu = List.fold_left (self#datatype_def) accu datatype_defs0 in
     accu

  method datatype_def : 'accumulator -> datatype_def -> 'accumulator = fun accu -> function
  | DataType (var1, tys0) ->
      self#datatype accu (var1, tys0)

  method datatype : 'accumulator -> var * ty list -> 'accumulator = fun accu -> 
  function (var1, tys0) -> 
      let accu = List.fold_left (self#ty) accu tys0 in
     accu

  method toplevel_abs : 'accumulator -> toplevel_abs -> 'accumulator = fun accu -> function
  (toplevel_definition1, program0) ->
      let accu = (self#toplevel_definition) accu toplevel_definition1 in
      let accu = (self#program) accu program0 in
      accu

  method lambda_abs : 'accumulator -> lambda_abs -> 'accumulator = fun accu -> function
  (binder1, term0) ->
      let accu = (self#binder) accu binder1 in
      let accu = (self#term) accu term0 in
      accu

  method let_abs : 'accumulator -> let_abs -> 'accumulator = fun accu -> function
  (var2, term1, term0) ->
      let accu = (self#term) accu term1 in
      let accu = (self#term) accu term0 in
      accu

  method rec_abs : 'accumulator -> rec_abs -> 'accumulator = fun accu -> function
  (var2, term1, term0) ->
      let accu = (self#term) accu term1 in
      let accu = (self#term) accu term0 in
      accu

  method clause_abs : 'accumulator -> clause_abs -> 'accumulator = fun accu -> function
  (annotation2, pattern1, term0) ->
      let accu = (self#annotation) accu annotation2 in
      let accu = (self#pattern) accu pattern1 in
      let accu = (self#term) accu term0 in
      accu

end
