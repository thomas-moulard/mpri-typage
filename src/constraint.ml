(* This file was generated from constraint.mla. Do not edit! *)

module Identifier = AlphaLib.Atom.String

exception IncompatibleStructures
exception UnboundIdentifier of Identifier.t

module Id = AlphaLib.Atom.Make(Identifier)

module Var = AlphaLib.Atom.Make(Identifier)

module Raw = struct

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

 and opaque_clet_ty_abs = {
    mutable clet_ty_abs_delayed: Id.Subst.t * Var.Subst.t;
    mutable clet_ty_abs: clet_ty_abs
  }

 and exists_abs = 
  ex_var list * ty_constraint

 and opaque_exists_abs = {
    mutable exists_abs_delayed: Id.Subst.t * Var.Subst.t;
    mutable exists_abs: exists_abs
  }

 and newconstant_abs = 
  var * ty_constraint

 and opaque_newconstant_abs = {
    mutable newconstant_abs_delayed: Id.Subst.t * Var.Subst.t;
    mutable newconstant_abs: newconstant_abs
  }

 and clet_scheme_abs = 
  ex_var list * ty_constraint * ty

 and opaque_clet_scheme_abs = {
    mutable clet_scheme_abs_delayed: Id.Subst.t * Var.Subst.t;
    mutable clet_scheme_abs: clet_scheme_abs
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

and import_ty_constraint : id Identifier.Map.t * var Identifier.Map.t -> Raw.ty_constraint -> ty_constraint = fun (id_env, var_env) -> function
  | Raw.CEq (ty1, ty0) ->
      CEq ((import_ty (var_env)) ty1, (import_ty (var_env)) ty0)
  | Raw.CInst (id1, ty0) ->
      CInst (id_atom_env_find id1 id_env, (import_ty (var_env)) ty0)
  | Raw.CLet (clet_ty_abs0) ->
      let (id_bvars) = bvi_clet_ty_abs clet_ty_abs0 in 
      let id_ienv = idmap_override Id.Atom.freshb id_bvars id_env in
      let clet_ty_abs0 = import_clet_ty_abs (id_env, var_env, id_ienv) clet_ty_abs0 in
      CLet (create_clet_ty_abs clet_ty_abs0)
  | Raw.CAnd (ty_constraint1, ty_constraint0) ->
      CAnd ((import_ty_constraint (id_env, var_env)) ty_constraint1, (import_ty_constraint (id_env, var_env)) ty_constraint0)
  | Raw.CExists (exists_abs0) ->
      let (var_bvars) = bvi_exists_abs exists_abs0 in 
      let var_ienv = idmap_override Var.Atom.freshb var_bvars var_env in
      let exists_abs0 = import_exists_abs (id_env, var_ienv) exists_abs0 in
      CExists (create_exists_abs exists_abs0)
  | Raw.CNewConstant (newconstant_abs0) ->
      let (var_bvars) = bvi_newconstant_abs newconstant_abs0 in 
      let var_ienv = idmap_override Var.Atom.freshb var_bvars var_env in
      let newconstant_abs0 = import_newconstant_abs (id_env, var_ienv) newconstant_abs0 in
      CNewConstant (create_newconstant_abs newconstant_abs0)
  | Raw.CTrue ->
      CTrue
  | Raw.CFalse ->
      CFalse
  | Raw.CDump ->
      CDump

and subst_ty_constraint : Id.Subst.t * Var.Subst.t -> ty_constraint -> ty_constraint = fun (id_env, var_env) -> function
  | CEq (ty1, ty0) ->
      CEq ((subst_ty (var_env)) ty1, (subst_ty (var_env)) ty0)
  | CInst (id1, ty0) ->
      CInst (Id.Subst.lookup id1 id_env, (subst_ty (var_env)) ty0)
  | CLet (clet_ty_abs0) ->
      CLet (apply_clet_ty_abs (id_env, var_env) clet_ty_abs0)
  | CAnd (ty_constraint1, ty_constraint0) ->
      CAnd ((subst_ty_constraint (id_env, var_env)) ty_constraint1, (subst_ty_constraint (id_env, var_env)) ty_constraint0)
  | CExists (exists_abs0) ->
      CExists (apply_exists_abs (id_env, var_env) exists_abs0)
  | CNewConstant (newconstant_abs0) ->
      CNewConstant (apply_newconstant_abs (id_env, var_env) newconstant_abs0)
  | CTrue ->
      CTrue
  | CFalse ->
      CFalse
  | CDump ->
      CDump

and export_ty_constraint : Id.AtomIdMap.t * Var.AtomIdMap.t -> ty_constraint -> Raw.ty_constraint = fun (id_m, var_m) -> function
  | CEq (ty1, ty0) ->
      Raw.CEq ((export_ty (var_m)) ty1, (export_ty (var_m)) ty0)
  | CInst (id1, ty0) ->
      Raw.CInst (Id.AtomIdMap.lookup id1 id_m, (export_ty (var_m)) ty0)
  | CLet (clet_ty_abs0) ->
      let clet_ty_abs = open_clet_ty_abs clet_ty_abs0 in
      let (id_bvars) = bound_clet_ty_abs clet_ty_abs in
      let id_im = Id.AtomIdMap.add_set id_bvars id_m in
      Raw.CLet (export_clet_ty_abs (id_m, var_m, id_im) clet_ty_abs)
  | CAnd (ty_constraint1, ty_constraint0) ->
      Raw.CAnd ((export_ty_constraint (id_m, var_m)) ty_constraint1, (export_ty_constraint (id_m, var_m)) ty_constraint0)
  | CExists (exists_abs0) ->
      let exists_abs = open_exists_abs exists_abs0 in
      let (var_bvars) = bound_exists_abs exists_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.CExists (export_exists_abs (id_m, var_im) exists_abs)
  | CNewConstant (newconstant_abs0) ->
      let newconstant_abs = open_newconstant_abs newconstant_abs0 in
      let (var_bvars) = bound_newconstant_abs newconstant_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.CNewConstant (export_newconstant_abs (id_m, var_im) newconstant_abs)
  | CTrue ->
      Raw.CTrue
  | CFalse ->
      Raw.CFalse
  | CDump ->
      Raw.CDump

and free_ty_constraint : ty_constraint -> Id.AtomSet.t * Var.AtomSet.t = 
  function ty_constraint -> free_accu_ty_constraint (Id.AtomSet.empty, Var.AtomSet.empty) ty_constraint

and free_accu_ty_constraint = fun (id_fvars, var_fvars) -> function
  | CEq (ty1, ty0) ->
      let (var_fvars) = free_accu_ty (var_fvars) ty1 in 
      let (var_fvars) = free_accu_ty (var_fvars) ty0 in 
      (id_fvars, var_fvars)
  | CInst (id1, ty0) ->
      let id_fvars = Id.AtomSet.add id1 id_fvars in
      let (var_fvars) = free_accu_ty (var_fvars) ty0 in 
      (id_fvars, var_fvars)
  | CLet (clet_ty_abs0) ->
      let clet_ty_abs = open_clet_ty_abs clet_ty_abs0 in
      let (id_bvars, id_ifvars, id_ofvars, var_ofvars) = bound_free_accu_clet_ty_abs (Id.AtomSet.empty, Id.AtomSet.empty, id_fvars, var_fvars) clet_ty_abs in
      let id_fvars = Id.AtomSet.union id_fvars (Id.AtomSet.diff id_ifvars id_bvars) in
      (id_fvars, var_fvars)
  | CAnd (ty_constraint1, ty_constraint0) ->
      let (id_fvars, var_fvars) = free_accu_ty_constraint (id_fvars, var_fvars) ty_constraint1 in 
      let (id_fvars, var_fvars) = free_accu_ty_constraint (id_fvars, var_fvars) ty_constraint0 in 
      (id_fvars, var_fvars)
  | CExists (exists_abs0) ->
      let exists_abs = open_exists_abs exists_abs0 in
      let (var_bvars, var_ifvars, id_ofvars) = bound_free_accu_exists_abs (Var.AtomSet.empty, Var.AtomSet.empty, id_fvars) exists_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (id_fvars, var_fvars)
  | CNewConstant (newconstant_abs0) ->
      let newconstant_abs = open_newconstant_abs newconstant_abs0 in
      let (var_bvars, var_ifvars, id_ofvars) = bound_free_accu_newconstant_abs (Var.AtomSet.empty, Var.AtomSet.empty, id_fvars) newconstant_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (id_fvars, var_fvars)
  | CTrue ->
      (id_fvars, var_fvars)
  | CFalse ->
      (id_fvars, var_fvars)
  | CDump ->
      (id_fvars, var_fvars)

and import_clet_scheme : id Identifier.Map.t * var Identifier.Map.t -> Raw.clet_scheme -> clet_scheme = fun (id_env, var_env) -> function
  (clet_scheme_abs0) ->
      let (var_bvars) = bvi_clet_scheme_abs clet_scheme_abs0 in 
      let var_ienv = idmap_override Var.Atom.freshb var_bvars var_env in
      let clet_scheme_abs0 = import_clet_scheme_abs (id_env, var_ienv) clet_scheme_abs0 in
    (create_clet_scheme_abs clet_scheme_abs0)

and subst_clet_scheme : Id.Subst.t * Var.Subst.t -> clet_scheme -> clet_scheme = fun (id_env, var_env) -> function
  (clet_scheme_abs0) ->
    (apply_clet_scheme_abs (id_env, var_env) clet_scheme_abs0)

and export_clet_scheme : Id.AtomIdMap.t * Var.AtomIdMap.t -> clet_scheme -> Raw.clet_scheme = fun (id_m, var_m) -> function
  (clet_scheme_abs0) ->
      let clet_scheme_abs = open_clet_scheme_abs clet_scheme_abs0 in
      let (var_bvars) = bound_clet_scheme_abs clet_scheme_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_clet_scheme_abs (id_m, var_im) clet_scheme_abs)

and free_clet_scheme : clet_scheme -> Id.AtomSet.t * Var.AtomSet.t = 
  function clet_scheme -> free_accu_clet_scheme (Id.AtomSet.empty, Var.AtomSet.empty) clet_scheme

and free_accu_clet_scheme = fun (id_fvars, var_fvars) -> function
  (clet_scheme_abs0) ->
      let clet_scheme_abs = open_clet_scheme_abs clet_scheme_abs0 in
      let (var_bvars, var_ifvars, id_ofvars) = bound_free_accu_clet_scheme_abs (Var.AtomSet.empty, Var.AtomSet.empty, id_fvars) clet_scheme_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (id_fvars, var_fvars)

and subst_ex_var : Var.Subst.t -> ex_var -> ex_var = fun (var_ienv) -> function
  (var0) ->
    (Var.Subst.lookup var0 var_ienv)

and bound_ex_var : ex_var -> Var.AtomSet.t = 
  function ex_var -> bound_accu_ex_var (Var.AtomSet.empty) ex_var

and bound_free_ex_var : ex_var -> Var.AtomSet.t = 
  function ex_var -> bound_free_accu_ex_var (Var.AtomSet.empty) ex_var

and export_ex_var : Var.AtomIdMap.t -> ex_var -> Raw.ex_var = fun (var_im) -> function
  (var0) ->
    (Var.AtomIdMap.lookup var0 var_im)

and import_ex_var = fun (var_ienv) -> function
  (var0) ->
    (id_atom_env_find var0 var_ienv)

and bvi_accu_ex_var = fun (var_bvars) -> function
  (var0) ->
      let var_bvars = Identifier.Map.add var0 () var_bvars in
      (var_bvars)

and bvi_ex_var = 
  function ex_var -> bvi_accu_ex_var (Identifier.Map.empty) ex_var

and bound_accu_ex_var = fun (var_bvars) -> function
  (var0) ->
      let var_bvars = Var.AtomSet.add var0 var_bvars in
      (var_bvars)

and cmp_ex_var : (Var.Subst.t) * (Var.Subst.t) -> ex_var -> ex_var -> (Var.Subst.t) * (Var.Subst.t) = fun ((var_env1), (var_env2)) -> fun a_cmp_ex_var1 a_cmp_ex_var2 -> match a_cmp_ex_var1, a_cmp_ex_var2 with
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

and bound_free_accu_ex_var = fun (var_bvars) -> function
  (var0) ->
      let var_bvars = Var.AtomSet.add var0 var_bvars in
      (var_bvars)

and import_ty_var : var Identifier.Map.t -> Raw.ty_var -> ty_var = fun (var_env) -> function
  (var0) ->
    (id_atom_env_find var0 var_env)

and subst_ty_var : Var.Subst.t -> ty_var -> ty_var = fun (var_env) -> function
  (var0) ->
    (Var.Subst.lookup var0 var_env)

and export_ty_var : Var.AtomIdMap.t -> ty_var -> Raw.ty_var = fun (var_m) -> function
  (var0) ->
    (Var.AtomIdMap.lookup var0 var_m)

and free_ty_var : ty_var -> Var.AtomSet.t = 
  function ty_var -> free_accu_ty_var (Var.AtomSet.empty) ty_var

and free_accu_ty_var = fun (var_fvars) -> function
  (var0) ->
      let var_fvars = Var.AtomSet.add var0 var_fvars in
      (var_fvars)

and import_ty : var Identifier.Map.t -> Raw.ty -> ty = fun (var_env) -> function
  | Raw.CTyVar (var0) ->
      CTyVar (id_atom_env_find var0 var_env)
  | Raw.CTyArrow (ty1, ty0) ->
      CTyArrow ((import_ty (var_env)) ty1, (import_ty (var_env)) ty0)
  | Raw.CTyApp (var1, tys0) ->
      CTyApp (id_atom_env_find var1 var_env, List.map (import_ty (var_env)) tys0)
  | Raw.CTyConstant (ty_constant0) ->
      CTyConstant ((import_ty_constant ()) ty_constant0)

and subst_ty : Var.Subst.t -> ty -> ty = fun (var_env) -> function
  | CTyVar (var0) ->
      CTyVar (Var.Subst.lookup var0 var_env)
  | CTyArrow (ty1, ty0) ->
      CTyArrow ((subst_ty (var_env)) ty1, (subst_ty (var_env)) ty0)
  | CTyApp (var1, tys0) ->
      CTyApp (Var.Subst.lookup var1 var_env, List.map (subst_ty (var_env)) tys0)
  | CTyConstant (ty_constant0) ->
      CTyConstant ((subst_ty_constant ()) ty_constant0)

and export_ty : Var.AtomIdMap.t -> ty -> Raw.ty = fun (var_m) -> function
  | CTyVar (var0) ->
      Raw.CTyVar (Var.AtomIdMap.lookup var0 var_m)
  | CTyArrow (ty1, ty0) ->
      Raw.CTyArrow ((export_ty (var_m)) ty1, (export_ty (var_m)) ty0)
  | CTyApp (var1, tys0) ->
      Raw.CTyApp (Var.AtomIdMap.lookup var1 var_m, List.map (export_ty (var_m)) tys0)
  | CTyConstant (ty_constant0) ->
      Raw.CTyConstant ((export_ty_constant ()) ty_constant0)

and free_ty : ty -> Var.AtomSet.t = 
  function ty -> free_accu_ty (Var.AtomSet.empty) ty

and free_accu_ty = fun (var_fvars) -> function
  | CTyVar (var0) ->
      let var_fvars = Var.AtomSet.add var0 var_fvars in
      (var_fvars)
  | CTyArrow (ty1, ty0) ->
      let (var_fvars) = free_accu_ty (var_fvars) ty1 in 
      let (var_fvars) = free_accu_ty (var_fvars) ty0 in 
      (var_fvars)
  | CTyApp (var1, tys0) ->
      let var_fvars = Var.AtomSet.add var1 var_fvars in
      let (var_fvars) = List.fold_left free_accu_ty (var_fvars) tys0 in 
      (var_fvars)
  | CTyConstant (ty_constant0) ->
      let () = free_accu_ty_constant () ty_constant0 in 
      (var_fvars)

and import_ty_constant : unit -> Raw.ty_constant -> ty_constant = fun () -> function
  | Raw.CTyInt ->
      CTyInt
  | Raw.CTyString ->
      CTyString

and subst_ty_constant : unit -> ty_constant -> ty_constant = fun () -> function
  | CTyInt ->
      CTyInt
  | CTyString ->
      CTyString

and export_ty_constant : unit -> ty_constant -> Raw.ty_constant = fun () -> function
  | CTyInt ->
      Raw.CTyInt
  | CTyString ->
      Raw.CTyString

and free_ty_constant : ty_constant -> unit = 
  function ty_constant -> free_accu_ty_constant () ty_constant

and free_accu_ty_constant = fun () -> function
  | CTyInt ->
      ()
  | CTyString ->
      ()

and subst_clet_ty_abs : Id.Subst.t * Var.Subst.t * Id.Subst.t -> clet_ty_abs -> clet_ty_abs = fun (id_oenv, var_oenv, id_ienv) -> function
  (id2, clet_scheme1, ty_constraint0) ->
    (Id.Subst.lookup id2 id_ienv, (subst_clet_scheme (id_oenv, var_oenv)) clet_scheme1, (subst_ty_constraint (id_ienv, var_oenv)) ty_constraint0)

and bound_clet_ty_abs : clet_ty_abs -> Id.AtomSet.t = 
  function clet_ty_abs -> bound_accu_clet_ty_abs (Id.AtomSet.empty) clet_ty_abs

and bound_free_clet_ty_abs : clet_ty_abs -> Id.AtomSet.t * Id.AtomSet.t * Id.AtomSet.t * Var.AtomSet.t = 
  function clet_ty_abs -> bound_free_accu_clet_ty_abs (Id.AtomSet.empty, Id.AtomSet.empty, Id.AtomSet.empty, Var.AtomSet.empty) clet_ty_abs

and export_clet_ty_abs : Id.AtomIdMap.t * Var.AtomIdMap.t * Id.AtomIdMap.t -> clet_ty_abs -> Raw.clet_ty_abs = fun (id_om, var_om, id_im) -> function
  (id2, clet_scheme1, ty_constraint0) ->
    (Id.AtomIdMap.lookup id2 id_im, (export_clet_scheme (id_om, var_om)) clet_scheme1, (export_ty_constraint (id_im, var_om)) ty_constraint0)

and import_clet_ty_abs = fun (id_oenv, var_oenv, id_ienv) -> function
  (id2, clet_scheme1, ty_constraint0) ->
    (id_atom_env_find id2 id_ienv, (import_clet_scheme (id_oenv, var_oenv)) clet_scheme1, (import_ty_constraint (id_ienv, var_oenv)) ty_constraint0)

and bvi_accu_clet_ty_abs = fun (id_bvars) -> function
  (id2, clet_scheme1, ty_constraint0) ->
      let id_bvars = Identifier.Map.add id2 () id_bvars in
      (id_bvars)

and bvi_clet_ty_abs = 
  function clet_ty_abs -> bvi_accu_clet_ty_abs (Identifier.Map.empty) clet_ty_abs

and bound_accu_clet_ty_abs = fun (id_bvars) -> function
  (id2, clet_scheme1, ty_constraint0) ->
      let id_bvars = Id.AtomSet.add id2 id_bvars in
      (id_bvars)

and cmp_clet_ty_abs : (Id.Subst.t) * (Id.Subst.t) -> clet_ty_abs -> clet_ty_abs -> (Id.Subst.t) * (Id.Subst.t) = fun ((id_env1), (id_env2)) -> fun a_cmp_clet_ty_abs1 a_cmp_clet_ty_abs2 -> match a_cmp_clet_ty_abs1, a_cmp_clet_ty_abs2 with
  (id2, clet_scheme1, ty_constraint0), (id5, clet_scheme4, ty_constraint3) ->
let id_env1 =
    let imvar1 = Id.Subst.lookup id2 id_env1 in
      if Id.Atom.equal imvar1 id5 then
        id_env1
      else if Id.Atom.equal imvar1 id2 then
        Id.Subst.add id2 id5 id_env1
      else raise IncompatibleStructures
  and id_env2 =
    let imvar2 = Id.Subst.lookup id5 id_env2 in
      if Id.Atom.equal imvar2 id2 then
        id_env2
      else if Id.Atom.equal imvar2 id5 then
        Id.Subst.add id5 id2 id_env2
      else raise IncompatibleStructures
  in 
    ((id_env1), (id_env2))

and bound_free_accu_clet_ty_abs = fun (id_bvars, id_ifvars, id_ofvars, var_ofvars) -> function
  (id2, clet_scheme1, ty_constraint0) ->
      let id_bvars = Id.AtomSet.add id2 id_bvars in
      let (id_ofvars, var_ofvars) = free_accu_clet_scheme (id_ofvars, var_ofvars) clet_scheme1 in
      let (id_ifvars, var_ofvars) = free_accu_ty_constraint (id_ifvars, var_ofvars) ty_constraint0 in
      (id_bvars, id_ifvars, id_ofvars, var_ofvars)

and create_clet_ty_abs : clet_ty_abs -> opaque_clet_ty_abs = 
  function body -> {
    clet_ty_abs_delayed = (Id.Subst.id, Var.Subst.id);
    clet_ty_abs = body
  }

and open_clet_ty_abs : opaque_clet_ty_abs -> clet_ty_abs = function abstraction ->
  let (id_delayed, var_delayed) = abstraction.clet_ty_abs_delayed in
  let body = abstraction.clet_ty_abs in
  let (id_bvars) = bound_clet_ty_abs body in
  let id_env = Id.Subst.freshen id_bvars id_delayed in
  let body = subst_clet_ty_abs (id_delayed, var_delayed, id_env) body in
  if not (Id.Subst.is_id id_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.clet_ty_abs_delayed <- (Id.Subst.id, Var.Subst.id);
    abstraction.clet_ty_abs <- body
  end;
  body

and open2_clet_ty_abs : opaque_clet_ty_abs -> opaque_clet_ty_abs -> clet_ty_abs * clet_ty_abs = fun abstraction1 abstraction2 ->
  let (id_delayed1, var_delayed1) = abstraction1.clet_ty_abs_delayed in
  let body1 = abstraction1.clet_ty_abs in
  let (id_delayed2, var_delayed2) = abstraction2.clet_ty_abs_delayed in
  let body2 = abstraction2.clet_ty_abs in
  let (id_rho1), (id_rho2) = cmp_clet_ty_abs ((Id.Subst.id), (Id.Subst.id)) body1 body2 in
  let id_subst1 = Id.Subst.map Id.Atom.fresha id_rho1 in
  let id_subst2 = Id.Subst.compose id_subst1 id_rho2 in
  let id_subst1 = Id.Subst.union id_subst1 id_delayed1 in
  let id_subst2 = Id.Subst.union id_subst2 id_delayed2 in

  let body1 = subst_clet_ty_abs (id_delayed1, var_delayed1, id_subst1) body1 in
  let body2 = subst_clet_ty_abs (id_delayed2, var_delayed2, id_subst2) body2 in
  if not (Id.Subst.is_id id_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.clet_ty_abs_delayed <- (Id.Subst.id, Var.Subst.id);
    abstraction1.clet_ty_abs <- body1
  end;
  if not (Id.Subst.is_id id_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.clet_ty_abs_delayed <- (Id.Subst.id, Var.Subst.id);
    abstraction2.clet_ty_abs <- body2
  end;
  (body1, body2)

and apply_clet_ty_abs = 
  fun (id_env, var_env) abstraction ->
    let (id_delayed, var_delayed) = abstraction.clet_ty_abs_delayed in {
      abstraction with clet_ty_abs_delayed = (Id.Subst.compose id_env id_delayed, Var.Subst.compose var_env var_delayed)
    }

and subst_exists_abs : Id.Subst.t * Var.Subst.t -> exists_abs -> exists_abs = fun (id_oenv, var_ienv) -> function
  (ex_vars1, ty_constraint0) ->
    (List.map (subst_ex_var (var_ienv)) ex_vars1, (subst_ty_constraint (id_oenv, var_ienv)) ty_constraint0)

and bound_exists_abs : exists_abs -> Var.AtomSet.t = 
  function exists_abs -> bound_accu_exists_abs (Var.AtomSet.empty) exists_abs

and bound_free_exists_abs : exists_abs -> Var.AtomSet.t * Var.AtomSet.t * Id.AtomSet.t = 
  function exists_abs -> bound_free_accu_exists_abs (Var.AtomSet.empty, Var.AtomSet.empty, Id.AtomSet.empty) exists_abs

and export_exists_abs : Id.AtomIdMap.t * Var.AtomIdMap.t -> exists_abs -> Raw.exists_abs = fun (id_om, var_im) -> function
  (ex_vars1, ty_constraint0) ->
    (List.map (export_ex_var (var_im)) ex_vars1, (export_ty_constraint (id_om, var_im)) ty_constraint0)

and import_exists_abs = fun (id_oenv, var_ienv) -> function
  (ex_vars1, ty_constraint0) ->
    (List.map (import_ex_var (var_ienv)) ex_vars1, (import_ty_constraint (id_oenv, var_ienv)) ty_constraint0)

and bvi_accu_exists_abs = fun (var_bvars) -> function
  (ex_vars1, ty_constraint0) ->
      let (var_bvars) = List.fold_left bvi_accu_ex_var (var_bvars) ex_vars1 in
      (var_bvars)

and bvi_exists_abs = 
  function exists_abs -> bvi_accu_exists_abs (Identifier.Map.empty) exists_abs

and bound_accu_exists_abs = fun (var_bvars) -> function
  (ex_vars1, ty_constraint0) ->
      let (var_bvars) = List.fold_left bound_accu_ex_var (var_bvars) ex_vars1 in
      (var_bvars)

and cmp_exists_abs : (Var.Subst.t) * (Var.Subst.t) -> exists_abs -> exists_abs -> (Var.Subst.t) * (Var.Subst.t) = fun ((var_env1), (var_env2)) -> fun a_cmp_exists_abs1 a_cmp_exists_abs2 -> match a_cmp_exists_abs1, a_cmp_exists_abs2 with
  (ex_vars1, ty_constraint0), (ex_vars3, ty_constraint2) ->
  let ((var_env1), (var_env2)) = List.fold_left2  cmp_ex_var ((var_env1), (var_env2)) ex_vars1 ex_vars3 in
    ((var_env1), (var_env2))

and bound_free_accu_exists_abs = fun (var_bvars, var_ifvars, id_ofvars) -> function
  (ex_vars1, ty_constraint0) ->
      let (var_bvars) = List.fold_left bound_free_accu_ex_var (var_bvars) ex_vars1 in
      let (id_ofvars, var_ifvars) = free_accu_ty_constraint (id_ofvars, var_ifvars) ty_constraint0 in
      (var_bvars, var_ifvars, id_ofvars)

and create_exists_abs : exists_abs -> opaque_exists_abs = 
  function body -> {
    exists_abs_delayed = (Id.Subst.id, Var.Subst.id);
    exists_abs = body
  }

and open_exists_abs : opaque_exists_abs -> exists_abs = function abstraction ->
  let (id_delayed, var_delayed) = abstraction.exists_abs_delayed in
  let body = abstraction.exists_abs in
  let (var_bvars) = bound_exists_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_exists_abs (id_delayed, var_env) body in
  if not (Id.Subst.is_id id_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.exists_abs_delayed <- (Id.Subst.id, Var.Subst.id);
    abstraction.exists_abs <- body
  end;
  body

and open2_exists_abs : opaque_exists_abs -> opaque_exists_abs -> exists_abs * exists_abs = fun abstraction1 abstraction2 ->
  let (id_delayed1, var_delayed1) = abstraction1.exists_abs_delayed in
  let body1 = abstraction1.exists_abs in
  let (id_delayed2, var_delayed2) = abstraction2.exists_abs_delayed in
  let body2 = abstraction2.exists_abs in
  let (var_rho1), (var_rho2) = cmp_exists_abs ((Var.Subst.id), (Var.Subst.id)) body1 body2 in
  let var_subst1 = Var.Subst.map Var.Atom.fresha var_rho1 in
  let var_subst2 = Var.Subst.compose var_subst1 var_rho2 in
  let var_subst1 = Var.Subst.union var_subst1 var_delayed1 in
  let var_subst2 = Var.Subst.union var_subst2 var_delayed2 in

  let body1 = subst_exists_abs (id_delayed1, var_subst1) body1 in
  let body2 = subst_exists_abs (id_delayed2, var_subst2) body2 in
  if not (Id.Subst.is_id id_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.exists_abs_delayed <- (Id.Subst.id, Var.Subst.id);
    abstraction1.exists_abs <- body1
  end;
  if not (Id.Subst.is_id id_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.exists_abs_delayed <- (Id.Subst.id, Var.Subst.id);
    abstraction2.exists_abs <- body2
  end;
  (body1, body2)

and apply_exists_abs = 
  fun (id_env, var_env) abstraction ->
    let (id_delayed, var_delayed) = abstraction.exists_abs_delayed in {
      abstraction with exists_abs_delayed = (Id.Subst.compose id_env id_delayed, Var.Subst.compose var_env var_delayed)
    }

and subst_newconstant_abs : Id.Subst.t * Var.Subst.t -> newconstant_abs -> newconstant_abs = fun (id_oenv, var_ienv) -> function
  (var1, ty_constraint0) ->
    (Var.Subst.lookup var1 var_ienv, (subst_ty_constraint (id_oenv, var_ienv)) ty_constraint0)

and bound_newconstant_abs : newconstant_abs -> Var.AtomSet.t = 
  function newconstant_abs -> bound_accu_newconstant_abs (Var.AtomSet.empty) newconstant_abs

and bound_free_newconstant_abs : newconstant_abs -> Var.AtomSet.t * Var.AtomSet.t * Id.AtomSet.t = 
  function newconstant_abs -> bound_free_accu_newconstant_abs (Var.AtomSet.empty, Var.AtomSet.empty, Id.AtomSet.empty) newconstant_abs

and export_newconstant_abs : Id.AtomIdMap.t * Var.AtomIdMap.t -> newconstant_abs -> Raw.newconstant_abs = fun (id_om, var_im) -> function
  (var1, ty_constraint0) ->
    (Var.AtomIdMap.lookup var1 var_im, (export_ty_constraint (id_om, var_im)) ty_constraint0)

and import_newconstant_abs = fun (id_oenv, var_ienv) -> function
  (var1, ty_constraint0) ->
    (id_atom_env_find var1 var_ienv, (import_ty_constraint (id_oenv, var_ienv)) ty_constraint0)

and bvi_accu_newconstant_abs = fun (var_bvars) -> function
  (var1, ty_constraint0) ->
      let var_bvars = Identifier.Map.add var1 () var_bvars in
      (var_bvars)

and bvi_newconstant_abs = 
  function newconstant_abs -> bvi_accu_newconstant_abs (Identifier.Map.empty) newconstant_abs

and bound_accu_newconstant_abs = fun (var_bvars) -> function
  (var1, ty_constraint0) ->
      let var_bvars = Var.AtomSet.add var1 var_bvars in
      (var_bvars)

and cmp_newconstant_abs : (Var.Subst.t) * (Var.Subst.t) -> newconstant_abs -> newconstant_abs -> (Var.Subst.t) * (Var.Subst.t) = fun ((var_env1), (var_env2)) -> fun a_cmp_newconstant_abs1 a_cmp_newconstant_abs2 -> match a_cmp_newconstant_abs1, a_cmp_newconstant_abs2 with
  (var1, ty_constraint0), (var3, ty_constraint2) ->
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

and bound_free_accu_newconstant_abs = fun (var_bvars, var_ifvars, id_ofvars) -> function
  (var1, ty_constraint0) ->
      let var_bvars = Var.AtomSet.add var1 var_bvars in
      let (id_ofvars, var_ifvars) = free_accu_ty_constraint (id_ofvars, var_ifvars) ty_constraint0 in
      (var_bvars, var_ifvars, id_ofvars)

and create_newconstant_abs : newconstant_abs -> opaque_newconstant_abs = 
  function body -> {
    newconstant_abs_delayed = (Id.Subst.id, Var.Subst.id);
    newconstant_abs = body
  }

and open_newconstant_abs : opaque_newconstant_abs -> newconstant_abs = function abstraction ->
  let (id_delayed, var_delayed) = abstraction.newconstant_abs_delayed in
  let body = abstraction.newconstant_abs in
  let (var_bvars) = bound_newconstant_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_newconstant_abs (id_delayed, var_env) body in
  if not (Id.Subst.is_id id_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.newconstant_abs_delayed <- (Id.Subst.id, Var.Subst.id);
    abstraction.newconstant_abs <- body
  end;
  body

and open2_newconstant_abs : opaque_newconstant_abs -> opaque_newconstant_abs -> newconstant_abs * newconstant_abs = fun abstraction1 abstraction2 ->
  let (id_delayed1, var_delayed1) = abstraction1.newconstant_abs_delayed in
  let body1 = abstraction1.newconstant_abs in
  let (id_delayed2, var_delayed2) = abstraction2.newconstant_abs_delayed in
  let body2 = abstraction2.newconstant_abs in
  let (var_rho1), (var_rho2) = cmp_newconstant_abs ((Var.Subst.id), (Var.Subst.id)) body1 body2 in
  let var_subst1 = Var.Subst.map Var.Atom.fresha var_rho1 in
  let var_subst2 = Var.Subst.compose var_subst1 var_rho2 in
  let var_subst1 = Var.Subst.union var_subst1 var_delayed1 in
  let var_subst2 = Var.Subst.union var_subst2 var_delayed2 in

  let body1 = subst_newconstant_abs (id_delayed1, var_subst1) body1 in
  let body2 = subst_newconstant_abs (id_delayed2, var_subst2) body2 in
  if not (Id.Subst.is_id id_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.newconstant_abs_delayed <- (Id.Subst.id, Var.Subst.id);
    abstraction1.newconstant_abs <- body1
  end;
  if not (Id.Subst.is_id id_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.newconstant_abs_delayed <- (Id.Subst.id, Var.Subst.id);
    abstraction2.newconstant_abs <- body2
  end;
  (body1, body2)

and apply_newconstant_abs = 
  fun (id_env, var_env) abstraction ->
    let (id_delayed, var_delayed) = abstraction.newconstant_abs_delayed in {
      abstraction with newconstant_abs_delayed = (Id.Subst.compose id_env id_delayed, Var.Subst.compose var_env var_delayed)
    }

and subst_clet_scheme_abs : Id.Subst.t * Var.Subst.t -> clet_scheme_abs -> clet_scheme_abs = fun (id_oenv, var_ienv) -> function
  (ex_vars2, ty_constraint1, ty0) ->
    (List.map (subst_ex_var (var_ienv)) ex_vars2, (subst_ty_constraint (id_oenv, var_ienv)) ty_constraint1, (subst_ty (var_ienv)) ty0)

and bound_clet_scheme_abs : clet_scheme_abs -> Var.AtomSet.t = 
  function clet_scheme_abs -> bound_accu_clet_scheme_abs (Var.AtomSet.empty) clet_scheme_abs

and bound_free_clet_scheme_abs : clet_scheme_abs -> Var.AtomSet.t * Var.AtomSet.t * Id.AtomSet.t = 
  function clet_scheme_abs -> bound_free_accu_clet_scheme_abs (Var.AtomSet.empty, Var.AtomSet.empty, Id.AtomSet.empty) clet_scheme_abs

and export_clet_scheme_abs : Id.AtomIdMap.t * Var.AtomIdMap.t -> clet_scheme_abs -> Raw.clet_scheme_abs = fun (id_om, var_im) -> function
  (ex_vars2, ty_constraint1, ty0) ->
    (List.map (export_ex_var (var_im)) ex_vars2, (export_ty_constraint (id_om, var_im)) ty_constraint1, (export_ty (var_im)) ty0)

and import_clet_scheme_abs = fun (id_oenv, var_ienv) -> function
  (ex_vars2, ty_constraint1, ty0) ->
    (List.map (import_ex_var (var_ienv)) ex_vars2, (import_ty_constraint (id_oenv, var_ienv)) ty_constraint1, (import_ty (var_ienv)) ty0)

and bvi_accu_clet_scheme_abs = fun (var_bvars) -> function
  (ex_vars2, ty_constraint1, ty0) ->
      let (var_bvars) = List.fold_left bvi_accu_ex_var (var_bvars) ex_vars2 in
      (var_bvars)

and bvi_clet_scheme_abs = 
  function clet_scheme_abs -> bvi_accu_clet_scheme_abs (Identifier.Map.empty) clet_scheme_abs

and bound_accu_clet_scheme_abs = fun (var_bvars) -> function
  (ex_vars2, ty_constraint1, ty0) ->
      let (var_bvars) = List.fold_left bound_accu_ex_var (var_bvars) ex_vars2 in
      (var_bvars)

and cmp_clet_scheme_abs : (Var.Subst.t) * (Var.Subst.t) -> clet_scheme_abs -> clet_scheme_abs -> (Var.Subst.t) * (Var.Subst.t) = fun ((var_env1), (var_env2)) -> fun a_cmp_clet_scheme_abs1 a_cmp_clet_scheme_abs2 -> match a_cmp_clet_scheme_abs1, a_cmp_clet_scheme_abs2 with
  (ex_vars2, ty_constraint1, ty0), (ex_vars5, ty_constraint4, ty3) ->
  let ((var_env1), (var_env2)) = List.fold_left2  cmp_ex_var ((var_env1), (var_env2)) ex_vars2 ex_vars5 in
    ((var_env1), (var_env2))

and bound_free_accu_clet_scheme_abs = fun (var_bvars, var_ifvars, id_ofvars) -> function
  (ex_vars2, ty_constraint1, ty0) ->
      let (var_bvars) = List.fold_left bound_free_accu_ex_var (var_bvars) ex_vars2 in
      let (id_ofvars, var_ifvars) = free_accu_ty_constraint (id_ofvars, var_ifvars) ty_constraint1 in
      let (var_ifvars) = free_accu_ty (var_ifvars) ty0 in
      (var_bvars, var_ifvars, id_ofvars)

and create_clet_scheme_abs : clet_scheme_abs -> opaque_clet_scheme_abs = 
  function body -> {
    clet_scheme_abs_delayed = (Id.Subst.id, Var.Subst.id);
    clet_scheme_abs = body
  }

and open_clet_scheme_abs : opaque_clet_scheme_abs -> clet_scheme_abs = function abstraction ->
  let (id_delayed, var_delayed) = abstraction.clet_scheme_abs_delayed in
  let body = abstraction.clet_scheme_abs in
  let (var_bvars) = bound_clet_scheme_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_clet_scheme_abs (id_delayed, var_env) body in
  if not (Id.Subst.is_id id_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.clet_scheme_abs_delayed <- (Id.Subst.id, Var.Subst.id);
    abstraction.clet_scheme_abs <- body
  end;
  body

and open2_clet_scheme_abs : opaque_clet_scheme_abs -> opaque_clet_scheme_abs -> clet_scheme_abs * clet_scheme_abs = fun abstraction1 abstraction2 ->
  let (id_delayed1, var_delayed1) = abstraction1.clet_scheme_abs_delayed in
  let body1 = abstraction1.clet_scheme_abs in
  let (id_delayed2, var_delayed2) = abstraction2.clet_scheme_abs_delayed in
  let body2 = abstraction2.clet_scheme_abs in
  let (var_rho1), (var_rho2) = cmp_clet_scheme_abs ((Var.Subst.id), (Var.Subst.id)) body1 body2 in
  let var_subst1 = Var.Subst.map Var.Atom.fresha var_rho1 in
  let var_subst2 = Var.Subst.compose var_subst1 var_rho2 in
  let var_subst1 = Var.Subst.union var_subst1 var_delayed1 in
  let var_subst2 = Var.Subst.union var_subst2 var_delayed2 in

  let body1 = subst_clet_scheme_abs (id_delayed1, var_subst1) body1 in
  let body2 = subst_clet_scheme_abs (id_delayed2, var_subst2) body2 in
  if not (Id.Subst.is_id id_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.clet_scheme_abs_delayed <- (Id.Subst.id, Var.Subst.id);
    abstraction1.clet_scheme_abs <- body1
  end;
  if not (Id.Subst.is_id id_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.clet_scheme_abs_delayed <- (Id.Subst.id, Var.Subst.id);
    abstraction2.clet_scheme_abs <- body2
  end;
  (body1, body2)

and apply_clet_scheme_abs = 
  fun (id_env, var_env) abstraction ->
    let (id_delayed, var_delayed) = abstraction.clet_scheme_abs_delayed in {
      abstraction with clet_scheme_abs_delayed = (Id.Subst.compose id_env id_delayed, Var.Subst.compose var_env var_delayed)
    }

class [ 'accumulator ] fold2 = object(self)

  method ty_constraint : 'accumulator -> ty_constraint -> ty_constraint -> 'accumulator = fun accu -> fun a_ty_constraint1 a_ty_constraint2 -> match a_ty_constraint1, a_ty_constraint2 with
  | CEq (ty1, ty0), CEq (ty3, ty2) ->
      let accu = (self#ty) accu ty1 ty3 in
      let accu = (self#ty) accu ty0 ty2 in
    accu
  | CInst (id1, ty0), CInst (id3, ty2) ->
if not (Id.Atom.equal id1 id3) then raise IncompatibleStructures;
      let accu = (self#ty) accu ty0 ty2 in
    accu
  | CLet (clet_ty_abs0), CLet (clet_ty_abs1) ->
      let clet_ty_abs0, clet_ty_abs1 = open2_clet_ty_abs clet_ty_abs0 clet_ty_abs1 in
      let accu = self#clet_ty_abs accu clet_ty_abs0 clet_ty_abs1 in
    accu
  | CAnd (ty_constraint1, ty_constraint0), CAnd (ty_constraint3, ty_constraint2) ->
      let accu = (self#ty_constraint) accu ty_constraint1 ty_constraint3 in
      let accu = (self#ty_constraint) accu ty_constraint0 ty_constraint2 in
    accu
  | CExists (exists_abs0), CExists (exists_abs1) ->
      let exists_abs0, exists_abs1 = open2_exists_abs exists_abs0 exists_abs1 in
      let accu = self#exists_abs accu exists_abs0 exists_abs1 in
    accu
  | CNewConstant (newconstant_abs0), CNewConstant (newconstant_abs1) ->
      let newconstant_abs0, newconstant_abs1 = open2_newconstant_abs newconstant_abs0 newconstant_abs1 in
      let accu = self#newconstant_abs accu newconstant_abs0 newconstant_abs1 in
    accu
  | CTrue, CTrue ->
    accu
  | CFalse, CFalse ->
    accu
  | CDump, CDump ->
    accu
  | _ -> raise IncompatibleStructures

  method ceq : 'accumulator -> ty * ty -> ty * ty -> 'accumulator = fun accu -> 
fun (ty1, ty0)  (ty3, ty2) -> 
      let accu = (self#ty) accu ty1 ty3 in
      let accu = (self#ty) accu ty0 ty2 in
      accu

  method cinst : 'accumulator -> id * ty -> id * ty -> 'accumulator = fun accu -> 
fun (id1, ty0)  (id3, ty2) -> 
if not (Id.Atom.equal id1 id3) then raise IncompatibleStructures;
      let accu = (self#ty) accu ty0 ty2 in
      accu

  method clet : 'accumulator -> opaque_clet_ty_abs -> opaque_clet_ty_abs -> 'accumulator = fun accu -> 
fun (clet_ty_abs0)  (clet_ty_abs1) -> 
      let clet_ty_abs0, clet_ty_abs1 = open2_clet_ty_abs clet_ty_abs0 clet_ty_abs1 in
      let accu = self#clet_ty_abs accu clet_ty_abs0 clet_ty_abs1 in
      accu

  method cand : 'accumulator -> ty_constraint * ty_constraint -> ty_constraint * ty_constraint -> 'accumulator = fun accu -> 
fun (ty_constraint1, ty_constraint0)  (ty_constraint3, ty_constraint2) -> 
      let accu = (self#ty_constraint) accu ty_constraint1 ty_constraint3 in
      let accu = (self#ty_constraint) accu ty_constraint0 ty_constraint2 in
      accu

  method cexists : 'accumulator -> opaque_exists_abs -> opaque_exists_abs -> 'accumulator = fun accu -> 
fun (exists_abs0)  (exists_abs1) -> 
      let exists_abs0, exists_abs1 = open2_exists_abs exists_abs0 exists_abs1 in
      let accu = self#exists_abs accu exists_abs0 exists_abs1 in
      accu

  method cnewconstant : 'accumulator -> opaque_newconstant_abs -> opaque_newconstant_abs -> 'accumulator = fun accu -> 
fun (newconstant_abs0)  (newconstant_abs1) -> 
      let newconstant_abs0, newconstant_abs1 = open2_newconstant_abs newconstant_abs0 newconstant_abs1 in
      let accu = self#newconstant_abs accu newconstant_abs0 newconstant_abs1 in
      accu

  method ctrue : 'accumulator -> 'accumulator = fun accu -> 
 
      accu

  method cfalse : 'accumulator -> 'accumulator = fun accu -> 
 
      accu

  method cdump : 'accumulator -> 'accumulator = fun accu -> 
 
      accu

  method clet_scheme : 'accumulator -> clet_scheme -> clet_scheme -> 'accumulator = fun accu -> fun a_clet_scheme1 a_clet_scheme2 -> match a_clet_scheme1, a_clet_scheme2 with
  (clet_scheme_abs0), (clet_scheme_abs1) ->
      let clet_scheme_abs0, clet_scheme_abs1 = open2_clet_scheme_abs clet_scheme_abs0 clet_scheme_abs1 in
      let accu = self#clet_scheme_abs accu clet_scheme_abs0 clet_scheme_abs1 in
    accu

  method ex_var : 'accumulator -> ex_var -> ex_var -> 'accumulator = fun accu -> fun a_ex_var1 a_ex_var2 -> match a_ex_var1, a_ex_var2 with
  (var0), (var1) ->
assert (Var.Atom.equal var0 var1);
    accu

  method ty_var : 'accumulator -> ty_var -> ty_var -> 'accumulator = fun accu -> fun a_ty_var1 a_ty_var2 -> match a_ty_var1, a_ty_var2 with
  (var0), (var1) ->
if not (Var.Atom.equal var0 var1) then raise IncompatibleStructures;
    accu

  method ty : 'accumulator -> ty -> ty -> 'accumulator = fun accu -> fun a_ty1 a_ty2 -> match a_ty1, a_ty2 with
  | CTyVar (var0), CTyVar (var1) ->
if not (Var.Atom.equal var0 var1) then raise IncompatibleStructures;
    accu
  | CTyArrow (ty1, ty0), CTyArrow (ty3, ty2) ->
      let accu = (self#ty) accu ty1 ty3 in
      let accu = (self#ty) accu ty0 ty2 in
    accu
  | CTyApp (var1, tys0), CTyApp (var3, tys2) ->
if not (Var.Atom.equal var1 var3) then raise IncompatibleStructures;
      let accu = List.fold_left2 (self#ty) accu tys0 tys2 in
    accu
  | CTyConstant (ty_constant0), CTyConstant (ty_constant1) ->
      let accu = (self#ty_constant) accu ty_constant0 ty_constant1 in
    accu
  | _ -> raise IncompatibleStructures

  method ctyvar : 'accumulator -> var -> var -> 'accumulator = fun accu -> 
fun (var0)  (var1) -> 
if not (Var.Atom.equal var0 var1) then raise IncompatibleStructures;
      accu

  method ctyarrow : 'accumulator -> ty * ty -> ty * ty -> 'accumulator = fun accu -> 
fun (ty1, ty0)  (ty3, ty2) -> 
      let accu = (self#ty) accu ty1 ty3 in
      let accu = (self#ty) accu ty0 ty2 in
      accu

  method ctyapp : 'accumulator -> var * ty list -> var * ty list -> 'accumulator = fun accu -> 
fun (var1, tys0)  (var3, tys2) -> 
if not (Var.Atom.equal var1 var3) then raise IncompatibleStructures;
      let accu = List.fold_left2 (self#ty) accu tys0 tys2 in
      accu

  method ctyconstant : 'accumulator -> ty_constant -> ty_constant -> 'accumulator = fun accu -> 
fun (ty_constant0)  (ty_constant1) -> 
      let accu = (self#ty_constant) accu ty_constant0 ty_constant1 in
      accu

  method ty_constant : 'accumulator -> ty_constant -> ty_constant -> 'accumulator = fun accu -> fun a_ty_constant1 a_ty_constant2 -> match a_ty_constant1, a_ty_constant2 with
  | CTyInt, CTyInt ->
    accu
  | CTyString, CTyString ->
    accu
  | _ -> raise IncompatibleStructures

  method ctyint : 'accumulator -> 'accumulator = fun accu -> 
 
      accu

  method ctystring : 'accumulator -> 'accumulator = fun accu -> 
 
      accu

  method clet_ty_abs : 'accumulator -> clet_ty_abs -> clet_ty_abs -> 'accumulator = fun accu -> fun a_clet_ty_abs1 a_clet_ty_abs2 -> match a_clet_ty_abs1, a_clet_ty_abs2 with
  (id2, clet_scheme1, ty_constraint0), (id5, clet_scheme4, ty_constraint3) ->
assert (Id.Atom.equal id2 id5);
      let accu = (self#clet_scheme) accu clet_scheme1 clet_scheme4 in
      let accu = (self#ty_constraint) accu ty_constraint0 ty_constraint3 in
    accu

  method exists_abs : 'accumulator -> exists_abs -> exists_abs -> 'accumulator = fun accu -> fun a_exists_abs1 a_exists_abs2 -> match a_exists_abs1, a_exists_abs2 with
  (ex_vars1, ty_constraint0), (ex_vars3, ty_constraint2) ->
      let accu = List.fold_left2 (self#ex_var) accu ex_vars1 ex_vars3 in
      let accu = (self#ty_constraint) accu ty_constraint0 ty_constraint2 in
    accu

  method newconstant_abs : 'accumulator -> newconstant_abs -> newconstant_abs -> 'accumulator = fun accu -> fun a_newconstant_abs1 a_newconstant_abs2 -> match a_newconstant_abs1, a_newconstant_abs2 with
  (var1, ty_constraint0), (var3, ty_constraint2) ->
assert (Var.Atom.equal var1 var3);
      let accu = (self#ty_constraint) accu ty_constraint0 ty_constraint2 in
    accu

  method clet_scheme_abs : 'accumulator -> clet_scheme_abs -> clet_scheme_abs -> 'accumulator = fun accu -> fun a_clet_scheme_abs1 a_clet_scheme_abs2 -> match a_clet_scheme_abs1, a_clet_scheme_abs2 with
  (ex_vars2, ty_constraint1, ty0), (ex_vars5, ty_constraint4, ty3) ->
      let accu = List.fold_left2 (self#ex_var) accu ex_vars2 ex_vars5 in
      let accu = (self#ty_constraint) accu ty_constraint1 ty_constraint4 in
      let accu = (self#ty) accu ty0 ty3 in
    accu

end
let rec f x = x

and eq_ty_constraint : ty_constraint -> ty_constraint -> bool = fun p1 p2 ->
   try ((new fold2)#ty_constraint true p1 p2 = true) with IncompatibleStructures -> false

and eq_clet_scheme : clet_scheme -> clet_scheme -> bool = fun p1 p2 ->
   try ((new fold2)#clet_scheme true p1 p2 = true) with IncompatibleStructures -> false

and eq_ex_var : ex_var -> ex_var -> bool = fun p1 p2 ->
   try ((new fold2)#ex_var true p1 p2 = true) with IncompatibleStructures -> false

and eq_ty_var : ty_var -> ty_var -> bool = fun p1 p2 ->
   try ((new fold2)#ty_var true p1 p2 = true) with IncompatibleStructures -> false

and eq_ty : ty -> ty -> bool = fun p1 p2 ->
   try ((new fold2)#ty true p1 p2 = true) with IncompatibleStructures -> false

and eq_ty_constant : ty_constant -> ty_constant -> bool = fun p1 p2 ->
   try ((new fold2)#ty_constant true p1 p2 = true) with IncompatibleStructures -> false

and eq_clet_ty_abs : clet_ty_abs -> clet_ty_abs -> bool = fun p1 p2 ->
   try ((new fold2)#clet_ty_abs true p1 p2 = true) with IncompatibleStructures -> false

and eq_exists_abs : exists_abs -> exists_abs -> bool = fun p1 p2 ->
   try ((new fold2)#exists_abs true p1 p2 = true) with IncompatibleStructures -> false

and eq_newconstant_abs : newconstant_abs -> newconstant_abs -> bool = fun p1 p2 ->
   try ((new fold2)#newconstant_abs true p1 p2 = true) with IncompatibleStructures -> false

and eq_clet_scheme_abs : clet_scheme_abs -> clet_scheme_abs -> bool = fun p1 p2 ->
   try ((new fold2)#clet_scheme_abs true p1 p2 = true) with IncompatibleStructures -> false

class map = object(self)

  method ty_constraint : ty_constraint -> ty_constraint = function
  | CEq (ty1, ty0) ->
      self#ceq (ty1, ty0)
  | CInst (id1, ty0) ->
      self#cinst (id1, ty0)
  | CLet (clet_ty_abs0) ->
      self#clet (clet_ty_abs0)
  | CAnd (ty_constraint1, ty_constraint0) ->
      self#cand (ty_constraint1, ty_constraint0)
  | CExists (exists_abs0) ->
      self#cexists (exists_abs0)
  | CNewConstant (newconstant_abs0) ->
      self#cnewconstant (newconstant_abs0)
  | CTrue ->
      self#ctrue
  | CFalse ->
      self#cfalse
  | CDump ->
      self#cdump

  method ceq : ty * ty -> ty_constraint = 
  function (ty1, ty0) -> 
      CEq ((self#ty) ty1, (self#ty) ty0)

  method cinst : id * ty -> ty_constraint = 
  function (id1, ty0) -> 
      CInst (id1, (self#ty) ty0)

  method clet : opaque_clet_ty_abs -> ty_constraint = 
  function (clet_ty_abs0) -> 
      CLet (create_clet_ty_abs (self#clet_ty_abs (open_clet_ty_abs clet_ty_abs0)))

  method cand : ty_constraint * ty_constraint -> ty_constraint = 
  function (ty_constraint1, ty_constraint0) -> 
      CAnd ((self#ty_constraint) ty_constraint1, (self#ty_constraint) ty_constraint0)

  method cexists : opaque_exists_abs -> ty_constraint = 
  function (exists_abs0) -> 
      CExists (create_exists_abs (self#exists_abs (open_exists_abs exists_abs0)))

  method cnewconstant : opaque_newconstant_abs -> ty_constraint = 
  function (newconstant_abs0) -> 
      CNewConstant (create_newconstant_abs (self#newconstant_abs (open_newconstant_abs newconstant_abs0)))

  method ctrue : ty_constraint = CTrue

  method cfalse : ty_constraint = CFalse

  method cdump : ty_constraint = CDump

  method clet_scheme : clet_scheme -> clet_scheme = function
  (clet_scheme_abs0) ->
    (create_clet_scheme_abs (self#clet_scheme_abs (open_clet_scheme_abs clet_scheme_abs0)))

  method ex_var : ex_var -> ex_var = function
  (var0) ->
    (var0)

  method ty_var : ty_var -> ty_var = function
  (var0) ->
    (var0)

  method ty : ty -> ty = function
  | CTyVar (var0) ->
      self#ctyvar (var0)
  | CTyArrow (ty1, ty0) ->
      self#ctyarrow (ty1, ty0)
  | CTyApp (var1, tys0) ->
      self#ctyapp (var1, tys0)
  | CTyConstant (ty_constant0) ->
      self#ctyconstant (ty_constant0)

  method ctyvar : var -> ty = 
  function (var0) -> 
      CTyVar (var0)

  method ctyarrow : ty * ty -> ty = 
  function (ty1, ty0) -> 
      CTyArrow ((self#ty) ty1, (self#ty) ty0)

  method ctyapp : var * ty list -> ty = 
  function (var1, tys0) -> 
      CTyApp (var1, List.map (self#ty) tys0)

  method ctyconstant : ty_constant -> ty = 
  function (ty_constant0) -> 
      CTyConstant ((self#ty_constant) ty_constant0)

  method ty_constant : ty_constant -> ty_constant = function
  | CTyInt ->
      self#ctyint
  | CTyString ->
      self#ctystring

  method ctyint : ty_constant = CTyInt

  method ctystring : ty_constant = CTyString

  method clet_ty_abs : clet_ty_abs -> clet_ty_abs = function
  (id2, clet_scheme1, ty_constraint0) ->
    (id2, (self#clet_scheme) clet_scheme1, (self#ty_constraint) ty_constraint0)

  method exists_abs : exists_abs -> exists_abs = function
  (ex_vars1, ty_constraint0) ->
    (List.map (self#ex_var) ex_vars1, (self#ty_constraint) ty_constraint0)

  method newconstant_abs : newconstant_abs -> newconstant_abs = function
  (var1, ty_constraint0) ->
    (var1, (self#ty_constraint) ty_constraint0)

  method clet_scheme_abs : clet_scheme_abs -> clet_scheme_abs = function
  (ex_vars2, ty_constraint1, ty0) ->
    (List.map (self#ex_var) ex_vars2, (self#ty_constraint) ty_constraint1, (self#ty) ty0)

end

class [ 'accumulator ] fold = object(self)

  method ty_constraint : 'accumulator -> ty_constraint -> 'accumulator = fun accu -> function
  | CEq (ty1, ty0) ->
      self#ceq accu (ty1, ty0)
  | CInst (id1, ty0) ->
      self#cinst accu (id1, ty0)
  | CLet (clet_ty_abs0) ->
      self#clet accu (clet_ty_abs0)
  | CAnd (ty_constraint1, ty_constraint0) ->
      self#cand accu (ty_constraint1, ty_constraint0)
  | CExists (exists_abs0) ->
      self#cexists accu (exists_abs0)
  | CNewConstant (newconstant_abs0) ->
      self#cnewconstant accu (newconstant_abs0)
  | CTrue ->
      self#ctrue accu
  | CFalse ->
      self#cfalse accu
  | CDump ->
      self#cdump accu

  method ceq : 'accumulator -> ty * ty -> 'accumulator = fun accu -> 
  function (ty1, ty0) -> 
      let accu = (self#ty) accu ty1 in
      let accu = (self#ty) accu ty0 in
     accu

  method cinst : 'accumulator -> id * ty -> 'accumulator = fun accu -> 
  function (id1, ty0) -> 
      let accu = (self#ty) accu ty0 in
     accu

  method clet : 'accumulator -> opaque_clet_ty_abs -> 'accumulator = fun accu -> 
  function (clet_ty_abs0) -> 
      let accu = self#clet_ty_abs accu (open_clet_ty_abs clet_ty_abs0) in
     accu

  method cand : 'accumulator -> ty_constraint * ty_constraint -> 'accumulator = fun accu -> 
  function (ty_constraint1, ty_constraint0) -> 
      let accu = (self#ty_constraint) accu ty_constraint1 in
      let accu = (self#ty_constraint) accu ty_constraint0 in
     accu

  method cexists : 'accumulator -> opaque_exists_abs -> 'accumulator = fun accu -> 
  function (exists_abs0) -> 
      let accu = self#exists_abs accu (open_exists_abs exists_abs0) in
     accu

  method cnewconstant : 'accumulator -> opaque_newconstant_abs -> 'accumulator = fun accu -> 
  function (newconstant_abs0) -> 
      let accu = self#newconstant_abs accu (open_newconstant_abs newconstant_abs0) in
     accu

  method ctrue : 'accumulator -> 'accumulator = fun accu ->      accu

  method cfalse : 'accumulator -> 'accumulator = fun accu ->      accu

  method cdump : 'accumulator -> 'accumulator = fun accu ->      accu

  method clet_scheme : 'accumulator -> clet_scheme -> 'accumulator = fun accu -> function
  (clet_scheme_abs0) ->
      let accu = self#clet_scheme_abs accu (open_clet_scheme_abs clet_scheme_abs0) in
      accu

  method ex_var : 'accumulator -> ex_var -> 'accumulator = fun accu -> function
  (var0) ->
      accu

  method ty_var : 'accumulator -> ty_var -> 'accumulator = fun accu -> function
  (var0) ->
      accu

  method ty : 'accumulator -> ty -> 'accumulator = fun accu -> function
  | CTyVar (var0) ->
      self#ctyvar accu (var0)
  | CTyArrow (ty1, ty0) ->
      self#ctyarrow accu (ty1, ty0)
  | CTyApp (var1, tys0) ->
      self#ctyapp accu (var1, tys0)
  | CTyConstant (ty_constant0) ->
      self#ctyconstant accu (ty_constant0)

  method ctyvar : 'accumulator -> var -> 'accumulator = fun accu -> 
  function (var0) -> 
     accu

  method ctyarrow : 'accumulator -> ty * ty -> 'accumulator = fun accu -> 
  function (ty1, ty0) -> 
      let accu = (self#ty) accu ty1 in
      let accu = (self#ty) accu ty0 in
     accu

  method ctyapp : 'accumulator -> var * ty list -> 'accumulator = fun accu -> 
  function (var1, tys0) -> 
      let accu = List.fold_left (self#ty) accu tys0 in
     accu

  method ctyconstant : 'accumulator -> ty_constant -> 'accumulator = fun accu -> 
  function (ty_constant0) -> 
      let accu = (self#ty_constant) accu ty_constant0 in
     accu

  method ty_constant : 'accumulator -> ty_constant -> 'accumulator = fun accu -> function
  | CTyInt ->
      self#ctyint accu
  | CTyString ->
      self#ctystring accu

  method ctyint : 'accumulator -> 'accumulator = fun accu ->      accu

  method ctystring : 'accumulator -> 'accumulator = fun accu ->      accu

  method clet_ty_abs : 'accumulator -> clet_ty_abs -> 'accumulator = fun accu -> function
  (id2, clet_scheme1, ty_constraint0) ->
      let accu = (self#clet_scheme) accu clet_scheme1 in
      let accu = (self#ty_constraint) accu ty_constraint0 in
      accu

  method exists_abs : 'accumulator -> exists_abs -> 'accumulator = fun accu -> function
  (ex_vars1, ty_constraint0) ->
      let accu = List.fold_left (self#ex_var) accu ex_vars1 in
      let accu = (self#ty_constraint) accu ty_constraint0 in
      accu

  method newconstant_abs : 'accumulator -> newconstant_abs -> 'accumulator = fun accu -> function
  (var1, ty_constraint0) ->
      let accu = (self#ty_constraint) accu ty_constraint0 in
      accu

  method clet_scheme_abs : 'accumulator -> clet_scheme_abs -> 'accumulator = fun accu -> function
  (ex_vars2, ty_constraint1, ty0) ->
      let accu = List.fold_left (self#ex_var) accu ex_vars2 in
      let accu = (self#ty_constraint) accu ty_constraint1 in
      let accu = (self#ty) accu ty0 in
      accu

end
