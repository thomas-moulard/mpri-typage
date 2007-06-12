type id = Constraint.Id.Atom.t

type tycon = Constraint.Var.Atom.t

type ty_constraint =
  | CEq of ty * ty
  | CInst of id * ty
  | CLet of id * clet_scheme * ty_constraint
  | CAnd of ty_constraint * ty_constraint
  | CExists of ex_var list * ty_constraint
  | CTrue
  | CFalse
  | CDump

and clet_scheme =
  ex_var list * ty_constraint * var

and ex_var = var

and ty_var = var

and ty = 
  | CTyVar of var
  | CTyArrow of ty * ty
  | CTyApp of tycon * ty list
  | CTyConstant of ty_constant

and ty_constant = 
  | CTyInt
  | CTyString

and descr =
    {
      mutable rank      : int;
      mutable mark	: Mark.t;
      mutable structure : ty option;
      mutable name	: string;
    }

and var = descr UnionFind.point

let descr =
  UnionFind.find 

let name x = 
  (descr x).name

let set_name x n = 
  (descr x).name <- n

let structure x = 
  (descr x).structure

let set_structure x s =
  (descr x).structure <- s

let set_mark mark x =
  (descr x).mark <- mark

let mark x =
  (descr x).mark

let set_rank k x =
  (descr x).rank <- k

let generalized_rank = -1

let undefined_rank = -2

let change_rank rank x =
  set_rank rank x;
  x

let rank x =
  (descr x).rank

let fresh_var ?name () =
  UnionFind.fresh 
    {
      rank = undefined_rank;
      mark = Mark.none;
      structure = None;
      name = match name with None -> Name.fresh () | Some x -> x
    }

let name_of_atom a =
  Constraint.Var.Atom.basename a 
  ^ string_of_int (Constraint.Var.Atom.identity a)

let introduce_atom (vs, env) x =
  let v = fresh_var ~name:(name_of_atom x) () in
    (v :: vs, Constraint.Var.AtomMap.add x v env)

let empty_env = 
  Constraint.Var.AtomMap.empty

let from_atom env x = 
  try
    Constraint.Var.AtomMap.lookup x env
  with Not_found -> 
    failwith ("Fatal error: Unbound" ^ Constraint.Var.Atom.basename x)

let rec from_constraint env = function
  | Constraint.CEq (t1, t2) ->
      CEq (from_ty env t1, from_ty env t2), env

  | Constraint.CInst (id, t) ->
      CInst (id, from_ty env t), env

  | Constraint.CNewConstant newconstant_abs -> 
      let (x, t) = Constraint.open_newconstant_abs newconstant_abs in
	from_constraint env t 

  | Constraint.CLet clet_abs -> 
      let (id, s, c2) = Constraint.open_clet_ty_abs clet_abs in
      let (vs, c1, ty) = Constraint.open_clet_scheme_abs s in
      let vs', env = List.fold_left introduce_atom ([], env) vs in
      let v' = fresh_var () in
      let ty' = from_ty env ty in
      let c1', env = from_constraint env c1 in
      let c2', env = from_constraint env c2 in
	set_structure v' (Some ty');
	CLet (id, (v' :: vs', c1', v'), c2'), env
  
  | Constraint.CAnd (c1, c2) ->
      let c1', env = from_constraint env c1 in
      let c2', env = from_constraint env c2 in
	CAnd (c1', c2'), env

  | Constraint.CExists exists_abs ->
      let (vs, c) = Constraint.open_exists_abs exists_abs in
      let vs', env = List.fold_left introduce_atom ([], env) vs in
      let c', env = from_constraint env c in
	CExists (vs', c'), env

  | Constraint.CTrue ->
      CTrue, env

  | Constraint.CFalse ->
      CFalse, env

  | Constraint.CDump ->
      CDump, env

and from_ty env = function
  | Constraint.CTyVar x ->
      CTyVar (from_atom env x)

  | Constraint.CTyArrow (ty1, ty2) ->
      CTyArrow (from_ty env ty1, from_ty env ty2)

  | Constraint.CTyApp (x, ts) ->
      CTyApp (x, List.map (from_ty env) ts)

  | Constraint.CTyConstant c ->
      CTyConstant (from_ty_constant c)

and from_ty_constant = function
  | Constraint.CTyInt -> CTyInt
  | Constraint.CTyString -> CTyString

let from c = 
  fst (from_constraint empty_env c)
	
let equalize = UnionFind.union

let equivalent = UnionFind.equivalent
