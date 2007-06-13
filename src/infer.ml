open Syntax
open Constraint

type env = 
    {
      constraint_ids : Constraint.Id.Atom.t Syntax.Var.AtomMap.t;
      tyvars         : (Constraint.Var.Atom.t * int) Syntax.Var.AtomMap.t
    }

let empty_env = 
  {
    constraint_ids = Syntax.Var.AtomMap.empty;
    tyvars	   = Syntax.Var.AtomMap.empty;
  }

let lookup_constraint_id env x =
  try 
    Syntax.Var.AtomMap.lookup x env.constraint_ids 
  with Not_found ->
    failwith ("Unbound " ^ Syntax.Var.Atom.basename x)

let bind_constraint_id env x =
  let x' = Constraint.Id.Atom.freshb (Syntax.Var.Atom.basename x) in
    ({ env with 
	 constraint_ids = Syntax.Var.AtomMap.add x x' env.constraint_ids },
     x')

let lookup_tyvar env x =
  try
    fst (Syntax.Var.AtomMap.lookup x env.tyvars)
  with Not_found -> failwith ("Unbound " ^ Syntax.Var.Atom.basename x)

let lookup_tyvar_arity env x =
  try
    snd (Syntax.Var.AtomMap.lookup x env.tyvars)
  with Not_found -> failwith ("Unbound " ^ Syntax.Var.Atom.basename x)

let bind_tyvar env x arity =
  let x' = Constraint.Var.Atom.freshb (Syntax.Var.Atom.basename x) in
    ({ env with tyvars = Syntax.Var.AtomMap.add x (x', arity) env.tyvars }, x')

let exists f =
  let v = Constraint.Var.Atom.freshb "f" in
    CExists (create_exists_abs ([v], f v))

let arrow t t' = 
  CTyArrow (t, t')

let arrows ts t = 
  List.fold_left (fun a t -> arrow t a) t (List.rev ts)

let conj cs = 
  List.fold_left (fun c c' -> CAnd (c, c')) CTrue cs

let clet x vs c1 ty c2 =
  CLet (create_clet_ty_abs 
	  (x, create_clet_scheme_abs (vs, c1, ty), c2)) 

let as_types vs = 
  List.map (fun v -> CTyVar v) vs

let rec term env ty = 
(* FIXME *) assert false

and clause env tv ty clause_abs =
(* FIXME *) assert false

and pattern env tv = 
(* FIXME *) assert false
      
and ty_of_constant = function
  | _ -> (* FIXME *) assert false

and ty_to_cty env = 
(* FIXME *) assert false

and convert_ty_constant = function
  | TyInt -> CTyInt
  | TyString -> CTyString

and bind env b f =
(* FIXME *) assert false  
    
and bind_id env x f =
(* FIXME *) assert false


and scheme_of env t =
(* FIXME *) assert false  
  
and program end_constraint env = function
  | EmptyProgram ->
      end_constraint 

  | NewDefinition (_, toplevel_abs) ->
      let d, prog = open_toplevel_abs toplevel_abs in
      let context, env = toplevel_definition env d in
	context (program end_constraint env prog)

and toplevel_definition env = function

  | TypeDefinition (tvar, ps, tdef) ->
(* FIXME *) assert false

  | RecDefinitions (x, t) ->
(* FIXME *) assert false

  | ValDefinition (x, t) ->
(* FIXME *) assert false

and bind_type_definition tvar params env = 
(* FIXME *) assert false

and bind_algebraic_dataconstructor tvar params env = 
(* FIXME *) assert false

let dump_ty_constraint_of p = 
  program CDump empty_env p

let ty_constraint_of p = 
  program CTrue empty_env p
  
      
