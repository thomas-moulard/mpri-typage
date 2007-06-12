open SolvingConstraint
open SolverEnv
open Unifier

exception Answer of string

let generalize env =
(* FIXME *) assert false

let instanciate env x =
(* FIXME *) assert false

let introduce_term env t = 
  match t with
    | CTyVar x ->
	env, x
    | t -> 
	let x = fresh_var () in
	  set_structure x (Some t);
	  introduce false env [ x ], x

let unify_terms env t1 t2 = 
  let env, x1 = introduce_term env t1 in
  let env, x2 = introduce_term env t2 in
    merge_var x1 x2;
    env

let rec solve env c = 
(* FIXME *) assert false

let solve_constraint c = 
  ignore (solve empty_env c)

  
  
