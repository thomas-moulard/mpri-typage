open Constraint.Raw
open CommonPrinter

let binop env p op t1 t2 = 
  let env = p env t1 in
  let env = string env op in
    p env t2

let rec ty_constraint env = function

  | CEq (t1, t2) ->
      binop env ty " = " t1 t2

  | CInst (x, t) ->
      let env = string env (x ^ " < ") in
	ty env t

  | CLet (x, s, c') ->
      let env = string env ("let " ^ x ^ " : ") in
      let env = scheme env s in
      let env = string env " in " in
      let env = new_block env in
	end_block (ty_constraint env c')

  | CAnd (c1, c2) ->
      binop env ty_constraint " /\\ " c1 c2

  | CExists (xs, c) -> 
      let env = string env "exists " in
      let env = seps space string env xs in
      let env = string env "." in
      let env = new_block env in
	end_block (ty_constraint env c)

  | CNewConstant (x, c) -> 
      let env = string env ("new " ^ x) in
      let env = string env "." in
      let env = new_block env in
	end_block (ty_constraint env c)

  | CTrue ->
      string env "true"

  | CFalse ->
      string env "false"

  | CDump ->
      string env "dump"

and scheme env (xs, c, t) =
  if xs = [] && c = CTrue then
    ty env t
  else 
    let env = string env "forall " in
    let env = seps space string env xs in
    let env = string env "[" in
    let env = ty_constraint env c in
    let env = string env "] " in
      ty env t

and ty env = function

  | CTyVar x ->
      string env x

  | CTyArrow (t1, t2) ->
      may_paren (fun env ->
		   let env = new_level env Level.arrow in 
		   let env = end_level (ty env t1) in
		   let env = string env " -> " in
		     ty env t2) env

  | CTyApp (x, ts) ->
      let env = string env (x ^" ") in
      let env = new_level env Level.PrefixApp in
	end_level (List.fold_left (space ty) env ts)

  | CTyConstant tc ->
      ty_constant env tc

and ty_constant env = function

  | CTyInt ->
      string env "int"

  | CTyString ->
      string env "string"

let as_raw = 
  Constraint.export_ty_constraint (Constraint.Id.AtomIdMap.empty, 
				   Constraint.Var.AtomIdMap.empty) 
