open Syntax.Raw
open CommonPrinter

let string_of_id x = 
  if Lexer.is_binop (Lexing.from_string x) then
    "( "^x^" )" 
  else 
    x

let rec print_def env kwd x binder xs printer t = 
  let env = string env (Printf.sprintf "%s %s " kwd (string_of_id x)) in
  let env = ended space binder env xs in
  let env = string env "=" in
  let env = new_block env in
  let env = printer env t in
    end_block env

let rec extract_fun = function

  | ELambda (_, (b, t)) ->
      let bs, t = extract_fun t in
	b :: bs, t

  | x -> [], x

let rec program env = function
  | EmptyProgram -> 
      env

  | NewDefinition (_, (tdef, prog)) ->
      let env = toplevel_definition env tdef in
	program env prog

and toplevel_definition env = function
  | TypeDefinition (x, xs, type_def) ->
      ignore (newline (print_def env "type" x string xs type_definition type_def));
      string env "\nend\n"

  | ValDefinition (x, t) ->
      let xs, t = extract_fun t in
	newline (print_def env "val" x binder xs term t)

  | RecDefinitions (x, t) ->
      let xs, t = extract_fun t in
	newline (print_def env "val rec" x binder xs term t)
	
and term env = function
  | EVar (_, x) ->
      string env x
	
  | EApp (_, Level.PrefixApp, t1, t2) ->
      may_paren (fun env ->
		   let env = term env t1 in
		   let env = new_level env Level.PrefixApp in
		   let env = string env " " in
		     end_level (term env t2)) env

  | EApp (_, level, t1, t2) ->
      may_paren (fun env ->
		   let env = new_level env level in
		   let env = term env t2 in
		   let env = string env " " in
		     end_level (term env t1)) env

  | ELambda (_, (bind, t)) ->
      may_paren (fun env ->
		   let env = string env "fun " in
		   let env = binder env bind in
		   let env = string env "." in
		     term env t) env

  | ELet (_, (x, t, t2)) ->
      let xs, t1 = extract_fun t in
      let env = print_def env "let" x binder xs term t1 in
      let env = string env "in\n" in
	term env t2

  | ELetRec (_, (x, t, t2)) ->
      let xs, t1 = extract_fun t in
      let env = print_def env "let rec" x binder xs term t1 in
      let env = string env "in\n" in
	term env t2

  | EConstant (_, c) ->
      primitive env c

  | EMatch (_, t, cs) ->
      let env = string env "match " in
      let env = term env t in
      let env = string env " with " in
      let env = new_block env in
      let env = List.fold_left clause env cs in
	string (end_block env) "\nend\n"

  | EAnnot (_, t, annot) ->
      let env = string env "(" in
      let env = term env t in
      let env = string env " : " in
      let env = ty env annot in
	string env ")"

and binder env = function
  | AnnotatedBind (x, annot) ->
      paren (fun env -> 
	       let env = string env x in
	       let env = string env " : " in
		 ty env annot) env

  | Bind x ->
      string env x

and clause env (_, p, t) =
  let env = new_block env in
  let env = string env "| " in
  let env = pattern env p in
  let env = string env " -> " in
    end_block (term env t)

and pattern env = function
  | PVar (_, x) ->
      string env x

  | PDataCon (_, k, ps) ->
      let env = string env k in
	List.fold_left (space pattern) env ps

and primitive env = function
  | Int x ->
      string env (string_of_int x)

  | String s ->
      string env ("\""^s^"\"")

and ty env = function

  | TyArrow (_, t1, t2) ->
      may_paren (fun env ->
		   let env = new_level env Level.arrow in 
		   let env = end_level (ty env t1) in
		   let env = string env " -> " in
		     ty env t2) env

  | TyApp (_, x, ts) ->
      let env = string env (x ^" ") in
      let env = new_level env Level.PrefixApp in
	end_level (List.fold_left (space ty) env ts)

  | TyVar (_, x) ->
      string env x

  | TyConstant (_, tc) ->
      ty_constant env tc

and ty_constant env = function
  | TyInt ->
      string env "int"

  | TyString ->
      string env "string"

and type_definition env = function
  | AlgebraicDataType ds ->
      List.fold_left datatype_def env ds

and datatype_def env = function
  | DataType (x, atys) ->
      if atys = [] then
	string env (x ^"\n")
      else 
	let env = string env ("| " ^ x ^ " of ") in
	  string (arg_tys env atys) "\n"
     
and star f env = 
  f (string env " * ") 

and arg_tys env =
  seps star ty env 

