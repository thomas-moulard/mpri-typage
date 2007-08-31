open Syntax

let var_of_binder = function
  | AnnotatedBind (x, _) -> x
  | Bind x -> x

exception Matching_Failed

class subst_gen =
object (self)
  inherit map

  val mutable phi_ref = None

  method set_phi phi = phi_ref <- Some phi

  method phi = match phi_ref with None -> assert false | Some phi -> phi

  method evar (pos, x) =
    try
      Var.AtomMap.lookup x self#phi
    with Not_found ->
      (*print_string "Not found < ";
      print_string (Syntax.Var.Atom.basename x);
      print_endline " >";*)
      EVar (pos, x)

end

let subst_id =
  Var.AtomMap.empty

let ( |-> ) x t =
  Var.AtomMap.add x t subst_id

let union f g =
  Var.AtomMap.union f g

let subst_gen = new subst_gen


let subst_term phi term =
  try
    subst_gen#set_phi phi;
    subst_gen#term term
  with Not_found ->
    failwith "Error during application of a substitution to a term"

let subst_program phi p =
  try
    subst_gen#set_phi phi;
    subst_gen#program p
  with Not_found ->
    failwith "Error during application of a substitution to a program"

let is_value = function
  | _ -> (* FIXME *) assert false

let rec destruct_dataconstructor_app =  function
  | EApp (_, _, t, t') ->
      let (constructor, args) = destruct_dataconstructor_app t in
        (constructor, args @ [t'])
  | EVar (_, v) -> (v, [])
  | _ -> assert false

let dataconstructor_counter = ref 0

let dataconstructor_value () =
  incr dataconstructor_counter;
  EConstant (Positions.dummy, Int (!dataconstructor_counter))

let rec fix x t1 =
  x (fix x) t1

let rec eval = function
  | EVar (annotation, var) ->
      print_string "Eval -> EVar [ ";
        print_string (Syntax.Var.Atom.basename (var));
        print_endline " ]";
      subst_gen#evar (annotation, var) (* A priori ca va chercher la variable comme il faut *)

  | EConstant (annotation, primitive) as x ->
      (*let () = print_endline "Eval -> EConstant" in*)
      x

  | EApp (annotation, level, term1, term2) ->
      let () = print_endline "Eval -> EApp" in 
      let tfun = eval term1 in
      (
        match tfun with
          | ELambda(annotation, lambda_abs) ->
              let (bind, term) = open_lambda_abs lambda_abs in
              let var = var_of_binder bind in
              print_string "EApp -> ELambda [";
              print_string (Syntax.Var.Atom.basename var);
              print_endline "]";
              let new_bind = (var_of_binder bind) |-> (eval term2) in
              let substterm = subst_term new_bind term in
                eval substterm
          | EVar(a, var) as x -> 
              print_string "EApp -> EVar [";
              print_string (Syntax.Var.Atom.basename var);
              print_endline "]";
                  x
          | _ ->
                  print_endline "EApp -> _";
                  EApp(annotation, level, tfun, eval term2)
      )

  | ELambda (annotation, lambda_abs) as x ->
      let () = print_string "Eval -> ELambda [ " in
        let (binder, term) = (open_lambda_abs lambda_abs) in
        let var = var_of_binder binder in
        let () = print_string (Syntax.Var.Atom.basename var) in
        let () = print_endline " ]" in
      
      x (* Ne pas évaluer une fonction ... *)

  | ELet (annotation, let_abs)  -> (*let () = print_string "Eval -> ELet [ " in*)
      let (var, term1, term2) = (open_let_abs let_abs) in
        (*let () = print_string (Syntax.Var.Atom.basename var) in
          let () = print_endline " ]" in*)
      let () = ignore(subst_term (var |-> (eval term1)) term1) in
        eval term2

  (* ... *)
  | ELetRec (annotation, rec_abs) ->
      print_endline "Eval -> ELetRec";
      let (v, t, t') = open_rec_abs rec_abs in
      let () = ignore(subst_term (v |-> (t)) t) in
        eval (ELet (annotation, create_let_abs(v, t, t')))

  | EMatch (annotation, term, clause_list) ->
      print_endline "Eval -> EMatch";
      eval_match (eval term) clause_list
  | EAnnot (annotation, term, ty) as x -> print_endline "Eval -> EAnnot"; x

and eval_match v = function
  | [] -> failwith "Pattern matching has failed."
  | clause::l -> let (_, p ,t) = open_clause_abs clause in
      try
          eval (subst_term (match_pattern p v) t)
      with
          Matching_Failed -> eval_match v l

and match_pattern p v =
  match p with
    | PVar(_, var) -> union subst_gen#phi (var |-> v)
    | PDataCon(_, c, p) ->
        let (constructor, args) = destruct_dataconstructor_app v in
          if constructor = c then
            List.fold_left union subst_id (List.map2 match_pattern p args)
          else
            raise Matching_Failed

let rec eval_datatype_def = function
  | [] -> []
  | DataType(v, tl)::dt -> (v, EVar(Positions.dummy, v))::eval_datatype_def dt

let () = subst_gen#set_phi subst_id
let rec eval_toplevel_definition = function
  | ValDefinition (var, term) ->
      (*print_string "Eval_toplevel_definition -> ValDefinition [ ";
        print_string (Syntax.Var.Atom.basename var);
        print_string " ]";
        print_newline ();
      *)
      let evalterm = eval term in
      let x = var |-> evalterm in
      let y = union subst_gen#phi x in
      let () = subst_gen#set_phi y in
        [(var, evalterm)]

  | TypeDefinition (var, param_list, type_def) ->
      (*print_string (Syntax.Var.Atom.basename var);
        print_param_list param_list;
        print_newline ();
        print_typedef typedef;*)
      let AlgebraicDataType(dt) = type_def in
        eval_datatype_def dt


  | RecDefinitions (var, term) ->
      print_endline "RecDef";
      [(var, eval term)]


and eval_program = function 
  | EmptyProgram -> []
  | NewDefinition (i, otlabs) ->
      let (tldef, program) = open_toplevel_abs otlabs in
      let evaltoplevel = eval_toplevel_definition tldef in
        evaltoplevel @ eval_program program
