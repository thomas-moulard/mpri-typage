open Syntax

let var_of_binder = function
  | AnnotatedBind (x, _) -> x
  | Bind x -> x

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
  | _ -> (* FIXME *) assert false

let dataconstructor_counter = ref 0

let dataconstructor_value () =
  incr dataconstructor_counter;
  EConstant (Positions.dummy, Int (!dataconstructor_counter))

let fix x t1 =
(* FIXME *) assert false

let term2string = function
  | EVar (_, var) -> Syntax.Var.Atom.basename var
  | ELambda (_, lambda_abs) -> let (binder, _) = open_lambda_abs lambda_abs in
                               Syntax.Var.Atom.basename (var_of_binder binder)
  | ELet (_, let_abs) -> let (var, _, _) = open_let_abs let_abs in
                         Syntax.Var.Atom.basename var
  | EConstant (_, primitive) -> (match primitive with
                                  | Int(i) -> "<" ^ (string_of_int i) ^ ">"
                                  | String(s) -> "\"" ^ s ^ "\""
                                )
  | x -> " xxx "



let dump_map map =
  let buffer = Buffer.create 100000 in
  let pk buf key = Buffer.add_string buf (Var.Atom.basename key) in
  let pa buf data = Buffer.add_string buf (term2string data) in
  let () = (Var.AtomMap.print pk pa) buffer map in
  (*Buffer.output_buffer stdout buffer*)
  let s = Buffer.contents buffer in
  print_endline "+----------------------+";
  print_endline s;
  print_endline "+----------------------+"

let rec eval = function
  | EVar (annotation, var) -> (*print_string "Eval -> EVar [ ";
                              print_string (Syntax.Var.Atom.basename (var));
                              print_endline " ]";*)
                              subst_gen#evar (annotation, var) (* A priori ca va chercher la variable comme il faut *)

  | EConstant (annotation, primitive) as x -> (*let () = print_endline "Eval -> EConstant" in*)
                                              x

  | EApp (annotation, level, term1, term2) as x -> (*let () = print_endline "Eval -> EApp" in *)
                                                   let et2 = eval term2 in
                                                   let et1 = eval term1 in (* utile ou pas ? *)
                                                   et2

  | ELambda (annotation, lambda_abs) as x -> (*let () = print_string "Eval -> ELambda [ " in
                                             let (binder, term) = (open_lambda_abs lambda_abs) in
                                             let var = var_of_binder binder in
                                             let () = print_string (Syntax.Var.Atom.basename var) in
                                             let () = print_endline " ]" in
                                             *)
                                             x (* Ne pas évaluer une fonction ... *)

  | ELet (annotation, let_abs) as x -> (*let () = print_string "Eval -> ELet [ " in*)
                                       let (var, term1, term2) = (open_let_abs let_abs) in
                                       (*let () = print_string (Syntax.Var.Atom.basename var) in
                                       let () = print_endline " ]" in*)
                                       let () = ignore(subst_term (var |-> (eval term1)) term1) in
                                       eval term2

  (* ... *)
  | ELetRec (annotation, rec_abs) as x -> print_endline "Eval -> ELetRec"; x
  | EMatch (annotation, term, clause_list) as x -> print_endline "Eval -> EMatch"; x
  | EAnnot (annotation, term, ty) as x -> print_endline "Eval -> EAnnot"; x

and eval_match v =
(* FIXME *) assert false

and match_pattern p v =
(* FIXME *) assert false


let rec print_list f l = match l with
  | [] -> ""
  | e::l -> f e ^ "\n" ^ print_list f l

let rec print_param_list l = print_endline (print_list Syntax.Var.Atom.basename l)

let rec print_ty ty = match ty with
  | TyArrow(annotation, ty1, ty2) -> print_ty ty1 ^ " -> " ^ print_ty ty2
  | TyApp(annotation, var, tyl) -> (Syntax.Var.Atom.basename var) ^ "." ^ (print_list print_ty tyl)
  | TyVar(annotation, var) -> "var " ^ (Syntax.Var.Atom.basename var)
  | TyConstant(annotation, ty_constant) ->
      match ty_constant with
        | TyString -> "string"
        | TyInt -> "int"

let print_datatype x = match x with
  | DataType(v, tyl) -> "DataType" ^ "\n" ^ (Syntax.Var.Atom.basename v) ^ print_list print_ty tyl
  | _ -> "xxx"

let print_typedef td = match td with
  | AlgebraicDataType(x) -> print_endline "AlgebraicDataType"; print_endline (print_list print_datatype x)
  | _ -> print_string "xxx"

let rec eval_toplevel_definition = function
  | TypeDefinition (var, param_list, typedef) as x -> (*print_string (Syntax.Var.Atom.basename var);
                                                        print_param_list param_list;
                                                        print_newline ();
                                                        print_typedef typedef;*)
                                                      x
  | ValDefinition (var, term) -> (*print_string "Eval_toplevel_definition -> ValDefinition [ ";
                                 print_string (Syntax.Var.Atom.basename var);
                                 print_string " ]";
                                 print_newline ();*)
                                 ValDefinition(var, eval term)
  | RecDefinitions (var, term) -> RecDefinitions(var, eval term)

and eval_program = function
  | EmptyProgram -> (*print_endline "Eval_program -> EmptyProgram";*) []
  | NewDefinition (i, otlabs) as prog ->
      (*let () = print_endline "Eval_program -> NewDefinition" in *)
      let (tldef, program) = open_toplevel_abs otlabs in
      match (eval_toplevel_definition tldef) with
        | TypeDefinition (var, param_list, typedef) -> eval_program program
        | ValDefinition (var, term) ->
            let () = ignore (subst_program subst_id prog) in
              (var, term) :: (eval_program program)
        | RecDefinitions (var, term) ->
            let () = ignore (subst_program subst_id prog) in
              (var, term) :: (eval_program program)

