(** Ce module implémente le comportement du programme principal. *)

(** Ici, on génère un analyseur syntaxique pour le langage de programmation
    à partir d'un lexeur et d'un parseur. *)
module SyntacticAnalyser = 
  SyntacticAnalysis.Make (struct 
			    type output = Syntax.Raw.program
			    include Parser
			    let lexer = Lexer.main
			    let parser = program
			  end)

(** Options *)

let filename = ref ""
let pretty_print = ref false
let pretty_print_constraint = ref false
let eval_program = ref false

let options = Arg.align
  [
    "-p", Arg.Set pretty_print, " print the input program.";
    "-e", Arg.Set eval_program, " evaluate the input program.";
    "-c", Arg.Set pretty_print_constraint, " print the typing constraint."
  ]

let set_filename s = 
  filename := s

let usage = ""

let () = Arg.parse options set_filename usage

(** Analyse syntaxique du programme. *)
let raw_program = SyntacticAnalyser.parse !filename

(** On l'affiche si demandé. *)
let () = 
  if !pretty_print then
    CommonPrinter.in_stdout Printer.program raw_program

(** Passage en représentation interne de AlphaCaml. Voir la documentation
    de AlphaCaml. *)
let program = 
  Syntax.import_program Syntax.Identifier.Map.empty raw_program

(** On l'évalue si demandé. *)
let () = 
  if !eval_program then
    try
      let result = Interpreter.eval_program program in
	ignore 
	  (List.fold_left (fun ids (x, v) -> 
			     let ids = Syntax.Var.AtomIdMap.add x ids in
			     let v_raw = Syntax.export_term ids v in
			       Printf.printf "%s = " 
				 (Syntax.Var.AtomIdMap.lookup x ids);
			       CommonPrinter.in_stdout Printer.term v_raw;
			       Printf.printf "\n";
			       ids)
	     Syntax.Var.AtomIdMap.empty result)
    with _ -> Printf.printf "Evaluation failed\n"
      
(*
(** Génère la contrainte de typage du programme. *)
let ty_constraint = 
  Infer.dump_ty_constraint_of program

(** On l'affiche si demandé. *)
let () = 
  if !pretty_print_constraint then
    let raw_ty_constraint = ConstraintPrinter.as_raw ty_constraint in
	CommonPrinter.in_stdout 
	  ConstraintPrinter.ty_constraint raw_ty_constraint

(** On transforme cette contrainte en une structure de données plus
    adapté à la résolution de contrainte. *)
let solving_ty_constraint = 
  SolvingConstraint.from ty_constraint

(** Résolution de la contrainte. *)
let () = 
  try 
    Solver.solve_constraint solving_ty_constraint
  with 
    | Solver.Answer s ->
	Printf.printf "%s\n" s
    | Failure s | Unifier.Error s ->
	Printf.printf "Type error: %s\n" s;
	exit 1
    | Unifier.Cycle ->
	Printf.printf "There is a cycle in the type of this program.\n";
	exit 1
*)
