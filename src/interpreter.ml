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

let is_value = 
(* FIXME *) assert false

let rec destruct_dataconstructor_app = 
(* FIXME *) assert false

let dataconstructor_counter = ref 0

let dataconstructor_value () =
  incr dataconstructor_counter;
  EConstant (Positions.dummy, Int (!dataconstructor_counter))

let fix x t1 = 
(* FIXME *) assert false

let rec eval = 
(* FIXME *) assert false

and eval_match v = 
(* FIXME *) assert false

and match_pattern p v = 
(* FIXME *) assert false

let rec eval_toplevel_definition = 
(* FIXME *) assert false

and eval_program = 
(* FIXME *) assert false

      
