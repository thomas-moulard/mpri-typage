type env =
    {
      out		: out_channel;
      indent		: int;
      level		: Level.t list;
      newline		: bool
    }

let delta_indent = 2

let new_block env =
  { env with 
      indent = env.indent + delta_indent;
      newline = true
  }

let end_block env =
  { env with 
      indent = env.indent - delta_indent;
      newline = true
  }

let new_level env level =
  { env with level = level :: env.level }

let end_level env =
  { env with level = List.tl env.level }

let at_toplevel env =
  env.level = []

let new_line env = 
  { env with newline = true }

let indent_space env = 
  String.make env.indent ' '

let print_string env s = 
  let s = indent_space env ^ s in
    output_string env.out s;
    env

let string env s =
  if env.newline then (
    output_string env.out "\n";
    let env = print_string env s in
      { env with newline = false }
  ) else (
    output_string env.out s;
    env
  )

let space f env = 
  let env = string env " " in
    f env

let seps sep f env = function
  | [] -> env
  | [ x ] -> f env x
  | x :: xs -> 
      let env = f env x in
	List.fold_left (sep f) env xs

let ended sep f env ys = 
  List.fold_left (fun env y -> 
	    let env = f env y in
	      sep (fun x -> x) env)
    env ys

let paren f env = 
  let env = string env "(" in
  let env = f env in
    string env ")"

let may_paren f env =
  if at_toplevel env then
    f env
  else 
    paren f env

let newline env = 
  string env "\n"

let in_stdout printer p = 
  ignore 
    (string 
       (printer { out = stdout; indent = 0; level = []; newline = false } p)
       "\n")

