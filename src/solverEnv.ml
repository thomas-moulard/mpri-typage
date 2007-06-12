open Mark
open SolvingConstraint

module IntMap = Map.Make (struct 
			    type t = int 
			    let compare x y = x - y
			  end)

type env =
    {
      typs         : (Constraint.Id.Atom.t * var) list;
      pools        : (var list) IntMap.t;
      current_rank : int;
    }

let empty_env = 
  {
    typs = [];
    pools = IntMap.empty;
    current_rank = 0;
  }
    
let lookup env x =
  try
    snd (List.find (fun (y, _) -> Constraint.Id.Atom.equal x y) env.typs)
  with Not_found -> failwith ("Unbound "^ Constraint.Id.Atom.basename x)

let move_var m x =
  let r = rank x in
    if r = generalized_rank then
      m
    else 
      let vs = 
	try IntMap.find r m with Not_found -> 
	  Printf.printf "Invariant broken: %d is not available.\n" r;
	  exit 1
      in
	IntMap.add r (x :: vs) m

let add_in_pool k pools vs =
  try
    IntMap.add k (vs @ IntMap.find k pools) pools
  with Not_found ->
    IntMap.add k vs pools

let current_pool env =
  IntMap.find env.current_rank env.pools 

let iter_over_pools f env =
  IntMap.iter f env.pools

let current_rank env = 
  env.current_rank

let introduce new_rank env vs =
  let rank = 
    if new_rank then
      env.current_rank + 1 
    else 
      env.current_rank
  in
    { env with
	pools = add_in_pool rank env.pools (List.map (change_rank rank) vs);
	current_rank = rank
    }

let bind env x v =
  { env with typs = (x, v) :: env.typs }

let pop_rank env =
  { env with current_rank = current_rank env - 1 }

let pop_let env vs =
  let pools = IntMap.remove env.current_rank env.pools in
  let pools = List.fold_left move_var pools vs in
  let rank = env.current_rank - 1 in 
    {
      current_rank = rank;
      pools = pools;
      typs = List.tl env.typs;
    }

let rec fold_typ f accu = function

  | CTyVar x -> 
      f accu x 

  | CTyArrow (t1, t2) -> 
      fold_typ f (fold_typ f accu t1) t2

  | CTyConstant _ ->
      accu

  | CTyApp (x, ts) ->
      List.fold_left (fold_typ f) accu ts

let rec foldmap_typ f accu = function

  | CTyVar x -> 
      accu, CTyVar x

  | CTyArrow (t1, t2) ->
      let accu, t1 = foldmap_typ f accu t1 in
      let accu, t2 = foldmap_typ f accu t2 in
	accu, CTyArrow (t1, t2)

  | CTyConstant x ->
      accu, CTyConstant x

  | CTyApp (x, ts) ->
      let accu, ts = 
	List.fold_left (fun (accu, ts) t ->
		  let accu, t = foldmap_typ f accu t in
		    (accu, t :: ts)) (accu, []) ts 
      in
	accu, CTyApp (x, List.rev ts)

let scheme_var_names = ref []

let add_scheme_var_name x v =
  scheme_var_names := (x, v) :: !scheme_var_names

let find_scheme_var_name x = 
  snd (List.find (fun (a, _) -> equivalent x a) 
	 !scheme_var_names)

let scheme_var_name_counter = ref 0

let scheme_var_reset () = 
  scheme_var_name_counter := 0;
  scheme_var_names := []

let new_scheme_var_name () =
  incr scheme_var_name_counter;
  let name = String.make 1 (char_of_int (96 + !scheme_var_name_counter mod 10)) in
    "'" ^
      if !scheme_var_name_counter > 10 then
	name ^ string_of_int (!scheme_var_name_counter / 10)
      else 
	name

let scheme_var_name x = 
  try
    find_scheme_var_name x
  with Not_found ->
    let name = new_scheme_var_name () in
      add_scheme_var_name x name;
      name

let rec string_of_var x =
  match structure x with
    | None -> 
	scheme_var_name x

    | Some t -> string_of_typ t 

and string_of_typ = function
  | CTyVar x -> string_of_var x
  | CTyArrow (t1, t2) ->
      "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | CTyConstant CTyInt ->
      "int"
  | CTyConstant CTyString ->
      "string"
  | CTyApp (x, []) ->
      Constraint.Var.Atom.basename x
  | CTyApp (x, ts) ->
      Constraint.Var.Atom.basename x ^ " " 
      ^ String.concat " " (List.map string_of_typ ts)

let string_of_scheme x =
  scheme_var_reset ();
  let add_generalized_var gvars x =
    if not (List.exists (equivalent x) gvars) then
      x :: gvars
    else 
      gvars
  in
  let rec traverse gvars x =
    match structure x with
      | None -> 
	  if rank x = generalized_rank then 
	    add_generalized_var gvars x 
	  else gvars
      | Some t -> fold_typ traverse gvars t
  in
  let gvars = traverse [] x in
  let fl = 
    if gvars <> [] then 
      "forall "^ String.concat " " (List.map string_of_var gvars) ^"." 
    else ""
  in
    fl ^ string_of_var x

let string_of_env env =
  String.concat "\n" 
    (List.map (fun (x, t) -> 
		 Constraint.Id.Atom.basename x ^ " : " 
		 ^ string_of_scheme t) env.typs)

