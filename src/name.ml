let name_counter = ref 0

(* Ces noms sont frais car "_" n'est pas utilisables par l'utilisateur
   dans le pr�fixe d'un identifiant. *)
let fresh () =
  incr name_counter;
  "_"^(string_of_int !name_counter)

