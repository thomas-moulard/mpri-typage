(** Ce module impl�mente une notion de marque. *)

type t

(** On peut obtenir une marque qui n'a jamais servie (fraiche). *)
val fresh : unit -> t

(** On peut tester l'�galit� de deux marques. *)
val equal : t -> t -> bool

(** La marque suivante est la marque par d�faut. *)
val none : t
