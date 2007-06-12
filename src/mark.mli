(** Ce module implémente une notion de marque. *)

type t

(** On peut obtenir une marque qui n'a jamais servie (fraiche). *)
val fresh : unit -> t

(** On peut tester l'égalité de deux marques. *)
val equal : t -> t -> bool

(** La marque suivante est la marque par défaut. *)
val none : t
