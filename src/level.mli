(** Ce module définit une notion de niveau de priorité pour les 
    pretty-printers. *)

type t = InfixApp of int | PrefixApp

val arrow : t
