(** Ce module d�finit une notion de niveau de priorit� pour les 
    pretty-printers. *)

type t = InfixApp of int | PrefixApp

val arrow : t
