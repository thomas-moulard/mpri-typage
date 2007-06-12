(** Ce module renvoie des chaines de caractère pouvant être utilisées comme
    noms frais. Ces chaînes commencent par des "_". L'utilisateur ne 
    peut pas avoir choisi un identifiant de ce type grâce à la spécification 
    des lexeurs. *)

val fresh: unit -> string


