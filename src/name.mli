(** Ce module renvoie des chaines de caract�re pouvant �tre utilis�es comme
    noms frais. Ces cha�nes commencent par des "_". L'utilisateur ne 
    peut pas avoir choisi un identifiant de ce type gr�ce � la sp�cification 
    des lexeurs. *)

val fresh: unit -> string


