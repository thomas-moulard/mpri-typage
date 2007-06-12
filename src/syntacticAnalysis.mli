(** Ce module g�n�re un analyseur syntaxique � partir d'un parseur et 
    d'un lexeur. *)

module type Parser =
  sig
    type token
    type output
    val parser : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> output
    val lexer : Lexing.lexbuf -> token
    exception Error
  end
module Make : functor (S : Parser) -> sig val parse : string -> S.output end
