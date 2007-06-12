(** Ce module implémente des primitives pour les pretty-printer. *)

type env 

val new_block : env -> env

val end_block : env -> env

val new_level : env -> Level.t -> env

val end_level : env -> env

val at_toplevel : env -> bool

val new_line : env -> env

val indent_space : env -> string

val string : env -> string -> env

val space : (env -> 'a) -> env -> 'a

val seps :
  (('a -> 'b -> 'a) -> 'a -> 'b -> 'a) ->
  ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

val ended :
  (('a -> 'a) -> 'b -> 'c) -> ('c -> 'd -> 'b) -> 'c -> 'd list -> 'c

val paren : (env -> env) -> env -> env

val may_paren : (env -> env) -> env -> env

val newline : env -> env

val in_stdout : (env -> 'a -> env) -> 'a -> unit
