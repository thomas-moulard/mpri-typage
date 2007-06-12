exception Error

type token = 
  | TRUE
  | STRING
  | RPAREN
  | RBRACK
  | LPAREN
  | LET
  | LBRACK
  | INT
  | INST
  | IN
  | ID of (string)
  | FORALL
  | FALSE
  | EXISTS
  | EQ
  | EOF
  | DUMP
  | DOT
  | COLON
  | ARROW
  | AND


val input: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Constraint.Raw.ty_constraint)