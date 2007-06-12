exception Error

type token = 
  | WITH
  | VALREC
  | VAL
  | TYSTRING
  | TYPE
  | TYINT
  | TID of (string)
  | STRING of (string)
  | STAR
  | RPAREN
  | PIPE
  | OF
  | MATCH
  | LPAREN
  | LID of (string)
  | LETREC
  | LET
  | LAMBDA
  | INT of (int)
  | IN
  | ID of (string)
  | EQ
  | EOF
  | END
  | DOT
  | COLON
  | BINOP of (string)
  | ARROW


val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.Raw.program)