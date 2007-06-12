{
  open Parser
  open Lexing
}

let newline = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ]

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9' '\''] 

let integer = [ '0'-'9' ]

let binop_char1 = [ '+' '-' '/' '<' '>' '%' '&' '^' '$' '@' '#' '\\' ]
let binop_char2 = [ '=' '-' 'A'-'Z' 'a'-'z' ]

let blank =
  [' ' '\009' '\012']

rule main = parse
| newline { let pos = lexbuf.lex_curr_p in
	lexbuf.lex_curr_p <- 
	{ pos with 
	    pos_lnum = pos.pos_lnum + 1;
	    pos_bol  = pos.pos_cnum;
	};
	main lexbuf }
| blank +
{ main lexbuf }
| "\""
{ 
  let b = Buffer.create 12 in
    string b lexbuf;
    STRING (Buffer.contents b)
}
| integer+
{ INT (int_of_string (lexeme lexbuf)) }
| "->"
{ ARROW }
| "*"
{ STAR }
| binop_char1 (binop_char1 | binop_char2)*
{ BINOP (lexeme lexbuf) }
| "type"
{ TYPE }
| "let"
{ LET }
| "let rec"
{ LETREC }
| "val"
{ VAL }
| "val rec"
{ VALREC }
| "match"
{ MATCH }
| "in"
{ IN }
| "end"
{ END }
| "with"
{ WITH }
| "of"
{ OF }
| "fun"
{ LAMBDA }
| "int"
{ TYINT }
| "string"
{ TYSTRING }
| "="
{ EQ }
| "("
{ LPAREN }
| ")"
{ RPAREN }
| "|"
{ PIPE }
| ":"
{ COLON }
| "."
{ DOT }
| lowercase identchar*
{ ID (lexeme lexbuf) }
| '\'' lowercase identchar*
{ TID (lexeme lexbuf) }
| uppercase identchar*
{ LID (lexeme lexbuf) }
| "(*"
{ comment 0 lexbuf }
| eof
{ EOF }
| _
{ failwith ("Unexpected token: " ^ lexeme lexbuf) }

and comment level = parse
newline { let pos = lexbuf.lex_curr_p in
	lexbuf.lex_curr_p <- 
	{ pos with 
	    pos_lnum = pos.pos_lnum + 1;
	    pos_bol  = pos.pos_cnum;
	};
	comment level lexbuf }
| "*)" 
{ if level = 0 then main lexbuf else comment (level - 1) lexbuf }
| "(*"
{ comment (level + 1) lexbuf }
| eof
{ failwith "Unterminated comment" }
| _ 
{ comment level lexbuf }

and string b = parse
  | "\""   { () }
  | "\\" _ { Buffer.add_string b (lexeme lexbuf); string b lexbuf } 
  | eof    { failwith "unterminated string" }
  | _      { Buffer.add_string b (lexeme lexbuf); string b lexbuf } 

and is_binop = parse
| binop_char1 (binop_char1 | binop_char2)* { true }
| _ { false } 

