{
  open ConstraintParser
}

let newline = 
  ('\010' | '\013' | "\013\010")
let blank =
  [' ' '\009' '\012']
let lowercase =
  ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase =
  ['A'-'Z' '\192'-'\214' '\216'-'\222']
let greek_letter = 
  [  '\xCE' ] _
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9' '*' ]
let forall = "\xE2\x88\x80"
let exists = "\xE2\x88\x83"
let andsym = "\xE2\x88\xA7"
let kindarrow = "=>"
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1']*

rule token = parse
  | newline    { token lexbuf }
  | blank+     { token lexbuf }
  | ";"	       { EOF }
  | "/\\"      { AND }
  | "="        { EQ }
  | "->"       { ARROW }
  | "int"      { INT }
  | "string"   { STRING }
  | "<"	       { INST }
  | "let"      { LET }
  | "in"       { IN }
  | "true"     { TRUE }
  | "false"    { FALSE }
  | "dump"     { DUMP }
  | "forall"   { FORALL }
  | "exists"   { EXISTS }
  | "."	       { DOT }
  | "["	       { LBRACK }
  | "]"	       { RBRACK }
  | ":"	       { COLON }
  | "("	       { LPAREN }
  | ")"	       { RPAREN }
  | identchar+ { ID (Lexing.lexeme lexbuf) }
  | eof	       { EOF }
  | _	       { failwith ("Lexing error:"^(Lexing.lexeme lexbuf)) }
