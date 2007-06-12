
module type Parser = 
sig
  type token 
  type output 
  val parser : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> output
  val lexer : Lexing.lexbuf -> token
  exception Error 
end

module Make (S : Parser) =
struct

  let parse filename =
    let channel = open_in filename in
    let lexbuf = Lexing.from_channel channel in
      lexbuf.Lexing.lex_curr_p <-
	{
          Lexing.pos_fname = filename;
          Lexing.pos_lnum  = 1;
          Lexing.pos_bol   = 0;
          Lexing.pos_cnum  = 0
	};
      try
	S.parser S.lexer lexbuf
      with S.Error ->
	Printf.printf "%s: %s\n" 
	  (Positions.string_of_pos 
	     (Positions.lex_join 
		(Lexing.lexeme_start_p lexbuf)
		(Lexing.lexeme_end_p lexbuf)))
	  "Syntax error.";
	exit 1
end

