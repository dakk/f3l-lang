{
  open Parser
  open Lexing
	
	exception SyntaxError2 of string

  let reserved = [
    "type"; 
		"list";
    "open";
    "extern";
		"let";
    "rec";
		"in";
		"true";
		"false";
		"and";
		"or";
		"not";
    "fun";
  ]


	let next_line lexbuf =
		let pos = lexbuf.lex_curr_p in
		lexbuf.lex_curr_p <-
			{ pos with pos_bol = lexbuf.lex_curr_pos;
								pos_lnum = pos.pos_lnum + 1
			}

}

let digit = ['0'-'9']
let letter_up = ['A'-'Z']
let letter_dw = ['a'-'z']
let letter = letter_up | letter_dw

(* let mident = letter_up (letter | digit | '_')* *)
let mident = letter_up (letter | digit | '_')*
let ident = letter (letter | digit | '_')*
let nat = digit digit* "n"
let int = digit digit*
let float = digit digit* "." digit*

let hex_digit = ['a'-'f'] | ['A' - 'F'] | ['0' - '9']

let blank = [' ' '\t' '\r']
let newline = '\n'
let quote = '"'
let string = quote (letter | digit | ' ' | '\'' | '=' | ':' | '_' | '.' | '/')* quote
(* let squote = '\''
let char = squote (letter | digit | ' ' | '\'' | '=' | ':' | '_' | '.' | '/') squote *)

let bytes = 'b' string 

rule token = parse 
  | newline         { Lexing.new_line lexbuf; token lexbuf }
  | blank+          { token lexbuf }
  | float as f      { FLOAT (float_of_string f) }
  | int as i 			  { INT (int_of_string i) }
  | nat as i 			  { NAT (int_of_string (String.sub i 0 ((String.length i) - 1))) }

	| "()"					  { UNIT }
  | "external"      { EXTERNAL }
  | "open"          { OPEN }
  | "type"          { TYPE }
  | "list"          { CONT "list" }
  | "if"				  	{ IF }
  | "then"				  { THEN }
  | "else"				  { ELSE }
  | "and"				  	{ AND }
  | "or"				  	{ OR }
  | "not"				  	{ NOT }
	| "let"						{ LET }
	| "in"						{ IN }
  | "fun"           { FUN }
  | "rec"           { REC }
  
  | "'a"            { TANY }  
  | "->"				 	 	{ LAMBDA }
  | "{"             { LBRACE }
  | "}"             { RBRACE }
  | "["					  	{ LSQUARE }
  | "]"					  	{ RSQUARE }
  | "."					  	{ DOT }
  | "("             { LPAR }
  | ")"             { RPAR }
  | ","             { COMMA }
  | ":"             { COLON }
  | ";"             { SEMICOLON }
  | "|"             { PIPE }

  | "+"					  	{ ADD }
  | "-"					  	{ SUB }
  | "/"					  	{ DIV }
  | "*"					  	{ MUL }
  | "%"					  	{ MOD }
  | "="             { EQ }
  | "<>"				  	{ NEQ }
  | ">"					  	{ GT }
  | "<"					  	{ LT }
  | "<="			      { LTE }
  | ">="			      { GTE }
  | "true"				  { TRUE }
  | "false"				  { FALSE }

  | "(*"            { comment_multiline lexbuf; token lexbuf }

  | string as s     { STRING (String.sub s 1 ((String.length s) - 2)) }
  | bytes as s     	{ BYTES (String.sub s 2 ((String.length s) - 3)) }

  | mident as i      { if List.exists (fun r -> r = i) reserved then raise (SyntaxError2 ("Using reserved word for identifier")) else IDENT i }
  | ident as i      { if List.exists (fun r -> r = i) reserved then raise (SyntaxError2 ("Using reserved word for identifier")) else IDENT i }

  | eof             { EOF }
  | _ as c          { raise (SyntaxError2 (Format.sprintf "Invalid string starting with %C" c)) }



and comment_multiline = parse
  | "*)"   					{ () }
  | eof    					{ failwith "unterminated comment" }
  | newline					{ new_line lexbuf; comment_multiline lexbuf }
  | _      					{ comment_multiline lexbuf }