%{
	open Parse_tree
	open Pt_loc 
%}

%token EOF
// QUOTE SIZE HT ASTERISK AT GET HAS QUESTION ASSERT
%token LBRACE, RBRACE, LPAR, RPAR, COMMA, COLON, SEMICOLON, PIPE, EQ, DOT, LSQUARE, RSQUARE
%token ENUM, TYPE, DEF, AND, OR, NOT, TRUE, FALSE
%token ADD, SUB, DIV, MUL, MOD, IF, THEN, ELSE, WITH, MATCH
%token LTE, LT, GT, GTE, EQEQ, NONE, SOME, HT, LET, IN
%token LAMBDAB, NEQ, UNIT, UNDERSCORE
%token OPEN, EXTERNAL, MODULE, STRUCT, END, RET
%token LAMBDA
%token <string> STRING
%token <string> BYTES
%token <int> INT
%token <int> NAT
%token <float> FLOAT
%token <string> CONT
%token <string> IDENT

%left NOT
%left OR
%left AND
%left EQEQ, NEQ
%left LTE, LT, GT, GTE
%left ADD, SUB
%left MOD
%left MUL, DIV

%start <Parse_tree.t> program

%%
  program: dl=expr EOF { dl }

  ident: | i=IDENT { i }

  parameter: | i=IDENT COLON t=type_sig { (i, t) }

  param_opt_typed: 
	| i=IDENT COLON t=type_sig 	{ (i, Some(t)) }
	| i=IDENT  									{ (i, None) }

  type_sig:
    | t=ident                                       { Parse_tree.PTBuiltin (t) }
    | bt=type_expr c=CONT                           { Parse_tree.PTCont (c, bt) }
    | LPAR t1=type_sig MUL tl=separated_nonempty_list(MUL, type_sig) RPAR         
                                                    { Parse_tree.PTTuple (t1::tl) }
    | LBRACE tl=separated_nonempty_list(SEMICOLON, parameter) RBRACE
                                                    { Parse_tree.PTRecord (tl)}
    | ENUM LPAR el=separated_list(PIPE, ident) RPAR { Parse_tree.PTEnum (el) }
    | LPAR t=type_sig RPAR											    { t }
    | p=type_sig LAMBDA pr=type_sig									{ Parse_tree.PTLambda (p, pr) }

  type_expr: | te=type_sig {te}

  erec_element:
    | i=IDENT EQ b=expr { (i, b) }
    
	match_case:
		| PIPE e=expr LAMBDA v=expr { (e, v) }
		| PIPE UNDERSCORE LAMBDA v=expr { (loce $startpos $endpos @@ Parse_tree.PECaseDefault, v) }

	left:
		| l=left DOT i=IDENT				{ loce $startpos $endpos @@ Parse_tree.PEDot (l, i) }
    | i=IDENT 						      { loce $startpos $endpos @@ Parse_tree.PERef (i) }

  expr:
		| UNIT						{ loce $startpos $endpos @@ Parse_tree.PEUnit }
    | NONE  					{ loce $startpos $endpos @@ Parse_tree.PENone }
    | TRUE            { loce $startpos $endpos @@ Parse_tree.PEBool (true) }
    | FALSE           { loce $startpos $endpos @@ Parse_tree.PEBool (false) }
    | x=STRING 				{ loce $startpos $endpos @@ Parse_tree.PEString (x) }
    | x=BYTES			 		{ loce $startpos $endpos @@ Parse_tree.PEBytes (x) }
    | x=FLOAT					{ loce $startpos $endpos @@ Parse_tree.PEFloat (x) }
    | x=INT 					{ loce $startpos $endpos @@ Parse_tree.PEInt (x) }
    | x=NAT 					{ loce $startpos $endpos @@ Parse_tree.PENat (x) }
    | SOME LPAR x=expr RPAR 	  { loce $startpos $endpos @@ Parse_tree.PESome (x) }
    | LBRACE tl=separated_nonempty_list(SEMICOLON, erec_element) RBRACE
                                { loce $startpos $endpos @@ Parse_tree.PERecord (tl) }
    | LSQUARE tl=separated_list(COMMA, expr) RSQUARE
                                { loce $startpos $endpos @@ Parse_tree.PEList (tl) }
    | LPAR t=expr COMMA tl=separated_nonempty_list(COMMA, expr) RPAR
                                { loce $startpos $endpos @@ Parse_tree.PETuple (t::tl) }
    | LPAR tl=separated_list(COMMA, parameter) RPAR LAMBDAB LPAR e=expr RPAR
                                { loce $startpos $endpos @@ Parse_tree.PELambda (tl, e) }

		// bindings 
		| LET i=IDENT COLON t=type_sig EQ e=expr IN ee=expr { loce $startpos $endpos @@ Parse_tree.PELetIn (i, Some(t), e, ee) }
		| LET i=IDENT COLON t=type_sig EQ e=expr { loce $startpos $endpos @@ Parse_tree.PELet (i, Some(t), e) }
		| LET i=IDENT EQ e=expr IN ee=expr { loce $startpos $endpos @@ Parse_tree.PELetIn (i, None, e, ee) }
		| LET i=IDENT EQ e=expr { loce $startpos $endpos @@ Parse_tree.PELet (i, None, e) }

		| LET LPAR tl=separated_nonempty_list(COMMA, param_opt_typed) RPAR EQ e=expr IN ee=expr 
			{ loce $startpos $endpos @@ Parse_tree.PELetTupleIn (tl, e, ee) }
		| LET LPAR tl=separated_nonempty_list(COMMA, param_opt_typed) RPAR EQ e=expr 
			{ loce $startpos $endpos @@ Parse_tree.PELetTuple (tl, e) }


    // arithm
    | e1=expr ADD e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PEAdd (e1,e2) }
    | e1=expr SUB e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PESub (e1,e2) }
    | e1=expr DIV e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PEDiv (e1,e2) }
    | e1=expr MUL e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PEMul (e1,e2) }
    | e1=expr MOD e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PEMod (e1,e2) }

    // boolean
    | e1=expr AND e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PEAnd (e1,e2) }
    | e1=expr OR e2=expr 			  { loce $startpos $endpos @@ Parse_tree.PEOr (e1,e2) }
    | NOT e=expr 					      { loce $startpos $endpos @@ Parse_tree.PENot (e) }
    | e1=expr LT e2=expr 			  { loce $startpos $endpos @@ Parse_tree.PELt (e1,e2) }
    | e1=expr LTE e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PELte (e1,e2) }
    | e1=expr GT e2=expr 			  { loce $startpos $endpos @@ Parse_tree.PEGt (e1,e2) }
    | e1=expr GTE e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PEGte (e1,e2) }
    | e1=expr EQEQ e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PEEq (e1,e2) }
    | e1=expr NEQ e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PENeq (e1,e2) }

    // if then else
    | IF c=expr THEN e1=expr ELSE e2=expr 
                                { loce $startpos $endpos @@ Parse_tree.PEIfThenElse (c,e1,e2) }

		// match with
		| MATCH c=expr WITH cl=nonempty_list(match_case) 
																{ loce $startpos $endpos @@ Parse_tree.PEMatchWith (c, cl) }

    | i=IDENT 						      { loce $startpos $endpos @@ Parse_tree.PERef (i) }
    | e=left DOT i=IDENT 				{ loce $startpos $endpos @@ Parse_tree.PEDot (e, i) }
    | e=expr DOT i=IDENT 				{ loce $startpos $endpos @@ Parse_tree.PEDot (e, i) }
    | ii=IDENT HT i=IDENT 			{ loce $startpos $endpos @@ Parse_tree.PEHt (ii, i) }

    // apply a function
    | i=left LPAR p=separated_list(COMMA, expr) RPAR 			{ loce $startpos $endpos @@ PEApply(i, p) }
    | i=expr LPAR p=separated_list(COMMA, expr) RPAR 			{ loce $startpos $endpos @@ PEApply(i, p) }
		
    | LPAR e=expr RPAR 				  { loce $startpos $endpos @@ e }
    | LPAR v=expr COLON t=type_sig RPAR { loce $startpos $endpos @@ Parse_tree.PETyped (v, t) }

    | TYPE x=IDENT EQ tl=type_sig
      { loce $startpos $endpos @@ Parse_tree.PEType (x, tl) }

    | EXTERNAL x=IDENT COLON t=type_expr EQ n=STRING
      { loce $startpos $endpos @@ Parse_tree.PEExternal (x, t, n) }

    | MODULE x=IDENT EQ STRUCT dl=expr END
      { loce $startpos $endpos @@ Parse_tree.PEModule (x, dl) }
  
    | OPEN p=ident 
      { loce $startpos $endpos @@ Parse_tree.PEOpen (p) }

    | a=expr RET b=expr
      { loce $startpos $endpos @@ Parse_tree.PESeq (a, b) }

    | a=expr SEMICOLON SEMICOLON b=expr
      { loce $startpos $endpos @@ Parse_tree.PESeq (a, b) }
