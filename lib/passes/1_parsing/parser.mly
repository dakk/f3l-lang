%{
	open Parse_tree
	open Pt_loc 
%}

%token EOF
%token LBRACE, RBRACE, LPAR, RPAR, COMMA, COLON, SEMICOLON, PIPE, EQ, DOT, LSQUARE, RSQUARE
%token TYPE, AND, OR, NOT, TRUE, FALSE
%token ADD, SUB, DIV, MUL, MOD, IF, THEN, ELSE
%token LTE, LT, GT, GTE, LET, IN
%token NEQ, UNIT, TANY, REC
%token OPEN, EXTERNAL
%token LAMBDA, FUN
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
%left NEQ
%left LTE, LT, GT, GTE
%left ADD, SUB
%left MOD
%left MUL, DIV

%start <Parse_tree.t> program

%%
  program: dl=list(declaration) EOF { dl }

  parameter: | i=IDENT COLON t=type_sig { (i, t) }

  param_opt_typed: 
	| i=IDENT  									{ (i, PTBuiltin("'a")) }
  | i=IDENT COLON t=IDENT 		{ (i, PTBuiltin(t)) }

  ident: | i=IDENT { i }

  type_sig:
    | TANY                                          { Parse_tree.PTBuiltin ("'a") }
    | t=ident                                       { Parse_tree.PTBuiltin (t) }
    | bt=type_expr c=CONT                           { Parse_tree.PTCont (c, bt) }
    | LPAR t1=type_sig MUL t2=type_sig RPAR         { Parse_tree.PTPair (t1, t2) }
    | LBRACE tl=separated_nonempty_list(SEMICOLON, parameter) RBRACE
                                                    { Parse_tree.PTRecord (tl)}
    | x=ident PIPE el=separated_nonempty_list(PIPE, ident)  
                                                    { Parse_tree.PTUnion (x::el) }
    | LPAR t=type_sig RPAR											    { t }
    | p=type_sig LAMBDA pr=type_sig									{ Parse_tree.PTLambda (p, pr) }

  type_expr: | te=type_sig {te}

  erec_element:
    | i=IDENT EQ b=expr { (i, b) }

	left:
		| l=left DOT i=IDENT				{ loce $startpos $endpos @@ Parse_tree.PEDot (l, i) }
    | i=IDENT 						      { loce $startpos $endpos @@ Parse_tree.PERef (i) }

  expr:
		| UNIT						{ loce $startpos $endpos @@ Parse_tree.PEUnit }
    | TRUE            { loce $startpos $endpos @@ Parse_tree.PEBool (true) }
    | FALSE           { loce $startpos $endpos @@ Parse_tree.PEBool (false) }
    | x=STRING 				{ loce $startpos $endpos @@ Parse_tree.PEString (x) }
    | x=BYTES			 		{ loce $startpos $endpos @@ Parse_tree.PEBytes (x) }
    | x=FLOAT					{ loce $startpos $endpos @@ Parse_tree.PEFloat (x) }
    | x=INT 					{ loce $startpos $endpos @@ Parse_tree.PEInt (x) }
    | x=NAT 					{ loce $startpos $endpos @@ Parse_tree.PENat (x) }
    | LBRACE tl=separated_nonempty_list(SEMICOLON, erec_element) RBRACE
                                { loce $startpos $endpos @@ Parse_tree.PERecord (tl) }
    | LPAR t1=expr COMMA t2=expr RPAR
                                { loce $startpos $endpos @@ Parse_tree.PEPair (t1, t2) }
    | FUN LPAR tl=separated_list(COMMA, parameter) RPAR LAMBDA e=expr
                                { loce $startpos $endpos @@ Parse_tree.PELambda (tl, e) }
    | FUN p=param_opt_typed LAMBDA e=expr
                                { loce $startpos $endpos @@ Parse_tree.PELambda ([p], e) }

		// bindings 
		| LET i=IDENT COLON t=type_sig EQ e=expr IN ee=expr { loce $startpos $endpos @@ Parse_tree.PELetIn (i, Some(t), e, ee, false) }
		| LET i=IDENT EQ e=expr IN ee=expr { loce $startpos $endpos @@ Parse_tree.PELetIn (i, None, e, ee, false) }
		| LET REC i=IDENT COLON t=type_sig EQ e=expr IN ee=expr { loce $startpos $endpos @@ Parse_tree.PELetIn (i, Some(t), e, ee, true) }
		| LET REC i=IDENT EQ e=expr IN ee=expr { loce $startpos $endpos @@ Parse_tree.PELetIn (i, None, e, ee, true) }

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
    | e1=expr EQ e2=expr 			  { loce $startpos $endpos @@ Parse_tree.PEEq (e1,e2) }
    | e1=expr NEQ e2=expr 			{ loce $startpos $endpos @@ Parse_tree.PENeq (e1,e2) }

    // if then else
    | IF c=expr THEN e1=expr ELSE e2=expr 
                                { loce $startpos $endpos @@ Parse_tree.PEIfThenElse (c,e1,e2) }


    | i=IDENT 						      { loce $startpos $endpos @@ Parse_tree.PERef (i) }
    | e=left DOT i=IDENT 				{ loce $startpos $endpos @@ Parse_tree.PEDot (e, i) }
    | e=expr DOT i=IDENT 				{ loce $startpos $endpos @@ Parse_tree.PEDot (e, i) }

    // apply a function
    // | i=left p=expr  			                                { loce $startpos $endpos @@ PEApply(i, p) }
    // | i=expr p=expr  			                                { loce $startpos $endpos @@ PEApply(i, p) }
    | i=left LPAR p=expr RPAR 			{ loce $startpos $endpos @@ PEApply(i, p) }
    | i=expr LPAR p=expr RPAR 			{ loce $startpos $endpos @@ PEApply(i, p) }
		
    | LPAR e=expr RPAR 				  { loce $startpos $endpos @@ e }
    | LPAR v=expr COLON t=type_sig RPAR { loce $startpos $endpos @@ Parse_tree.PETyped (v, t) }

  dtype:
    | TYPE TANY x=ident EQ tl=type_sig 
      { Parse_tree.DType (x, tl, true) }

    | TYPE x=IDENT EQ tl=type_sig
      { Parse_tree.DType (x, tl, false) }

  ddef:
    | LET x=IDENT COLON t=type_expr EQ v=expr
      { Parse_tree.DDef (x, Some(t), v, false) }
    | LET x=IDENT EQ v=expr
      { Parse_tree.DDef (x, None, v, false) }
    | LET REC x=IDENT COLON t=type_expr EQ v=expr
      { Parse_tree.DDef (x, Some(t), v, true) }
    | LET REC x=IDENT EQ v=expr
      { Parse_tree.DDef (x, None, v, true) }

  dexternal:
    | EXTERNAL x=IDENT COLON t=type_expr EQ n=STRING
      { Parse_tree.DExternal (x, t, n) }

  dopen: | OPEN p=ident { Parse_tree.DOpen (p) }

  declaration:
    | t=dtype           { locd $startpos $endpos t }
    | d=ddef            { locd $startpos $endpos d }
    | e=dexternal       { locd $startpos $endpos e }
    | o=dopen           { locd $startpos $endpos o }
