%{
	open Parse_tree
	open Pt_loc 

  exception SyntaxError2 of string
%}

%token EOF
%token LBRACE, RBRACE, LPAR, RPAR, COMMA, COLON, SEMICOLON, PIPE, EQ, DOT
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
%token <string> IDENT

%token MATCH, WITH, UNDERSCORE, PIPEGT

%left PIPEGT
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
  | LPAR i=IDENT COLON t=type_sig RPAR		{ (i, t) }

  param_opt_typed2: 
	| i=IDENT  									{ (i, PTBuiltin("'a")) }
  | i=IDENT COLON t=type_sig	{ (i, t) }

  ident: | i=IDENT { i }

  type_sig:
    | TANY                                          { PTBuiltin ("'a") }
    | t=ident                                       { PTBuiltin (t) }
    | t1=type_sig MUL t2=type_sig                   { PTPair (t1, t2) }
    | LBRACE tl=separated_nonempty_list(SEMICOLON, parameter) RBRACE
                                                    { PTRecord (tl)}
    | x=ident PIPE el=separated_nonempty_list(PIPE, ident)  
                                                    { PTUnion (x::el) }
    | LPAR t=type_sig RPAR											    { t }
    | p=type_sig LAMBDA pr=type_sig									{ PTLambda (p, pr) }

  type_expr: | te=type_sig {te}

  erec_element:
    | i=IDENT EQ b=expr { (i, b) }

	left:
		| l=left DOT i=IDENT				{ loce $startpos $endpos @@ PEDot (l, i) }
    | i=IDENT 						      { loce $startpos $endpos @@ PERef (i) }

  value:
  	| UNIT						{ loce $startpos $endpos @@ PEUnit }
    | TRUE            { loce $startpos $endpos @@ PEBool (true) }
    | FALSE           { loce $startpos $endpos @@ PEBool (false) }
    | x=STRING 				{ loce $startpos $endpos @@ PEString (x) }
    | x=BYTES			 		{ loce $startpos $endpos @@ PEBytes (x) }
    | x=FLOAT					{ loce $startpos $endpos @@ PEFloat (x) }
    | x=INT 					{ loce $startpos $endpos @@ PEInt (x) }
    | x=NAT 					{ loce $startpos $endpos @@ PENat (x) }
    | i=IDENT 				{ loce $startpos $endpos @@ PERef (i) }

  case:
    | x=value LAMBDA e=expr     { (Some(x), e) }
    | UNDERSCORE LAMBDA e=expr  { (None, e) }

  expr:
    | e=value                   { loce $startpos $endpos @@ e }
    | LBRACE tl=separated_nonempty_list(SEMICOLON, erec_element) RBRACE
                                { loce $startpos $endpos @@ PERecord (tl) }
    | LPAR t1=expr COMMA t2=expr RPAR
                                { loce $startpos $endpos @@ PEPair (t1, t2) }
    | FUN LPAR RPAR LAMBDA e=expr
                                { loce $startpos $endpos @@ PELambda (("_", PTBuiltin("unit")), e) }
    | FUN p=param_opt_typed LAMBDA e=expr
                                { loce $startpos $endpos @@ PELambda (p, e) }

		// bindings 
		| LET i=IDENT COLON t=type_sig EQ e=expr IN ee=expr { loce $startpos $endpos @@ PELetIn (i, Some(t), e, ee, false) }
		| LET i=IDENT EQ e=expr IN ee=expr { loce $startpos $endpos @@ PELetIn (i, None, e, ee, false) }
		| LET REC i=IDENT COLON t=type_sig EQ e=expr IN ee=expr { loce $startpos $endpos @@ PELetIn (i, Some(t), e, ee, true) }
		| LET REC i=IDENT EQ e=expr IN ee=expr { loce $startpos $endpos @@ PELetIn (i, None, e, ee, true) }

    // arithm
    | e1=expr ADD e2=expr 			{ loce $startpos $endpos @@ PEAdd (e1,e2) }
    | e1=expr SUB e2=expr 			{ loce $startpos $endpos @@ PESub (e1,e2) }
    | e1=expr DIV e2=expr 			{ loce $startpos $endpos @@ PEDiv (e1,e2) }
    | e1=expr MUL e2=expr 			{ loce $startpos $endpos @@ PEMul (e1,e2) }
    | e1=expr MOD e2=expr 			{ loce $startpos $endpos @@ PEMod (e1,e2) }

    // boolean
    | e1=expr AND e2=expr 			{ loce $startpos $endpos @@ PEAnd (e1,e2) }
    | e1=expr OR e2=expr 			  { loce $startpos $endpos @@ PEOr (e1,e2) }
    | NOT e=expr 					      { loce $startpos $endpos @@ PENot (e) }
    | e1=expr LT e2=expr 			  { loce $startpos $endpos @@ PELt (e1,e2) }
    | e1=expr LTE e2=expr 			{ loce $startpos $endpos @@ PELte (e1,e2) }
    | e1=expr GT e2=expr 			  { loce $startpos $endpos @@ PEGt (e1,e2) }
    | e1=expr GTE e2=expr 			{ loce $startpos $endpos @@ PEGte (e1,e2) }
    | e1=expr EQ e2=expr 			  { loce $startpos $endpos @@ PEEq (e1,e2) }
    | e1=expr NEQ e2=expr 			{ loce $startpos $endpos @@ PENeq (e1,e2) }

    // if then else
    | IF c=expr THEN e1=expr ELSE e2=expr 
                                { loce $startpos $endpos @@ PEIfThenElse (c,e1,e2) }

    | e=left DOT i=IDENT 				{ loce $startpos $endpos @@ PEDot (e, i) }
    | e=expr DOT i=IDENT 				{ loce $startpos $endpos @@ PEDot (e, i) }

    // apply a function
    | i=left LPAR p=expr RPAR 			{ loce $startpos $endpos @@ PEApply(i, p) }
    | i=expr LPAR p=expr RPAR 			{ loce $startpos $endpos @@ PEApply(i, p) }
		
    | LPAR e=expr RPAR 				          { loce $startpos $endpos @@ e }
    | LPAR v=expr COLON t=type_sig RPAR { loce $startpos $endpos @@ PETyped (v, t) }


    ////////// SUGAR

    // Multi-param lambda 
    // DEF: defun f (x,y) = fun x -> fun y -> x + y
    // APPLY: f(x,y) = f(x)(y)
    | FUN LPAR p=separated_nonempty_list(COMMA, param_opt_typed2) RPAR LAMBDA e=expr
                                  { let rec lbuild cl = match cl with 
                                    | c::[] -> PELambda (c, e)
                                    | c::cl -> PELambda (c, lbuild cl)
                                    in loce $startpos $endpos @@ lbuild p }


    // |> apply 
    | p=expr PIPEGT i=expr  			{ loce $startpos $endpos @@ PEApply(i, p) }

    // Match with
    | MATCH c=expr WITH w=separated_nonempty_list(PIPE, case)
    | MATCH c=expr WITH PIPE w=separated_nonempty_list(PIPE, case)
                                { let rec cases cl = match cl with 
                                  | [] -> raise (SyntaxError2 ("No cases in match"))
                                  | (None, e) :: [] -> e
                                  | (None, _) :: _ -> raise (SyntaxError2 ("Default case should be the last"))                             
                                  | (Some(_), e) :: [] -> e (* this cause runtime problems if the match-case is not exaustive *)
                                  | (Some(v), e) :: xl' -> PEIfThenElse (PEEq(c, v), e, cases xl')
                                  in loce $startpos $endpos @@ cases w }


  dtype:
    | TYPE TANY x=ident EQ tl=type_sig 
      { DType (x, tl) }

    | TYPE x=IDENT EQ tl=type_sig
      { DType (x, tl) }

  ddef:
    | LET x=IDENT COLON t=type_expr EQ v=expr
      { DDef (x, Some(t), v, false) }
    | LET x=IDENT EQ v=expr
      { DDef (x, None, v, false) }
    | LET REC x=IDENT COLON t=type_expr EQ v=expr
      { DDef (x, Some(t), v, true) }
    | LET REC x=IDENT EQ v=expr
      { DDef (x, None, v, true) }

  dexternal:
    | EXTERNAL x=IDENT COLON t=type_expr EQ n=STRING
      { DExternal (x, t, n) }

  dopen: | OPEN p=ident { DOpen (p) }

  declaration:
    | t=dtype           { locd $startpos $endpos t }
    | d=ddef            { locd $startpos $endpos d }
    | e=dexternal       { locd $startpos $endpos e }
    | o=dopen           { locd $startpos $endpos o }
