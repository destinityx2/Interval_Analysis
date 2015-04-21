%{
	open Ast
%}

%token <string> INT
%token <string> VAR
%token PLUS
%token MINUS
%token MUL
%token DIV
%token MODULO
%token EQ
%token NE
%token GT
%token GE
%token LT
%token LE
%token AND
%token OR
%token SKIP
%token ASSIGN
%token WRITE
%token READ
%token WHILE
%token DO
%token IF
%token THEN
%token ELSE
%token SEMICOLON
%token NEWLINE
%token LEFTBRACKET
%token RIGHTBRACKET
%token EOF

%right ASSIGN
%left EQ NE GT GE LT LE
%left OR
%left AND
%left PLUS MINUS
%left MUL DIV MODULO

%start program

%type <Ast.expr list> program

%%

program: exprlist EOF                   { $1 }

exprlist :                     			    { [] }
		 | expr SEMICOLON exprlist          { $1 :: $3 }                             
  ;

expr : VAR                                          { Ast.VAR $1 }
     | INT                                          { Ast.NUMBER (int_of_string $1) }
     | expr PLUS expr                               { Ast.PLUS ($1, $3) }
     | expr MINUS expr                              { Ast.MINUS ($1, $3) }
     | expr MUL expr                                { Ast.MUL ($1, $3) }
     | expr DIV expr                                { Ast.DIV ($1, $3) }
     | expr MODULO expr                             { Ast.MODULO ($1, $3) }
     | expr EQ expr                                 { Ast.EQ ($1, $3) }
     | expr NE expr                                 { Ast.NE ($1, $3) }
     | expr GT expr                                 { Ast.GT ($1, $3) }
     | expr GE expr                                 { Ast.GE ($1, $3) }
     | expr LT expr                                 { Ast.LT ($1, $3) }
     | expr LE expr                                 { Ast.LE ($1, $3) }
     | expr AND expr                                { Ast.AND ($1, $3) }
     | expr OR expr                                 { Ast.OR ($1, $3) }
  ;