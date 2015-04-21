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
%token <int> SEMICOLON
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

%type <Ast.instruction list> program

%%

program: instrlist EOF                       { $1 }

instrlist :                     			 { [] }
		 | instr SEMICOLON instrlist          { 
                                            match $1 with
                                            | Ast.SKIP (a) -> Ast.SKIP($2) :: $3
                                            | Ast.ASSIGN(a, b, c) -> Ast.ASSIGN(a, b, $2) :: $3
                                            | Ast.WRITE (a, b) -> Ast.WRITE (a, $2) :: $3
                                            | Ast.READ (a, b) -> Ast.READ (a, $2) :: $3
                                            | Ast.WHILE(a, b, c) -> Ast.WHILE(a, $2 , $2) :: $3 
                                            | Ast.IF(a, b, c) -> Ast.IF(a, $2, $2) :: $3
                                          }                             
  ;
  /** Исправить в While и If вторые параметры, т.к. они некорректно считаются*/
  /** Переформировать в этом месте наши instruction. Сделать SEMICOLON аннотированным */

instr :   | SKIP                                    { Ast.SKIP (0) }
          | VAR ASSIGN expr                         { Ast.ASSIGN ($1, $3, 0) } 
          | WRITE expr                              { Ast.WRITE ($2, 0) }
          | READ VAR                                { Ast.READ ($2, 0) }
          | WHILE expr DO LEFTBRACKET instrlist RIGHTBRACKET             { Ast.WHILE($2, 0, 0) }
          | IF expr THEN LEFTBRACKET instrlist RIGHTBRACKET
            ELSE LEFTBRACKET instrlist RIGHTBRACKET { Ast.IF ($2, 0, 0) }
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