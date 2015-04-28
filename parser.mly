%{
	open Ast


	(** TODO: Создать здесь глобальный список result_list с типом Ast.instruction и все в него напихать. *)
	let result_list = ref [Ast.SKIP (1); Ast.SKIP (0)];;
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
%token <int> WHILE
%token DO
%token <int> IF
%token THEN
%token ELSE
%token <int> SEMICOLON
%token NEWLINE
%token LEFTBRACKET
%token <int> RIGHTBRACKET
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

program: instrlist EOF                              {	
														!result_list 
													} 

																	/** Возвращаем глобальный список, но сначала 
                                                                        сортируем его! 
                                                                     */

instrlist : | instr instrlist                       {  } /** Здесь ничего не делаем */
			| instr                                 {  } /** Здесь ничего не делаем */
  ;


  /** TODO: Не возвращать здесь значения, а засовывать их сразу в глобальный список */

instr :   | SKIP SEMICOLON                                     { result_list := !result_list :: Ast.SKIP ($2) }
          | VAR ASSIGN expr SEMICOLON                          { result_list := !result_list :: Ast.ASSIGN ($4, $1, $3) } 
          | WRITE expr SEMICOLON                               { result_list := !result_list :: Ast.WRITE ($3, $2) }
          | READ VAR SEMICOLON                                 { result_list := !result_list :: Ast.READ ($3, $2) }
          | WHILE expr DO LEFTBRACKET instr RIGHTBRACKET       { result_list := !result_list :: Ast.WHILE($1, $2, $6 + 1) }
          | IF expr THEN LEFTBRACKET instr RIGHTBRACKET
            ELSE LEFTBRACKET instr RIGHTBRACKET                { result_list := !result_list :: Ast.IF ($1, $2, $6 + 1) }
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