%{
	open Ast

	(** TODO: Создать здесь глобальный список result_list с типом Ast.instruction и все в него напихать. *)
	let result_list = ref [];;

  let compare x y =
    if (Ast.get_pc x) < (Ast.get_pc y) then -1 else if (Ast.get_pc x) == (Ast.get_pc y) then 0 else 1;;
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

program: instrlist EOF    {
                            result_list := List.sort compare !result_list;
							!result_list 
					      } 

instrlist : | instr instrlist                       {  } /** Здесь ничего не делаем */
			| instr                                 {  } /** Здесь ничего не делаем */
  ;


  /** TODO: Не возвращать здесь значения, а засовывать их сразу в глобальный список */

instr :   | SKIP SEMICOLON                                     { result_list := Ast.SKIP ($2) :: !result_list   }
          | VAR ASSIGN expr SEMICOLON                          { result_list := Ast.ASSIGN ($4, $1, $3) :: !result_list } 
          | WRITE expr SEMICOLON                               { result_list := Ast.WRITE ($3, $2) :: !result_list }
          | READ VAR SEMICOLON                                 { result_list := Ast.READ ($3, $2) :: !result_list }
          | WHILE expr DO LEFTBRACKET instrlist RIGHTBRACKET   { result_list := Ast.WHILE($1, $2, $6 + 1) :: !result_list;
                                                                 result_list := Ast.RIGHTBRACKET($6, $1) :: !result_list }
          | IF expr THEN LEFTBRACKET instrlist RIGHTBRACKET
            ELSE LEFTBRACKET instrlist RIGHTBRACKET            { result_list := Ast.IF ($1, $2, $6 + 1) :: !result_list;
            													 result_list := Ast.RIGHTBRACKET($10, $10 + 1) :: !result_list;
                                                                 result_list := Ast.RIGHTBRACKET($6, $10 + 1) :: !result_list }
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