%{
	open Ast;;
  open Hashtbl;;
	let var_set = ref [];;

	let f res e = if List.mem e res then res else e::res;;
	let unique lst = List.fold_left f [] lst;;
	
	(** TODO: Создать здесь глобальный список result_list с типом Ast.instruction и все в него напихать. *)
	let result_list = ref [];;

  let compare x y =
    if (Ast.get_pc x) < (Ast.get_pc y) then -1 else if (Ast.get_pc x) == (Ast.get_pc y) then 0 else 1;;

  let cur_index = ref 0;;

  let tmp_list = ref [];;
  let renumerate instr = match instr with
    | SKIP (pc) -> begin cur_index := !cur_index + 1; tmp_list := SKIP(!cur_index) :: !tmp_list end
    | ASSIGN (pc, var, expr) -> begin cur_index := !cur_index + 1; tmp_list := ASSIGN(!cur_index, var, expr) :: !tmp_list end
    | WRITE (pc, expr) -> begin cur_index := !cur_index + 1; tmp_list := WRITE(!cur_index, expr) :: !tmp_list end
    | READ (pc, var) -> begin cur_index := !cur_index + 1; tmp_list := READ(!cur_index, var) :: !tmp_list end
    | WHILE (pc, expr, pc') -> begin cur_index := !cur_index + 1; tmp_list := WHILE(!cur_index, expr, !cur_index + pc' - pc) :: !tmp_list end
    | IF (pc, expr, pc') -> begin cur_index := !cur_index + 1; tmp_list := IF(!cur_index, expr, !cur_index + pc' - pc) :: !tmp_list end
    | RETURN (pc, expr) -> begin cur_index := !cur_index + 1; tmp_list := RETURN(!cur_index, expr) :: !tmp_list end
    | RIGHTBRACKET (pc, pc') -> begin cur_index := !cur_index + 1; tmp_list := RIGHTBRACKET(!cur_index, !cur_index + pc' - pc) :: !tmp_list end;;

  let hash_table = Hashtbl.create 123;;
%}

%token <string> INT
%token <string> VAR
%token COMMA
%token FUN
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
%token <int> LEFTPARENTHESIS
%token RIGHTPARENTHESIS
%token RETURN
%token EOF

%right ASSIGN
%left EQ NE GT GE LT LE
%left OR
%left AND
%left PLUS MINUS
%left MUL DIV MODULO

%start program

%type < (string, Ast.instruction list * string list * Ast.var list ) Hashtbl.t > program

%%

program: | func_list              { hash_table }
         | EOF                    { hash_table } 
  ;


instrlist : | instr instrlist                       {  } /** Здесь ничего не делаем */
			      | instr                                 {  } /** Здесь ничего не делаем */
  ;

func_list: | func_definition func_list             {  }
           | func_definition                       {  }
  ;

func_definition: FUN VAR LEFTPARENTHESIS var_list RIGHTPARENTHESIS
                 LEFTBRACKET instrlist RIGHTBRACKET                {  
                                                                     result_list := List.sort compare !result_list;
                                                                     cur_index := 0;
                                                                     List.iter renumerate !result_list;
                                                                     result_list := !tmp_list;
                                                                     result_list := List.rev !result_list;
                                                                     var_set := !var_set @ $4;
                                                                     Hashtbl.add hash_table $2 (!result_list, unique !var_set, $4);
                                                                     tmp_list := [];
                                                                     result_list := [];
                                                                     var_set := []
                                                                   }
  ;

  /** TODO: Не возвращать здесь значения, а засовывать их сразу в глобальный список */

instr :   | SKIP SEMICOLON                                     { result_list := Ast.SKIP ($2) :: !result_list   }
          | VAR ASSIGN expr SEMICOLON                          { result_list := Ast.ASSIGN ($4, $1, $3) :: !result_list; var_set := $1 :: !var_set } 
          | WRITE expr SEMICOLON                               { result_list := Ast.WRITE ($3, $2) :: !result_list }
          | READ VAR SEMICOLON                                 { result_list := Ast.READ ($3, $2) :: !result_list; var_set := $2 :: !var_set }
          | WHILE expr DO LEFTBRACKET instrlist RIGHTBRACKET   { result_list := Ast.WHILE($1, $2, $6 + 1) :: !result_list;
                                                                 result_list := Ast.RIGHTBRACKET($6, $1) :: !result_list }
          | IF expr THEN LEFTBRACKET instrlist RIGHTBRACKET
            ELSE LEFTBRACKET instrlist RIGHTBRACKET            { result_list := Ast.IF ($1, $2, $6 + 1) :: !result_list;
                                      													 result_list := Ast.RIGHTBRACKET($10, $10 + 1) :: !result_list;
                                                                 result_list := Ast.RIGHTBRACKET($6, $10 + 1) :: !result_list }
          | RETURN expr SEMICOLON                              { result_list := Ast.RETURN($3, $2) :: !result_list }                       
  ;


var_list:  /* empty */                                { [] }
		| VAR COMMA var_list                          { $1 :: $3 }
		| VAR                                         { $1 :: [] }
  
expr_list : /* empty */                                { [] }
         |  expr COMMA expr_list                       { $1 :: $3 }
         |  expr                                       { $1 :: [] }
  ;

expr : VAR                                            { Ast.VAR $1 }
     | INT                                            { Ast.NUMBER (int_of_string $1) }
     | VAR LEFTPARENTHESIS expr_list RIGHTPARENTHESIS { Ast.FUNC ($1, $3) }
     | expr PLUS expr                                 { Ast.PLUS ($1, $3) }
     | expr MINUS expr                                { Ast.MINUS ($1, $3) }
     | expr MUL expr                                  { Ast.MUL ($1, $3) }
     | expr DIV expr                                  { Ast.DIV ($1, $3) }
     | expr MODULO expr                               { Ast.MODULO ($1, $3) }
     | expr EQ expr                                   { Ast.EQ ($1, $3) }
     | expr NE expr                                   { Ast.NE ($1, $3) }
     | expr GT expr                                   { Ast.GT ($1, $3) }
     | expr GE expr                                   { Ast.GE ($1, $3) }
     | expr LT expr                                   { Ast.LT ($1, $3) }
     | expr LE expr                                   { Ast.LE ($1, $3) }
     | expr AND expr                                  { Ast.AND ($1, $3) }
     | expr OR expr                                   { Ast.OR ($1, $3) }
  ;