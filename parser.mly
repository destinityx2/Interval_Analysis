%{
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

%right ASSIGN
%left EQ NE GT GE LT LE
%left AND
%left OR
%left PLUS MINUS
%left MUL DIV MODULO

%start instlist

%type <Ast.inst list> instlist

%%

instlist : instlist SEMICOLON statement             { %1 :: %3 }
  ;

statement : SKIP                                    { %1 }
          | VAR ASSIGN expr                         {  }
          | statement SEMICOLON statement           { %1 :: %3 }
          | WRITE expr                              {  }
          | READ VAR                                {  }
          | WHILE expr DO statement                 {  }
          | IF expr THEN primary_statement ELSE primary_statement   {  }
  ;

primary_statement  : SKIP                           { %1 }
                   | VAR ASSIGN expr                {  } /** Нужно как-то научиться здесь давать аттрибут VAR'у */
                   | WRITE expr                     {  }
                   | READ VAR                       {  } /** Здесь тоже самое... */
                   | LEFTBRACKET primary_statement SEMICOLON primary_statement RIGHTBRACKET     { %2 :: %4 } /** Неоднозначная грамматика.. Это ок? */
  ;


expr : VAR                                          { int_of_string $1 }
     | INT                                          { int_of_string $1 }
     | expr PLUS expr                               { $1 + $3 }
     | expr MINUS expr                              { $1 - $3 }
     | expr MUL expr                                { $1 * $3 }
     | expr DIV expr                                { $1 / %3 }
     | expr MODULO expr                             { %1 % %3 }
     | expr EQ expr                                 { %1 == %3 }
     | expr NE expr                                 { %1 != %3 }
     | expr GT expr                                 { %1 > %3 }
     | expr GE expr                                 { %1 >= %3 }
     | expr LT expr                                 { %1 < %3 }
     | expr LE expr                                 { %1 <= %3 }
     | expr AND expr                                { %1 && %3 }
     | expr OR expr                                 { %1 || %3 }
  ;