{
  open Parser

  let cur_p = ref 0

  let get = Lexing.lexeme
}

let ws = [' ' '\t']
let newline = '\n' | '\r' | '\r' '\n'
let digit = ['0' - '9']
let letter = ['a' - 'z'] | ['A' - 'Z']
let variable = letter(letter|digit|'_')*
let number = (digit)+

rule token = parse
  | ws           { token lexbuf }
  | eof          { EOF }
  | newline      { Lexing.new_line lexbuf; token lexbuf }
  | "+"          { PLUS }
  | "-"          { MINUS }
  | "*"          { MUL }
  | "/"          { DIV }
  | "%"          { MODULO }
  | "=="         { EQ }
  | "!="         { NE }
  | "<="         { LE }
  | "<"          { LT }
  | ">="         { GE }
  | ">"          { GT }
  | "&&"         { AND }
  | "||"         { OR }
  | "skip"       { SKIP }
  | ":="         { ASSIGN }
  | "write"      { WRITE }
  | "read"       { READ }
  | "while"      { cur_p := !cur_p + 1; WHILE (!cur_p) }
  | "do"         { DO }
  | "if"         { cur_p := !cur_p + 1; IF (!cur_p) }
  | "then"       { THEN }
  | "else"       { ELSE }
  | ";"          { cur_p := !cur_p + 1; SEMICOLON (!cur_p) }
  | variable     { VAR (get lexbuf) }
  | digit+       { INT (get lexbuf) }
  | "{"          { LEFTBRACKET }
  | "}"          { cur_p := !cur_p + 1; RIGHTBRACKET (!cur_p) }


  (*(lexbuf.lex_curr_p.pos_lnum)*)