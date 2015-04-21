{
  open Parser

  let get = Lexing.lexeme
}

let ws = [' ' '\t']
let newline = '\n' | '\r' | '\r' '\n'
let digit = ['0' - '9']
let letter = ['a' - 'z'] | ['A' - 'Z']
let variable = letter(letter|digit|'_')*
let number = (digit)+

rule token = parse
  | ws | newline { token lexbuf }
  | eof          { EOF }
  (* | newline   { Lexing.new_line lexbuf; NEWLINE } *)
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
  | "while"      { WHILE }
  | "do"         { DO }
  | "if"         { IF }
  | "then"       { THEN }
  | "else"       { ELSE }
  | ";"          { SEMICOLON }
  | variable     { VAR (get lexbuf) }
  | digit+       { INT (get lexbuf) }
  | "{"          { LEFTBRACKET }
  | "}"          { RIGHTBRACKET }