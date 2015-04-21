open Ast
open Parser
open Lexer

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let result = Parser.program Lexer.token lexbuf in
  Ast.print_expressions result;;