open Ast
open Parser
open Lexer

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let result = Parser.program Lexer.token lexbuf in
  (*print_string "1";;*)
  print_string (Ast.string_of_expr (List.hd result));;