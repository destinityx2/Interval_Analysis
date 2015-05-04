open Ast
open Parser
open Lexer

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let prog = Parser.program Lexer.token lexbuf in
  Eval.run prog;;
  (*Ast.print_instructions prog;;*)