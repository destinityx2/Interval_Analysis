prog: lexer.cmo parser.cmo main.cmo ast.cmo 
	ocamlc -o prog lexer.cmo parser.cmo ast.cmo main.cmo

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

lexer.ml: lexer.mll parser.cmo
	ocamllex lexer.mll

parser.cmo: parser.ml
	ocamlc -c parser.mli
	ocamlc -c parser.ml

parser.ml: parser.mly ast.cmo
	ocamlyacc parser.mly

ast.cmo: ast.ml
	ocamlc -c ast.ml

main.cmo: main.ml lexer.ml parser.ml ast.ml
	ocamlc -c main.ml

clear:
	rm *.mli *.cmi *.cmo *o *.cmx lexer.ml parser.ml