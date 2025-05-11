all:
	ocamlc -c ast.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c eval.ml 
	ocamlc -c main.ml
	ocamlc ast.cmo lexer.cmo parser.cmo eval.cmo main.cmo

clean:
	rm -f *.cmo *.cmi a.out lexer.ml parser.ml parser.mli