ALL:	
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c Sheet.ml
	ocamlc -o a str.cma lexer.cmo parser.cmo Sheet.cmo