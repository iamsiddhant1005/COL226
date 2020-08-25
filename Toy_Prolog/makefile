ALL:	
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlc -c tree.ml
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c prolog.ml
	ocamlc -o a tree.cmo lexer.cmo parser.cmo prolog.cmo