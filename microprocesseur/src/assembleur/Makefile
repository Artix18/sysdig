OCAMLC=ocamlopt
LIBS=ast.cmo lexer.cmo parser.cmo  compile.cmo main.cmo

all: ast parser_inter parser_compil lexer_compil compile main
	$(OCAMLC) -annot -g -o assembler $(LIBS:.cmo=.cmx)

main:
	$(OCAMLC) -annot -g -c main.ml

lexer:
	ocamllex lexer.mll

parser:
	menhir --infer --explain -v parser.mly

ast:
	$(OCAMLC) -annot -g -c ast.ml
	
parser_compil: parser
	$(OCAMLC) -annot -g -c parser.ml

parser_inter: parser
	$(OCAMLC) -annot -g -c parser.mli
	
lexer_compil: lexer
	$(OCAMLC) -annot -g -c lexer.ml

compile:
	$(OCAMLC) -annot -g -c compile.ml

clean:
	rm -f *.cm[io] *.o *.annot *~ pscala lexer.ml parser.ml parser.mli
	rm -f *.output *.automaton
