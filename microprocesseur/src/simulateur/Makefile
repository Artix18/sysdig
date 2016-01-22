OCAMLC=ocamlopt
LIBS=graph.cmo netlist_ast.cmo scheduler.cmo netlist_lexer.cmo netlist_parser.cmo netlist.cmo mon_compilateur.cmo main.cmo

all: ast graph scheduler parser_inter parser_compil lexer_compil netlist compile main
	$(OCAMLC) -annot -g -o compil_netlist $(LIBS:.cmo=.cmx)

main:
	$(OCAMLC) -annot -g -c main.ml

lexer:
	ocamllex netlist_lexer.mll

parser:
	menhir --infer --explain -v netlist_parser.mly

ast:
	$(OCAMLC) -annot -g -c netlist_ast.ml

netlist:
	$(OCAMLC) -annot -g -c netlist.ml

graph:
	$(OCAMLC) -annot -g -c graph.ml

scheduler:
	$(OCAMLC) -annot -g -c scheduler.ml
	
parser_compil: parser
	$(OCAMLC) -annot -g -c netlist_parser.ml

parser_inter: parser
	$(OCAMLC) -annot -g -c netlist_parser.mli
	
lexer_compil: lexer
	$(OCAMLC) -annot -g -c netlist_lexer.ml

compile:
	$(OCAMLC) -annot -g -c mon_compilateur.ml

clean:
	rm -f *.cm[iox] *.o *.annot *~ pscala netlist_lexer.ml netlist_parser.ml netlist_parser.mli
	rm -f *.output *.automaton
