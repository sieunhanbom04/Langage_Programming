CMO = location.cmo x86_64.cmo lexer.cmo parser.cmo tcstatic.cmo tcresource.cmo  compile.cmo prustc.cmo
GENERATED = lexer.ml parser.ml parser.mli 
BIN = prustc
FLAGS = -dtypes

all:$(BIN)
	./$(BIN)

$(BIN):$(CMO)
	ocamlc $(FLAGS) -o $(BIN) $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

.mli.cmi:
	ocamlc $(FLAGS) -c  $<

.ml.cmo:
	ocamlc $(FLAGS) -c  $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir --infer -v $<

.mly.mli:
	menhir -v $<

	ocamlc -dtypes -o location.cmo location.ml

clean: 
	rm -f *.cm[io] *.o *.annot *~ $(BIN) $(GENERATED) test.s
	rm -f parser.automaton
	rm -f parser.conflicts
.depend depend: $(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend
parser.ml: ast.cmi
compile.ml: ast_code.cmi
ast.cmi: location.cmo
