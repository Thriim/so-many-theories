
SRC=ast.mli ast.ml equality_ast.mli equality_ast.ml parser.mly lexer.mll union_find.ml equality.ml sat.mli sat.ml main.ml 

EXEC=so-many-theories

CAMLC = ocamlc
CAMLOPT = ocamlopt 
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = ocamlyacc


SRC_MLL = $(filter %.mll, $(SRC))
SRC_MLY = $(filter %.mly, $(SRC))
SMLIY = $(SRC:.mly=.ml)
SMLIYL = $(SMLIY:.mll=.ml)
SMLYL = $(filter %.ml,$(SMLIYL))
OBJS = $(SMLYL:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)

all: depend $(EXEC)

opt: depend $(EXEC).opt

report: report.tex
	rubber -d report.tex

$(EXEC): $(OBJS)
	$(CAMLC) -o $@ $^

$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) -o $@ $^

.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

.mll.cmo:
	$(CAMLLEX) $<
	$(CAMLC) -c $*.ml

.mll.cmx:
	$(CAMLLEX) $<
	$(CAMLOPT) -c $*.ml

.mly.cmo:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli
	$(CAMLC) -c $*.ml

.mly.cmx:
	$(CAMLYACC) $<
	$(CAMLOPT) -c $*.mli
	$(CAMLOPT) -c $*.ml

.mly.cmi:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<

clean :
	rm -f *.cm[ioxt] *.cmti depend *.o
	rm -f $(SRC_MLL:.mll=.ml) $(SRC_MLY:.mly=.ml) $(SRC_MLY:.mly=.mli)
	rm -f $(EXEC)
	rm -f $(EXEC).opt

depend: $(SMLIY)
	$(CAMLDEP) $(SMLIY) $(SMLIY:.mly:.mli) > depend

-include depend
