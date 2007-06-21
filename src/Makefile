GENERATED           := level syntax lexer parser constraint \
		       constraintParser constraintLexer

MODULES             :=						\
	level positions syntax parser lexer			\
	commonPrinter syntacticAnalysis printer			\
	unionFind constraint			\
	constraintParser constraintLexer constraintPrinter	\
	infer mark name solvingConstraint solverEnv unifier     \
	solver interpreter main

EXECUTABLE          := pfiou

OCAMLC := ocamlfind ocamlc -g -package alphaLib
OCAMLOPT := ocamlfind ocamlopt -package alphaLib

all: fake

fake: pfiou

syntax.ml syntax.mli: syntax.mla
	alphaCaml syntax.mla
	$(OCAMLOPT) -c syntax.mli
	ocamldep syntax.mli > syntax.mli.d

syntax.ml.d: syntax.ml	
	ocamldep syntax.ml > syntax.ml.d

syntax.mli.d: syntax.mli
	ocamldep syntax.mli > syntax.mli.d

constraint.ml constraint.mli: constraint.mla
	alphaCaml constraint.mla
	$(OCAMLOPT) -c constraint.mli
	ocamldep constraint.mli > constraint.mli.d

constraint.ml.d: constraint.ml	
	ocamldep constraint.ml > constraint.ml.d

constraint.mli.d: constraint.mli
	ocamldep constraint.mli > constraint.mli.d

parser.mli.d: parser.mly
	ocamldep parser.ml > parser.ml.d
	ocamldep parser.mli > parser.mli.d

constraintParser.mli.d: constraintParser.mly
	ocamldep constraintParser.ml > constraintParser.ml.d
	ocamldep constraintParser.mli > constraintParser.mli.d

include Makefile.shared
include Makefile.auto

OLNKFLAGS += alphaLib.cmx unix.cmxa
BLNKFLAGS += alphaLib.cmo unix.cma

