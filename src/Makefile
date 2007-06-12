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

syntax.ml: syntax.mla
	alphaCaml syntax.mla
	$(OCAMLOPT) -c syntax.mli
	
constraint.ml: constraint.mla
	alphaCaml constraint.mla
	$(OCAMLOPT) -c constraint.mli

parser.mli.d: parser.mly
	ocamlfind ocamldep parser.mli

constraintParser.mli.d: constraintParser.mly
	ocamlfind ocamldep constraintParser.mli

include Makefile.shared
include Makefile.auto

OLNKFLAGS += alphaLib.cmx unix.cmxa
BLNKFLAGS += alphaLib.cmo unix.cma

