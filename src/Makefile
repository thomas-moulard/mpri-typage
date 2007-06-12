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

all: pfiou

syntax.ml syntax.mli: syntax.mla
	alphaCaml syntax.mla

constraint.ml constraint.mli: constraint.mla
	alphaCaml constraint.mla

include Makefile.shared
include Makefile.auto

OLNKFLAGS += alphaLib.cmx unix.cmxa
BLNKFLAGS += alphaLib.cmo unix.cma
