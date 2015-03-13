
SOURCES=untypeast.ml compiler_eq.ml

PACKAGEFLAGS=-package cmdliner -package compiler-libs.common -package unix

all: compiler_eq


compiler_eq: ${SOURCES}
	ocamlfind ocamlopt -linkpkg ${PACKAGEFLAGS} ${SOURCES} -o compiler_eq
