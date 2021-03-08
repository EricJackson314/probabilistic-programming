MODULES=ast eval types parser lexer main
OBJECTS=$(MODULES:=.cmo)
OCAMLBUILD = ocamlbuild -use-ocamlfind

default: build
	utop

build: clean
	$(OCAMLBUILD) $(OBJECTS)

clean:
	ocamlbuild -clean