.PHONY: all clean byte test repl utop

OCB_FLAGS = -tag bin_annot -use-menhir -use-ocamlfind -pkgs oUnit -pkg graphics
OCB = ocamlbuild $(OCB_FLAGS)

all: byte

clean:
	$(OCB) -clean

byte:
	$(OCB) ms.byte grid.byte 

utop: byte
	utop

test:
	$(OCB) test.byte && ./test.byte
