OTHER := gradedtests.ml studenttests.ml main.ml
SUBMIT := backend.ml team.txt

all: main.native

.PHONY: test
test: main.native
	./main.native --test

main.native: $(OTHER) $(SUBMIT)
	ocamlbuild -Is util,x86,ll,grading -libs unix,str main.native -use-menhir -pkg num

main.byte: $(OTHER) $(SUBMIT)
	ocamlbuild -Is util,x86,ll,grading -libs unix,str main.byte -use-menhir -pkg num

.PHONY: utop repl
utop: main.byte
	utop -require unix,str,num

repl: utop

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -rf output a.out
