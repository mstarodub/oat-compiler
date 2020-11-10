INCLUDES= util,x86,grading,ll
LIBS = unix,str
SUBMIT := lexer.mll parser.mly frontend.ml team.txt
OTHER := gradedtests.ml studenttests.ml main.ml

HWNAME := hw04
TIMESTAMP := $(shell /bin/date "+%Y-%m-%d-%H:%M:%S")
ZIPNAME := $(HWNAME)-submit($(TIMESTAMP)).zip


all: main.native

.PHONY: test
test: main.native
	./main.native --test

.PHONY:
main.native: $(SUBMIT) $(OTHER)
	ocamlbuild -Is $(INCLUDES) -libs $(LIBS) main.native -use-menhir -pkg num -yaccflag --explain

.PHONY: repl
repl: main.native
	utop -init .ocamlinit -require unix,str,num

zip: $(SUBMIT)
	zip '$(ZIPNAME)' $(SUBMIT)

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -rf output a.out
