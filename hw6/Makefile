INCLUDES= util,x86,ll
SUBMIT := solver.ml alias.ml backend.ml dce.ml constprop.ml team.txt

LIBS = unix,str
PKGS = num

HWNAME := hw06
TIMESTAMP := $(shell /bin/date "+%Y-%m-%d-%H:%M:%S")
ZIPNAME := $(HWNAME)-submit($(TIMESTAMP)).zip

all: main.native

.PHONY: test
test: main.native
	./main.native --test

.PHONY: main.native
main.native: $(SUBMIT) ast.ml astlib.ml backend.ml driver.ml main.ml runtime.c
	ocamlbuild -cflags -g -Is $(INCLUDES) -pkg $(PKGS) -libs $(LIBS) main.native -use-menhir -yaccflag --explain

.PHONY: printanalysis.native
printanalysis.native: $(SUBMIT) ast.ml astlib.ml backend.ml driver.ml main.ml runtime.c
	ocamlbuild -Is $(INCLUDES) $(PKGS) -libs $(LIBS) printanalysis.native -use-menhir -yaccflag --explain

zip: $(SUBMIT)
	zip '$(ZIPNAME)' $(SUBMIT)

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -rf output a.out
