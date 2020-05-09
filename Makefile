MODULES=authors oScrabbl state command printer main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
OCAMLBUILDNATIVE=ocamlbuild.native -use-ocamlfind main.native

default: play

build:
	$(OCAMLBUILD) $(OBJECTS)

release: build
	mkdir -p "release"
	$(OCAMLBUILDNATIVE)
	cp _build/main.native release/
	cp -n OScrabbl release/ || true
	cp -i valid_words.txt release/ || true

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

check:
	bash checkenv.sh && bash checktypes.sh

finalcheck: check
	bash checkzip.sh

zip:
	zip OScrabbl.zip *.ml* *.txt *.md _tags .ocamlinit *.sh Makefile -x "_*.ml*"

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package ANSITerminal \
		-html -stars -hide-warnings -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package ANSITerminal \
		-html -stars -hide-warnings -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private OScrabbl.zip
