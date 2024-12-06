all: bin/p1

.PHONY: test

bin/%: src/%.ml
	ocamlc $^ -o $@
