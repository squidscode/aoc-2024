all: bin/p1

bin/%: src/%.ml
	ocamlc $^ -o $@
