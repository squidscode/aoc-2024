SRC=$(shell find src -type f | grep \.ml)

all: aoc

.PHONY: test clean

aoc: $(SRC)
	rm -f $@
	dune build
	ln -s ./_build/default/src/main.exe $@

clean:
	dune clean
	rm -f aoc

test:
	@echo "TODO" >&2
