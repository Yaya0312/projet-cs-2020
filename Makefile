.PHONY: default build-prod build install uninstall test clean utop coverage doc

default: build

start:
	opam install dune
	dune build
	opam install -y --deps-only .

build:
	dune build

build-prod:
	dune build --profile release

test:
	dune runtest -f

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

doc:
	dune build @doc
	xdg-open _build/default/_doc/_html/index.html

coverage : clean
	BISECT_ENABLE=yes dune exec ./test/tests.exe
	dune exec bisect-ppx-report -- html
	xdg-open _coverage/index.html