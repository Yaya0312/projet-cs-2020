.PHONY: default build-prod build install uninstall test clean utop coverage doc

default: build

deps:
	opam install -y --deps-only .

build:
	dune build

build-prod:
	dune build --profile release

test:
	dune runtest -f

clean:
	dune clean

doc:
	dune build @doc
	cp -r _build/default/_doc/_html ./docs
	xdg-open _build/default/_doc/_html/index.html

coverage : clean
	BISECT_ENABLE=yes dune exec ./test/tests.exe
	dune exec bisect-ppx-report -- html
	xdg-open _coverage/index.html