.PHONY: default build-prod build install uninstall test clean utop coverage doc

default: build

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

utop:
	dune utop lib

doc:
	dune build @doc
	firefox _build/default/_doc/_html/index.html

coverage : clean
	BISECT_ENABLE=yes dune build
	dune runtest --no-buffer -j 1
	bisect-ppx-report summary
	@echo See _coverage/index.html