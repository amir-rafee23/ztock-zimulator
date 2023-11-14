.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

run:
	OCAMLRUNPARAM=b dune exec bin2/tui.exe

zip:
	rm -f ms2_code.zip
	zip -r ms2_code.zip . -x@exclude.lst

clean:
	dune clean
	