.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop lib

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

zip:
	rm -f ms2_code.zip
	zip -r ms2_code.zip . -x@exclude.lst

clean:
	dune clean
	