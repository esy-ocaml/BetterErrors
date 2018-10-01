build:
	# Creating ocamlBetterErrors.opam so that jbuilder builds.
	dune build --profile=release -j 8

install: build
	esy-installer

test:
	jbuilder runtest

clean:
	rm *.opam
	jbuilder clean

.PHONY: build release test
