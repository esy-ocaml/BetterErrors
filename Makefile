build:
	# Creating ocamlBetterErrors.opam so that jbuilder builds.
	jbuilder build -j 8

test:
	jbuilder runtest

clean:
	rm *.opam
	jbuilder clean

.PHONY: build release test
