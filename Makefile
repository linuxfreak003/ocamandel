mandel: mandel.ml
	ocamlopt -o mandel mandel.ml

clean:
	rm *.cmi *.cmx *.o
	rm mandel
