mandel: mandel.ml
	ocamlopt -o mandel mandel.ml

clean:
	rm mandel
	rm *.cmi *.cmx *.o
