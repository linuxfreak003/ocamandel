mandel: mandel.ml
	ocamlopt -o mandel mandel.ml

clean:
	rm *.cmi *.cmo *.cmx *.o
	rm mandel
