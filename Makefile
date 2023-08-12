all: example

example: peterburg.cmo example.ml
	ocamlc -w p peterburg.cmo example.ml -o $@

peterburg.cmo: peterburg.cmi peterburg.ml
	ocamlc -w p -c peterburg.ml

peterburg.cmi: peterburg.mli
	ocamlc -c $<

clean: 
	rm -f *.cmi *.cmo example