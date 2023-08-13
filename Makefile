OCAMLC=ocamlc
OCAMLWARNINGS=-w p

all: example

example: peterburg.cmo example.ml
	$(OCAMLC) $(OCAMLWARNINGS) $^ -o $@

peterburg.cmo: peterburg.ml peterburg.cmi
	$(OCAMLC) $(OCAMLWARNINGS) -c $<

peterburg.cmi: peterburg.mli
	$(OCAMLC) -c $<

clean: 
	rm -f *.cmi *.cmo example