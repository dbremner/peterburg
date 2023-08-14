OCAMLC=ocamlc
OCAMLWARNINGS=-w p
SRCS:=$(wildcard *.ml) $(wildcard *.mli)

all: example format

example: peterburg.cmo example.ml
	$(OCAMLC) $(OCAMLWARNINGS) $^ -o $@

peterburg.cmo: peterburg.ml peterburg.cmi
	$(OCAMLC) $(OCAMLWARNINGS) -c $<

peterburg.cmi: peterburg.mli
	$(OCAMLC) -c $<

format: $(SRCS)
	ocamlformat --inplace $^

clean: 
	rm -f *.cmi *.cmo example