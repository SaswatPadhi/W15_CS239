.PHONY: dep-only clean

libs-only: escher_components.ml escher_core.ml escher_types.ml escher_synth.ml specInfer.ml
	ocamlc -c escher_types.ml escher_core.ml escher_components.ml escher_synth.ml
	ocamlfind ocamlc -package batteries -c specInfer.ml
	ocamlfind ocamlc -package batteries -c logisticInfer.ml

demoA: libs-only demoA.ml
	ocamlfind ocamlc -package batteries specInfer.cmo demoA.ml -o demoA

clean:
	rm *.cmo *.cmi
