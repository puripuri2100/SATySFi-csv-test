all:
	ocamlc str.cma main.ml -o main
	./main
	satysfi main.saty
