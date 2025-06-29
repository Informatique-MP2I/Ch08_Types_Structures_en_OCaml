# Makefile - Dates
SRC := Grimaud_Dates
EXE := dates

.PHONY: clean

$(EXE): $(SRC).ml
	ocamlopt -o $@ $<

clean:
	rm *.cmi *.cmo *.cmx $(SRC).o $(EXE)
