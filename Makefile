build:
	dune build

search:
	OCAMLRUNPARAM=b dune exec ./backend/interface.exe

play:

	OCAMLRUNPARAM=b dune exec ./frontend/Guione.exe

zip:
	rm -f housing.zip
	zip -r housing.zip . 