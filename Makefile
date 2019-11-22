boltzmann:
	dune build src/bin/bench.exe
	_build/default/src/bin/bench.exe | column -ts :

bool:
	dune exec src/bin/benchbool.exe | column -ts :
