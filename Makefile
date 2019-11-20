all:
	dune build src/bin/bench.exe
	_build/default/src/bin/bench.exe | column -ts :
