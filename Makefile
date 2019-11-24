boltzmann:
	dune build src/bin/bench.exe
	_build/default/src/bin/bench.exe | column -ts :

bool:
	dune exec src/bin/benchbool.exe | column -ts :

geom:
	dune exec src/bin/benchgeom.exe -- 10_000_000 0.01 | column -ts :
	dune exec src/bin/benchgeom.exe -- 10_000_000 0.1  | column -ts :
	dune exec src/bin/benchgeom.exe -- 10_000_000 0.5  | column -ts :
	dune exec src/bin/benchgeom.exe -- 10_000_000 0.9  | column -ts :
	dune exec src/bin/benchgeom.exe -- 10_000_000 0.99 | column -ts :

bernoulli:
	dune exec src/bin/bernoulli.exe -- 1_000_000 0.01 | column -ts :
	dune exec src/bin/bernoulli.exe -- 1_000_000 0.1  | column -ts :
	dune exec src/bin/bernoulli.exe -- 1_000_000 0.5  | column -ts :
	dune exec src/bin/bernoulli.exe -- 1_000_000 0.9  | column -ts :
	dune exec src/bin/bernoulli.exe -- 1_000_000 0.99 | column -ts :
