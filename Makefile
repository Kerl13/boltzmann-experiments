builder:
	dune build src/bin/binsize.exe
	dune build src/bin/bingen.exe
	dune build src/bin/bool.exe
	dune build src/bin/geom.exe
	dune build src/bin/bernoulli.exe

bingen:
	dune exec src/bin/bingen.exe | column -ts :

binsize:
	dune exec src/bin/binsize.exe | column -ts :

bool:
	dune exec src/bin/bool.exe | column -ts :

geom:
	dune exec src/bin/geom.exe -- 10_000_000 0.01 | column -ts :
	dune exec src/bin/geom.exe -- 10_000_000 0.1  | column -ts :
	dune exec src/bin/geom.exe -- 10_000_000 0.5  | column -ts :
	dune exec src/bin/geom.exe -- 10_000_000 0.9  | column -ts :
	dune exec src/bin/geom.exe -- 10_000_000 0.99 | column -ts :

bernoulli:
	dune exec src/bin/bernoulli.exe -- 1_000_000 0.01 | column -ts :
	dune exec src/bin/bernoulli.exe -- 1_000_000 0.1  | column -ts :
	dune exec src/bin/bernoulli.exe -- 1_000_000 0.25 | column -ts :
	dune exec src/bin/bernoulli.exe -- 1_000_000 0.333333333333333315 | column -ts :
	dune exec src/bin/bernoulli.exe -- 1_000_000 0.5  | column -ts :
	dune exec src/bin/bernoulli.exe -- 1_000_000 0.9  | column -ts :
	dune exec src/bin/bernoulli.exe -- 1_000_000 0.99 | column -ts :
