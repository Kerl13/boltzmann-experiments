# Boltzmann experiments

```
dune build src/bin/bench.exe
_build/default/src/bin/bench.exe | column -ts :
name             runtime         slowdown  ns/node
constant space   7.80460906029s  1.000x    24.856
arbogen          8.36654186249s  0.933x    26.645
gadt-stack       12.3843710423s  0.630x    39.441
generic          12.6424260139s  0.617x    40.263
stack-based      13.8135771751s  0.565x    43.993
constant space'  14.5778870583s  0.535x    46.427
arbogen'         15.1311049461s  0.516x    48.189
gadt-stack'      18.9064228535s  0.413x    60.213
generic'         18.9149041176s  0.413x    60.240
stack-based'     20.3235621452s  0.384x    64.726
```
