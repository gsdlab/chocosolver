# Search strategy benchmark

Compares Choco search strategies on Clafer models. The strategy replaces the default
`minDomLB` search (`PreferSmallerInstances`) in each branching group, through
`ClaferCompiler.intStrategyOverride`. The rest of the compilation is unchanged.

Requires `clafer` on the `PATH`, Maven and Java.

```
src/test/benchmark/run.sh <dir with .cfr models> [strategy...]
python3 src/test/benchmark/analyze.py target/benchmark/results
```

`run.sh` compiles the models to `.js` and runs three workloads:

* A: the first 10 instances,
* B: all instances, up to 100000, which checks that every strategy finds the same number,
* C: the full Pareto front of the models with objectives, which checks that the fronts are the same.

Each model runs in its own JVM, after one untimed warm-up run. Nodes and fails are
deterministic for most strategies; times are for comparison on the same machine only.

The strategies are listed in [StrategyBenchmark.java](../java/org/clafer/compiler/StrategyBenchmark.java).
`activityBased` restarts the search, so it returns duplicate instances and its counts are wrong.
