# The default search in 0.5.8: failureRateBased with deterministic ties

chocosolver 0.5.8 changes the default search, `PreferSmallerInstances`, from Choco's
`failureRateBased` to the same scoring with the ties broken by the first variable
(`failureRateBasedFirst` in the benchmark). This note records why, the measurements, and the
alternatives that were tried ([#46](https://github.com/gsdlab/chocosolver/issues/46)).

Decision: a model must reliably get its first instance. We accept that enumerating all instances
of two hard test models gets slower.

## The problem

Choco's `Search.failureRateBasedSearch` (FRBA, score type 2 of `FailureBased`) picks the
variable with the highest `(failure rate + recency) / domain size`, and breaks ties with a seeded
`java.util.Random`. Before the first failure every variable has the same failure rate (0.5) and
recency, so all variables with the same domain size tie, and the first decisions are random. The
seed is fixed, so a run is reproducible, but which variables the search starts with depends on the
seed and on the number of variables, not on the model's structure.

On the wiki model [ClaferTools/ClaferMooVizAbstractState](https://wiselab.uwaterloo.ca/modelwiki/ClaferTools/ClaferMooVizAbstractState)
those random first decisions lead into a subtree without instances, which the search never
leaves: no instance after minutes. `minDomLB`, the default up to 0.5.2, which takes the first
variable of the smallest domain, finds one without a single failure.

| Model (`issue-46/`) | `failureRateBased` (0.5.3–0.5.7) | `failureRateBasedFirst` (0.5.8) | `minDomLB` (≤ 0.5.2) |
|---|---|---|---|
| `ClaferMooVizAbstractState5.cfr` (5 products) | no instance, 20 s limit, 1340661 nodes | 18 ms, 44 nodes, 0 fails | 17 ms, 44 nodes, 0 fails |
| `ClaferMooVizAbstractState.cfr` (28 products) | no instance, 20 s limit, 96845 nodes | 109 ms, 159 nodes, 0 fails | 105 ms, 159 nodes, 0 fails |
| `bubbles.cfr` (excerpt, the test `SearchStrategyTest`) | no instance, 10 s limit, 2985578 nodes | 0.3 s | |

The released jars agree: chocosolver 0.5.2 (`minDomLB`) solves the 28-product model in 0.8 s,
0.5.3 to 0.5.7 (`failureRateBased`) reach a 30 s limit. Smaller excerpts than `bubbles.cfr` (fewer
clafers, fewer integer attributes, no `xor` group) are solved by both strategies: whether the random
first decisions go wrong is a matter of luck, which is the problem.

The same issue had a second, unrelated cause: `-n n` searched for one more instance after the
n-th. For `injective.cfr` (28 bubbles mapped one-to-one onto 28 products) the first instance takes
0.4 s, but proving that there is no other one is exponential (see below), so `-n 1` never ended.
That is fixed in `Normal.java`, independently of the search.

## Variants tried

On the issue's models, first instance, each variant set through `ClaferCompiler.intStrategyOverride`
as `StrategyBenchmark` does:

| Variant | ClaferMooViz, 5 products | ClaferMooViz, 28 products |
|---|---|---|
| `minDomLB` | 44 nodes | 159 nodes |
| `failureRateBased` (FRBA, random ties) | limit | limit |
| FRBA, first variable on ties (chosen) | 44 nodes | 159 nodes |
| FRB (score type 1), first variable on ties | 44 nodes | 159 nodes |
| `lastConflict(failureRateBased)` | limit | limit |
| `lastConflict(FRBA, first variable on ties)` | 44 nodes | 159 nodes |

Only the random tie-break matters. FRBA and FRB behave the same here; FRBA was kept, as it was the
default. `lastConflict` does not help the random version.

Proving that the 16-bubble version of `injective.cfr` has no second instance (`-n 2`):

| Strategy | Nodes | Time |
|---|---|---|
| `failureLengthBased` | 8898 | 0.35 s |
| `domOverWDegRef` | 15870 | 0.59 s |
| `failureRateBased` (random ties) | 18653 | 0.7 s |
| `domOverWDeg` | 30388 | 1.1 s |
| FRB, first variable on ties | 50584 | 2.3 s |
| FRBA, first variable on ties (chosen) | 63843 | 2.5 s |
| `conflictHistory` | 95036 | 3.9 s |
| `minDomLB` | 202524 | 4.3 s |

With 28 bubbles, every strategy reaches a 30 s limit; with `failureRateBased` the proof takes
0.05 s with 10 bubbles, 0.27 s with 14 and 0.79 s with 16. Rewriting the pairwise `b1.prod != b2.prod`, which the compiler posts as one `!=` per pair,
into a single `allDifferent` (AC) gave the same node counts, so it was not kept. Most likely the symmetry breaking leaves only the identity (the references
sorted and all different), and its propagators and the `!=` do not detect that together; this was
not investigated further. This remains open: `-n 2`, or the next instance in a tool,
does not end on such a model.

## Benchmark on Clafer's test models

`src/test/benchmark/run.sh` (see its README) on Nov 11, 2026: the 138 positive test models of Clafer
0.5.8 (135 compile for the Choco backend; `mutually_recursive_abstract_clafers`,
`reference_to_unextended_abstract` and `set_and_bag_references` fail at compile time with every
strategy), choco-solver 6.0.2, the three strategies in parallel on one machine (32 cores). Raw results
in `results-2026-11-11/` (`model,strategy,status,instances,ms,nodes,fails,front`).

| Workload | Models done by all | `failureRateBased` | `failureRateBasedFirst` | `minDomLB` |
|---|---|---|---|---|
| A: first 10 instances | 135 | 506 ms, 4342 nodes | 510 ms, 4051 nodes | 500 ms, 3953 nodes |
| B: all instances, up to 100000, 30 s limit | 132 | 17.9 s, 2252156 nodes | 17.7 s, 2251839 nodes | 16.8 s, 2251696 nodes |
| C: full Pareto fronts, 60 s limit | 8 | 34.3 s | 34.9 s | 55.4 s |

No strategy finds a different number of instances or a different Pareto front on a model that all
of them finish. On the models that every strategy finishes in A, `failureRateBasedFirst` explores
7% fewer nodes than `failureRateBased`. The models where they differ, all in B and C:

| Model | `failureRateBased` | `failureRateBasedFirst` | `minDomLB` |
|---|---|---|---|
| `enforcingInverseReferences` (B) | 100000 in 24.7 s | limit: 65438 in 30 s | limit: 28229 in 30 s |
| `nested_quantifiers_with_membership` (B) | 100000 in 23.7 s | 100000 in 26.4 s | limit: 47067 in 30 s |
| `objectivesMinMax` (B) | limit: 515 | limit: 515 | limit: 492 |
| `objectivesMinMax` (C) | 538 optimal in 34.1 s, 652 nodes | 538 optimal in 34.6 s, 652 nodes | 538 optimal in 55.2 s, 17078 nodes |

So the deterministic ties keep most of what `failureRateBased` gained over `minDomLB` in 0.5.3,
except on `enforcingInverseReferences`, where it enumerates 35% fewer instances in 30 s.
Times are from parallel runs and only comparable within this run; nodes and fails are
deterministic for all three strategies.

## Considerations

* First instance versus enumeration. Most uses (the IDE, the configurator, `-n 1`, `--repl`) need
  the first instances fast and rarely enumerate everything. A search that may never find the first
  instance of a satisfiable model is worse than one that enumerates a little more slowly.
* Determinism. Both versions are reproducible, but with random ties the outcome changes with
  unrelated parts of the model (more or fewer variables move the random choices). With the first
  variable, the order follows the model and the compiler's variable order, as with `minDomLB`.
* Restarts. Randomized search usually goes with restarts, which would let it leave a bad subtree.
  But the instance enumeration does not restart (only `--search Random` does, with Luby restarts),
  and restarts revisit instances, as `activityBased` shows in the benchmark. Adding restarts and
  nogoods to the default search is a larger change, not done here.
* Alternatives with better proofs (`failureLengthBased`, `domOverWDegRef`) also break their ties at
  random in Choco. They would need the same change and a full benchmark before becoming the
  default.

## Reproducing

```
src/test/benchmark/run.sh <clafer>/test/positive failureRateBased failureRateBasedFirst minDomLB
```

The models of the issue are in `issue-46/`; compile them with `clafer -k -m choco` and run
`java -jar chocosolver.jar --file <model>.js -n 1`, or the benchmark's `StrategyBenchmark` with a
strategy name.
