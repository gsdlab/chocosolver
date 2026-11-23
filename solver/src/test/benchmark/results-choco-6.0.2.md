# choco-solver 6.0.2 vs 4.10.18

The strategy benchmark (see [README.md](README.md)) on Oct 26, 2026, with choco-solver 4.10.18
(chocosolver 0.5.4) and 6.0.2, on the 129 models of `clafer/test/positive` that clafer 0.5.4
compiles for the choco backend, 7 of them with objectives. Both versions ran at the same time
on the same machine (32 cores), each running its strategies in parallel.

```
python3 src/test/benchmark/compare.py <4.10.18 results> <6.0.2 results>
```

## Summary

* No wrong results: every strategy finds the same number of instances (workloads B and C) and
  the same Pareto fronts (C) in both versions.
* The default strategy, `failureRateBased`, explores exactly the same nodes on every model,
  and the times are the same.
* `pickOnDom` and `conflictHistory` search differently and finish models that hit the time
  limit with 4.10.18. `pickOnDom` finds all instances of `enforcingInverseReferences` in 16 s,
  where `failureRateBased` reaches the 30 s limit after about 64000.
* The same 4 models fail in both versions, at compile time: `mutually_recursive_abstract_clafers`
  (AstException), `oclBench_b1` (JoinSetWithStringException), `reference_to_unextended_abstract`
  and `set_and_bag_references` (InsufficientScopeException).
* Only 3 models are hard (`enforcingInverseReferences`, `nested_quantifiers_with_membership`,
  `objectivesMinMax`); on the others all strategies tie. Larger models are needed before
  changing the default strategy.

## Results

`both`: models finished by both versions, `old+`/`new+`: finished only by 4.10.18/6.0.2;
times in ms over the models in `both`; `n/o`: geometric mean ratio 6.0.2/4.10.18;
`same nodes`: models with the same number of nodes.

```
=== Workload A (first 10 instances, 10 s limit)
strategy             models  both old+ new+ |   old ms   new ms | nodes n/o  ms n/o | same nodes | wrong
activityBased           129   124    0    0 |     7242     6994 |      1.00    1.01 |        124 | 0
conflictHistory         129   124    0    1 |     1415     1160 |      0.97    0.96 |         97 | 0
domOverWDeg             129   125    0    0 |     1536     1180 |      0.97    1.00 |        118 | 0
domOverWDegRef          129   125    0    0 |     1119      990 |      0.98    0.99 |        118 | 0
failureLengthBased      129   125    0    0 |     1191     1153 |      1.00    1.03 |        125 | 0
failureRateBased        129   125    0    0 |      980      989 |      1.00    0.99 |        125 | 0
minDomLB                129   125    0    0 |      952     1168 |      1.00    1.02 |        125 | 0
pickOnDom               129   124    0    1 |      859     1019 |      0.98    1.04 |        108 | 0

=== Workload B (all instances up to 100000, 30 s limit)
strategy             models  both old+ new+ |   old ms   new ms | nodes n/o  ms n/o | same nodes | wrong
activityBased           129   116    1    0 |    47756    49043 |      1.00    1.02 |        116 | 0
conflictHistory         129   122    0    0 |    39892    34447 |      0.98    0.98 |        101 | 0
domOverWDeg             129   122    0    0 |    28973    28464 |      0.98    1.04 |        115 | 0
domOverWDegRef          129   122    0    0 |    31543    29637 |      0.99    1.03 |        115 | 0
failureLengthBased      129   124    0    0 |    77568    76990 |      1.00    1.00 |        124 | 0
failureRateBased        129   122    0    0 |    29040    28513 |      1.00    1.00 |        122 | 0
minDomLB                129   122    0    0 |    28123    28500 |      1.00    1.00 |        122 | 0
pickOnDom               129   122    0    2 |    37449    23985 |      0.98    0.91 |        108 | 0

=== Workload C (full Pareto front, 60 s limit)
strategy             models  both old+ new+ |   old ms   new ms | nodes n/o  ms n/o | same nodes | wrong
activityBased             7     5    0    0 |     5868     5562 |      1.00    1.21 |          5 | 0
conflictHistory           7     6    0    1 |      792      825 |      0.97    1.19 |          1 | 0
domOverWDeg               7     7    0    0 |    56949    58159 |      0.76    0.91 |          1 | 0
domOverWDegRef            7     6    0    0 |      540      699 |      0.87    1.53 |          1 | 0
failureLengthBased        7     7    0    0 |    52248    51207 |      1.00    1.05 |          7 | 0
failureRateBased          7     7    0    0 |    45955    54961 |      1.00    1.09 |          7 | 0
minDomLB                  7     6    0    0 |      561      633 |      1.00    1.17 |          6 | 0
pickOnDom                 7     6    0    1 |      542      575 |      0.75    1.20 |          3 | 0

=== Models that finish in only one version, or whose time changes over 2x (>= 1 s)
A conflictHistory    enforcingInverseReferences: old limit 0 in 10000 ms, new done 10 in 131 ms
A pickOnDom          enforcingInverseReferences: old limit 0 in 10000 ms, new done 10 in 110 ms
B activityBased      nested_quantifiers_with_membership: old done 100000 in 29613 ms, new limit 99802 in 30000 ms
B pickOnDom          enforcingInverseReferences: old limit 0 in 30000 ms, new done 100000 in 16424 ms
B pickOnDom          nested_quantifiers_with_membership: old limit 8173 in 30000 ms, new done 100000 in 25351 ms
B pickOnDom          scopes: 7362 ms -> 3283 ms, nodes 245784 -> 200545
C conflictHistory    objectivesMinMax: old limit 313 in 60000 ms, new done 538 in 56889 ms
C pickOnDom          objectivesMinMax: old limit 288 in 60000 ms, new done 538 in 44225 ms
```

The times of the models in workload C are small except for `objectivesMinMax`, so the ratios of
C mostly reflect noise on short runs. With both versions running at the same time, single models
vary by up to about 20%: `failureRateBased` took 45.4 s on `objectivesMinMax` (C) with 4.10.18
and 54.5 s with 6.0.2, and found 97213 vs 87214 instances of `nested_quantifiers_with_membership`
within the limit (B). Rerun alone, one JVM at a time, twice per version, with the same nodes:

| model (workload) | 4.10.18 | 6.0.2 |
|---|---|---|
| `objectivesMinMax` (C) | 29.8 s, 29.0 s | 29.3 s, 28.9 s |
| `nested_quantifiers_with_membership` (B) | 20.8 s, 21.0 s | 21.3 s, 20.5 s |
