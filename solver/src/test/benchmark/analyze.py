"""Summarizes the results of run.sh.

Usage: python3 src/test/benchmark/analyze.py target/benchmark/results

For each workload and strategy: how many models finished, hit the limit, failed
or were killed; total nodes, fails and time, and the geometric mean ratios to
the minDomLB baseline, over the models every strategy finished. Also reports
wrong results: instance counts (workloads B and C) or Pareto fronts (C) that
differ from the baseline.
"""
import csv
import glob
import math
import os
import sys
from collections import defaultdict

BASE = 'minDomLB'


def load(results):
    rows = defaultdict(dict)  # (workload, strategy) -> model -> row
    for path in sorted(glob.glob(os.path.join(results, '*.csv'))):
        workload, strategy = os.path.basename(path)[:-4].split('.', 1)
        with open(path) as f:
            for r in csv.reader(f):
                if len(r) < 8:
                    continue
                model, _, status, inst, ms, nodes, fails = r[:7]
                rows[(workload, strategy)][model] = dict(
                    status=status, inst=int(inst), ms=int(ms), nodes=int(nodes),
                    fails=int(fails), front=','.join(r[7:]))
    return rows


def geomean(xs):
    xs = [x for x in xs if x > 0]
    return math.exp(sum(map(math.log, xs)) / len(xs)) if xs else float('nan')


def main(results):
    rows = load(results)
    for workload in sorted({w for w, _ in rows}):
        strategies = sorted((s for w, s in rows if w == workload), key=lambda s: (s != BASE, s))
        base = rows.get((workload, BASE), {})
        common = [m for m in base
                  if all(rows[(workload, s)].get(m, {}).get('status') == 'done' for s in strategies)]
        print(f'\n=== Workload {workload}: {len(base)} models, {len(common)} finished by every strategy')
        print(f'{"strategy":20} {"done":>5} {"limit":>5} {"error":>5} {"kill":>4} | {"nodes":>10} {"fails":>10} '
              f'{"ms":>8} | {"nodes/base":>10} {"ms/base":>8} | wrong')
        for s in strategies:
            r = rows[(workload, s)]
            status = defaultdict(int)
            for v in r.values():
                status[v['status']] += 1
            wrong = []
            for m, v in r.items():
                b = base.get(m)
                if not b or v['status'] != 'done' or b['status'] != 'done':
                    continue
                if workload in ('B', 'C') and v['inst'] != b['inst']:
                    wrong.append(f'{m}: {v["inst"]} instances, {BASE} {b["inst"]}')
                elif workload == 'C' and v['front'] != b['front']:
                    wrong.append(f'{m}: different Pareto front')
            print(f'{s:20} {status["done"]:5} {status["limit"]:5} {status["error"]:5} {status["killed"]:4} | '
                  f'{sum(r[m]["nodes"] for m in common):10} {sum(r[m]["fails"] for m in common):10} '
                  f'{sum(r[m]["ms"] for m in common):8} | '
                  f'{geomean([(r[m]["nodes"] + 1) / (base[m]["nodes"] + 1) for m in common]):10.2f} '
                  f'{geomean([(r[m]["ms"] + 1) / (base[m]["ms"] + 1) for m in common]):8.2f} | {len(wrong)}')
            for w in wrong[:5]:
                print('    WRONG', w)
        # The interesting models: those not every strategy finishes.
        for m in base:
            statuses = {s: rows[(workload, s)].get(m, {}) for s in strategies}
            if len({v.get('status') for v in statuses.values()}) > 1:
                print(f'  {m}: ' + ', '.join(
                    f'{s} {v.get("status")} {v.get("inst", 0)} in {v.get("ms", 0)} ms' for s, v in statuses.items()))


if __name__ == '__main__':
    main(sys.argv[1] if len(sys.argv) > 1 else 'target/benchmark/results')
