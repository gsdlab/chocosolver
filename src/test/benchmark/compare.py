"""Compares the results of run.sh for two versions, e.g., of choco-solver.

Usage: python3 src/test/benchmark/compare.py <old results> <new results>

For each workload and strategy, over the models both versions finished: total
time, the geometric mean ratios new/old of nodes and time, how many models have
the same number of nodes, and wrong results: instance counts (workloads B and
C) or Pareto fronts (C) that differ. Lists the models that finish in only one
version, or whose time changes over 2x (at least 1 s).
"""
import csv
import glob
import math
import os
import sys
from collections import defaultdict


def load(results):
    rows = defaultdict(dict)
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


def main(old_dir, new_dir):
    old, new = load(old_dir), load(new_dir)
    details = []
    for workload in sorted({w for w, _ in old}):
        print(f'\n=== Workload {workload}')
        print(f'{"strategy":20} {"models":>6} {"both":>5} {"old+":>4} {"new+":>4} | {"old ms":>8} {"new ms":>8} '
              f'| {"nodes n/o":>9} {"ms n/o":>7} | {"same nodes":>10} | wrong')
        for s in sorted(s for w, s in old if w == workload):
            o, n = old[(workload, s)], new.get((workload, s), {})
            both = [m for m in o if o[m]['status'] == 'done' and n.get(m, {}).get('status') == 'done']
            only_old = [m for m in o if o[m]['status'] == 'done' and n.get(m, {}).get('status') != 'done']
            only_new = [m for m in n if n[m]['status'] == 'done' and o.get(m, {}).get('status') != 'done']
            wrong = []
            for m in both:
                if workload in ('B', 'C') and o[m]['inst'] != n[m]['inst']:
                    wrong.append(f'{m}: {n[m]["inst"]} instances, old {o[m]["inst"]}')
                elif workload == 'C' and o[m]['front'] != n[m]['front']:
                    wrong.append(f'{m}: different Pareto front')
            same = sum(o[m]['nodes'] == n[m]['nodes'] for m in both)
            print(f'{s:20} {len(o):6} {len(both):5} {len(only_old):4} {len(only_new):4} | '
                  f'{sum(o[m]["ms"] for m in both):8} {sum(n[m]["ms"] for m in both):8} | '
                  f'{geomean([(n[m]["nodes"] + 1) / (o[m]["nodes"] + 1) for m in both]):9.2f} '
                  f'{geomean([(n[m]["ms"] + 1) / (o[m]["ms"] + 1) for m in both]):7.2f} | '
                  f'{same:10} | {len(wrong)}')
            for w in wrong:
                print('    WRONG', w)
            for m in only_old + only_new:
                details.append(f'{workload} {s:18} {m}: old {o.get(m, {}).get("status")} '
                               f'{o.get(m, {}).get("inst")} in {o.get(m, {}).get("ms")} ms, '
                               f'new {n.get(m, {}).get("status")} {n.get(m, {}).get("inst")} '
                               f'in {n.get(m, {}).get("ms")} ms')
            # Big time differences on models taking at least 1 s in either version.
            for m in both:
                a, b = o[m]['ms'], n[m]['ms']
                if max(a, b) >= 1000 and (b > 2 * a or a > 2 * b):
                    details.append(f'{workload} {s:18} {m}: {a} ms -> {b} ms, '
                                   f'nodes {o[m]["nodes"]} -> {n[m]["nodes"]}')
    print('\n=== Models that finish in only one version, or whose time changes over 2x (>= 1 s)')
    for d in details:
        print(d)


if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
