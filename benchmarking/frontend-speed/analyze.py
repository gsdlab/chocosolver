#!/usr/bin/env python3
"""The tables of the compile-time benchmark (run.sh), in Markdown.

Usage: analyze.py RESULTS_DIR WORK_DIR. WORK_DIR, the copies of the models, gives their sizes
(lines); the model names are relative to it, as run.sh writes them.
"""
import os
import statistics
import sys

results = sys.argv[1]
work = sys.argv[2] if len(sys.argv) > 2 else None


def rows(name):
    with open(os.path.join(results, name)) as f:
        return [line.rstrip('\n').split('\t') for line in f if line.strip()]


clafer = {}
for m, status, times in rows('clafer.tsv'):
    ts = [int(t) for t in times.split()]
    clafer[m] = (status, statistics.median(ts) if ts else None)
java = {r[0]: (r[1], float(r[2]), float(r[3]), None if r[4] in ('-', 'error') else float(r[4]))
        for r in rows('java.tsv')}
cold = {r[0]: (int(r[1]), float(r[2])) for r in rows('cold.tsv')}


def name(m):
    return os.path.relpath(m, work) if work and os.path.isabs(m) else m


def lines(m):
    with open(os.path.join(work, m) if work else m, encoding='latin-1') as f:
        return sum(1 for line in f if line.strip())


def suite(m):
    return name(m).split(os.sep)[0]


def fmt(x):
    if x is None:
        return '–'
    return f'{x:,.0f}' if x >= 100 else f'{x:.1f}'


def ratio(c, j):
    return f'{c / j:,.0f}×' if c / j >= 10 else f'{c / j:.1f}×'


models = sorted(java)
both = [m for m in models if java[m][0] == 'ok' and clafer[m][0] == 'ok']

print('### Summary by suite\n')
print('| Suite | Models | Both compile | Haskell, median (ms) | Java warm, median (ms) | Ratio of the medians'
      ' | Haskell, total (s) | Java warm, total (s) | Java in a cold JVM, median (ms) | Cold JVM run, median (ms) |')
print('|---|---|---|---|---|---|---|---|---|---|')
for s in sorted({suite(m) for m in models}) + ['all']:
    ms = [m for m in models if s == 'all' or suite(m) == s]
    bs = [m for m in both if m in ms]
    hc = statistics.median(clafer[m][1] for m in bs)
    jw = statistics.median(java[m][1] for m in bs)
    print(f'| {s} | {len(ms)} | {len(bs)} | {fmt(hc)} | {fmt(jw)} | {ratio(hc, jw)} | '
          f'{sum(clafer[m][1] for m in bs) / 1000:.1f} | {sum(java[m][1] for m in bs) / 1000:.2f} | '
          f'{fmt(statistics.median(cold[m][1] for m in bs))} | {fmt(statistics.median(cold[m][0] for m in bs))} |')

print('\n### The largest models\n')
print('| Model | Lines | Haskell (ms) | Java warm (ms) | Ratio | Rhino reads the .js (ms) | Java cold run (ms) |')
print('|---|---|---|---|---|---|---|')
for m in sorted(both, key=lambda m: -lines(m))[:15]:
    print(f'| {name(m)} | {lines(m)} | {fmt(clafer[m][1])} | {fmt(java[m][1])} | '
          f'{ratio(clafer[m][1], java[m][1])} | {fmt(java[m][3])} | {fmt(cold[m][0])} |')

print('\n### The slowest models for the Haskell compiler\n')
print('| Model | Lines | Haskell (ms) | Java warm (ms) | Ratio | Java cold run (ms) |')
print('|---|---|---|---|---|---|')
for m in sorted(both, key=lambda m: -clafer[m][1])[:10]:
    print(f'| {name(m)} | {lines(m)} | {fmt(clafer[m][1])} | {fmt(java[m][1])} | '
          f'{ratio(clafer[m][1], java[m][1])} | {fmt(cold[m][0])} |')

slower = [m for m in both if java[m][1] >= clafer[m][1]]
slower_cold = [m for m in both if cold[m][1] >= clafer[m][1]]
print(f'\nJava warm slower than the Haskell compiler: {len(slower)} models'
      + ''.join(f'\n- {name(m)}: {fmt(java[m][1])} ms against {fmt(clafer[m][1])} ms' for m in slower))
print(f'\nThe Java frontend in a cold JVM (its first run, as in the CLI) slower than the Haskell compiler:'
      f' {len(slower_cold)} of {len(both)} models')
if slower_cold:
    print('- the most: ' + ', '.join(f'{name(m)} ({fmt(cold[m][1])} against {fmt(clafer[m][1])} ms)'
                           for m in sorted(slower_cold, key=lambda m: -cold[m][1] / clafer[m][1])[:5]))
faster_cold = [m for m in both if cold[m][1] < clafer[m][1]]
print(f'- faster on {len(faster_cold)}: the models from '
      f'{fmt(min(clafer[m][1] for m in faster_cold))} ms of the Haskell compiler up' if faster_cold else '')
print(f'- a whole cold JVM run (start, frontend, exit): median {fmt(statistics.median(cold[m][0] for m in both))} ms,'
      f' from {fmt(min(cold[m][0] for m in both))} to {fmt(max(cold[m][0] for m in both))} ms')

print('\n### The models that fall back or that either side rejects\n')
print('| Model | Java frontend | Haskell compiler | chocosolver |')
print('|---|---|---|---|')
prefix = None
temporal = []
for m in models:
    js, cs = java[m][0], clafer[m][0]
    if js == 'ok' and cs == 'ok':
        continue
    if 'temporal operators' in cs and 'Chocosolver does not solve temporal models' in js:
        temporal.append(m)
        continue
    if js in ('escape', 'url-import'):
        what = 'falls back'
    elif js == 'ok':
        what = 'Java only (the compiler rejects it)'
    elif cs == 'ok':
        what = 'falls back (Java rejects it)'
    else:
        what = 'rejected'
    short = lambda s: s if len(s) < 120 else s[:117] + '...'
    print(f'| {name(m)} | {short(js).replace("|", "\\|")} | {short(cs).replace("|", "\\|")} | {what} |')
if temporal:
    print(f'| the {len(temporal)} temporal models of {", ".join(sorted({suite(m) for m in temporal}))} | '
          'Chocosolver does not solve temporal models, at the first temporal construct of each line | '
          'no Choco output: the model contains temporal operators | rejected |')

cli = os.path.join(results, 'cli.tsv')
if os.path.exists(cli):
    runs = {}
    for m, path, times in rows('cli.tsv'):
        runs.setdefault(m, {})[path] = statistics.median(int(t) for t in times.split())
    print('\n### Whole chocosolver runs, cold (cli.sh: -n 1 --noprint, the same solving on both paths)\n')
    print('| Model | Lines | Java frontend (ms) | --haskell-frontend (ms) | Saved (ms) |')
    print('|---|---|---|---|---|')
    for m, r in runs.items():
        if 'java' in r and 'haskell' in r:
            print(f'| {name(m)} | {lines(m)} | {fmt(r["java"])} | {fmt(r["haskell"])} | {fmt(r["haskell"] - r["java"])} |')
