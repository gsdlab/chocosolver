#!/bin/bash
# Whole chocosolver runs, cold, on both paths: `chocosolver --file M.cfr -n 1 --noprint` with the Java
# frontend and with --haskell-frontend (clafer, then Rhino reads the .js), RUNS times each. The
# solving is the same on both paths, so the difference is the compilation in a fresh JVM.
# Usage: cli.sh WORK_DIR MODEL... (models relative to WORK_DIR, as run.sh copied them); prints
# MODEL PATH TIMES_MS.
set -u
WORK=$1; shift
RUNS=${RUNS:-3}
CHOCO=$(cd "$(dirname "$0")/../.." && pwd)
J=$CHOCO/solver/target/chocosolver-0.6.0-jar-with-dependencies.jar
ms() { echo $(( $(date +%s%N) / 1000000 )); }
for m in "$@"; do
  for path in java haskell; do
    flag=(); [ $path = haskell ] && flag=(--haskell-frontend)
    times=()
    for ((r = 0; r < RUNS; r++)); do
      t0=$(ms)
      (cd "$WORK/$(dirname "$m")" && timeout 900 java -jar "$J" --file "$(basename "$m")" -n 1 --noprint "${flag[@]}" > /dev/null 2>&1)
      t1=$(ms); times+=($((t1 - t0)))
    done
    printf '%s\t%s\t%s\n' "$m" $path "${times[*]}"
  done
done
