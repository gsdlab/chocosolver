#!/bin/bash
# The compile times of the Java frontend and of the Haskell compiler, without solving, on Clafer's
# positive test models, chocosolver's own .cfr models and ClaferCaseStudies (next to chocosolver,
# as in clafer-tools).
#
#   Java, warm:  FrontendBenchmark in one JVM, WARMUPS runs then RUNS timed runs per model
#                (Frontend.readWithJava: what chocosolver solves, from the .cfr file), and Rhino
#                reading the compiler's model.js, the rest of the .js path before solving
#   Java, cold:  a fresh JVM per model, one run: the process's wall time and the frontend's own
#   Haskell:     clafer -k -m choco, as chocosolver runs it, CLAFER_RUNS times per model
#
# Usage: run.sh WORK_DIR RESULTS_DIR. WORK_DIR gets copies of the models (clafer writes model.js
# next to each); RESULTS_DIR gets clafer.tsv, java.tsv, cold.tsv. Then analyze.py RESULTS_DIR.
# Needs clafer on the path and `mvn install` (the jar and the test classes) in chocosolver.
set -u
mkdir -p "$1" "$2"
WORK=$(cd "$1" && pwd)
RESULTS=$(cd "$2" && pwd)
WARMUPS=${WARMUPS:-5}
RUNS=${RUNS:-5}
CLAFER_RUNS=${CLAFER_RUNS:-3}
TIMEOUT=${TIMEOUT:-900}
CHOCO=$(cd "$(dirname "$0")/../.." && pwd)
TOP=$(dirname "$CHOCO")
CP=$CHOCO/solver/target/test-classes:$CHOCO/solver/target/chocosolver-0.6.0-jar-with-dependencies.jar
test -f "$CHOCO/solver/target/test-classes/org/clafer/benchmark/FrontendBenchmark.class" || {
  echo "Run mvn install in $CHOCO first"; exit 2; }
command -v clafer > /dev/null || { echo "clafer is not on the path"; exit 2; }

rm -rf "${WORK:?}"/*
copy() { # copy SRC DEST: the .cfr models of SRC, with their folders
  (cd "$1" && find . -name '*.cfr' ! -name '*.des.cfr' ! -name '*.core.cfr' ! -path '*/target/*' -print0 |
    cpio -0 -pdm --quiet "$2")
}
copy "$TOP/clafer/test/positive" "$WORK/clafer-positive"
copy "$CHOCO" "$WORK/chocosolver"
copy "$TOP/ClaferCaseStudies" "$WORK/ClaferCaseStudies"
# the models, relative to WORK, which the runs go into
cd "$WORK"
find . -name '*.cfr' | sed 's|^\./||' | sort > "$RESULTS/models.txt"
wc -l < "$RESULTS/models.txt"
ms() { echo $(( $(date +%s%N) / 1000000 )); }

# Haskell: status, then each run's milliseconds
: > "$RESULTS/clafer.tsv"
while read -r m; do
  dir=$(dirname "$m"); base=$(basename "$m" .cfr); times=()
  for ((r = 0; r < CLAFER_RUNS; r++)); do
    rm -f "$dir/$base.js"
    t0=$(ms); out=$(cd "$dir" && timeout "$TIMEOUT" clafer -k -m choco "$base.cfr" 2>&1); rc=$?; t1=$(ms)
    times+=($((t1 - t0)))
    if [ $rc = 124 ]; then status=timeout; break
    elif [ $rc != 0 ]; then status="error: $(echo "$out" | grep -v '^\s*$' | head -1)"; break
    elif [ ! -f "$dir/$base.js" ]; then status="no-js: $(echo "$out" | grep -v '^\s*$' | tail -1)"; break
    else status=ok; fi
  done
  printf '%s\t%s\t%s\n' "$m" "${status//$'\t'/ }" "${times[*]}" >> "$RESULTS/clafer.tsv"
done < "$RESULTS/models.txt"

# Java, warm, with the model.js files that clafer wrote
xargs -d '\n' java -cp "$CP" org.clafer.benchmark.FrontendBenchmark "$WARMUPS" "$RUNS" \
  < "$RESULTS/models.txt" > "$RESULTS/java.tsv"

# Java, cold: wall time of a fresh JVM, then the frontend's own time
: > "$RESULTS/cold.tsv"
while read -r m; do
  t0=$(ms); line=$(java -cp "$CP" org.clafer.benchmark.FrontendBenchmark 0 1 "$m"); t1=$(ms)
  printf '%s\t%s\t%s\n' "$m" $((t1 - t0)) "$(echo "$line" | cut -f3)" >> "$RESULTS/cold.tsv"
done < "$RESULTS/models.txt"
echo done
