#!/bin/bash
# Compares Choco search strategies on Clafer models, see README.md.
#
# Usage: src/test/benchmark/run.sh <dir with .cfr models> [strategy...]
#
# Compiles the models with `clafer -m choco` (from PATH), builds the test
# classes, and runs three workloads for each strategy, strategies in parallel:
#   A  first 10 instances                        (10 s limit)
#   B  all instances, up to 100000               (30 s limit)
#   C  full Pareto front of the models with objectives (60 s limit)
# Each model runs in its own JVM, killed after 3x the limit. Results go to
# target/benchmark/results/<workload>.<strategy>.csv; summarize them with
#   python3 src/test/benchmark/analyze.py target/benchmark/results
set -e
cd "$(dirname "$0")/../../.."

if [ $# -lt 1 ] || [ ! -d "$1" ]; then
    echo "Usage: $0 <dir with .cfr models> [strategy...]" >&2
    exit 2
fi
models_src=$1; shift
strategies=("$@")
if [ ${#strategies[@]} -eq 0 ]; then
    strategies=(minDomLB domOverWDeg domOverWDegRef activityBased conflictHistory
                failureRateBased failureLengthBased pickOnDom)
fi

out=target/benchmark
# Start clean, so that models and results of an earlier run do not mix in.
rm -f "$out"/models/* "$out"/results/*
mkdir -p "$out/models" "$out/results"

# Models: compile to .js; models the choco backend does not support are skipped.
cp "$models_src"/*.cfr "$out/models/"
for cfr in "$out"/models/*.cfr; do
    clafer -m choco "$cfr" > /dev/null 2>&1 || true
done
models=("$out"/models/*.js)
optimization=()
for js in "${models[@]}"; do
    if grep -qE '^(max|min)\(' "$js"; then
        optimization+=("$js")
    fi
done
echo "${#models[@]} models, ${#optimization[@]} with objectives"

mvn -B -q test-compile dependency:build-classpath -Dmdep.outputFile="$out/classpath.txt"
classpath=target/classes:target/test-classes:$(cat "$out/classpath.txt")

# Runs one strategy on the models, one JVM per model.
bench() {
    local strategy=$1 workload=$2 n=$3 limit=$4; shift 4
    local csv="$out/results/$workload.$strategy.csv"
    : > "$csv"
    for js in "$@"; do
        row=$(timeout $(( limit * 3 / 1000 + 10 )) java -Xss64m -cp "$classpath" org.clafer.compiler.StrategyBenchmark \
            "$strategy" "$n" "$limit" "$js" 2>/dev/null | grep -v '^model,' | grep -v 'is free' | tail -n 1) || true
        if [ -z "$row" ]; then
            row="$(basename "$js" .js),$strategy,killed,0,0,0,0,"
        fi
        echo "$row" >> "$csv"
    done
}

workload() {
    local name=$1 n=$2 limit=$3; shift 3
    [ $# -gt 0 ] || return 0
    for s in "${strategies[@]}"; do
        bench "$s" "$name" "$n" "$limit" "$@" &
    done
    wait
    echo "workload $name done $(date +%T)"
}

workload A 10 10000 "${models[@]}"
workload B 100000 30000 "${models[@]}"
workload C 100000 60000 "${optimization[@]}"
