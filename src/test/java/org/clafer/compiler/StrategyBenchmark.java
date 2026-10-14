package org.clafer.compiler;

import java.io.File;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Function;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.search.strategy.Search;
import org.chocosolver.solver.search.strategy.strategy.AbstractStrategy;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.javascript.Javascript;
import org.clafer.javascript.JavascriptFile;
import org.clafer.objective.Objective;
import org.clafer.scope.Scope;

/**
 * Compares Choco search strategies on compiled Clafer models (.js).
 *
 * <pre>
 * java -cp ... org.clafer.compiler.StrategyBenchmark &lt;strategy&gt; &lt;n&gt; &lt;timeLimitMs&gt; &lt;model.js&gt;...
 * </pre>
 *
 * The strategy replaces the default search ({@code minDomLB}, i.e.
 * PreferSmallerInstances) in each branching group. For each model, finds up to
 * n instances (or optimal instances) within the time limit, after one untimed
 * warm-up run, and prints one CSV row:
 * {@code model,strategy,status,instances,ms,nodes,fails,front}, where front
 * lists the distinct objective values of the optimal instances found.
 *
 * @author Michał Antkiewicz
 */
public class StrategyBenchmark {

    static final Map<String, Function<IntVar[], AbstractStrategy<IntVar>>> STRATEGIES = new LinkedHashMap<>();

    static {
        STRATEGIES.put("minDomLB", Search::minDomLBSearch);
        STRATEGIES.put("domOverWDeg", Search::domOverWDegSearch);
        STRATEGIES.put("domOverWDegRef", Search::domOverWDegRefSearch);
        STRATEGIES.put("activityBased", Search::activityBasedSearch);
        STRATEGIES.put("conflictHistory", Search::conflictHistorySearch);
        STRATEGIES.put("failureRateBased", Search::failureRateBasedSearch);
        STRATEGIES.put("failureLengthBased", Search::failureLengthBasedSearch);
        STRATEGIES.put("pickOnDom", Search::pickOnDom);
    }

    public static void main(String[] args) throws Exception {
        if (args.length < 4 || !STRATEGIES.containsKey(args[0])) {
            System.err.println("Usage: StrategyBenchmark <strategy> <n> <timeLimitMs> <model.js>...");
            System.err.println("Strategies: " + STRATEGIES.keySet());
            System.exit(2);
        }
        String strategy = args[0];
        int n = Integer.parseInt(args[1]);
        long timeLimit = Long.parseLong(args[2]);
        ClaferCompiler.intStrategyOverride = STRATEGIES.get(strategy);

        System.out.println("model,strategy,status,instances,ms,nodes,fails,front");
        for (int i = 3; i < args.length; i++) {
            File file = new File(args[i]);
            String name = file.getName().replaceFirst("\\.js$", "");
            String row;
            try {
                run(file, n, timeLimit);
                row = run(file, n, timeLimit);
            } catch (Exception | StackOverflowError e) {
                row = "error,0,0,0,0," + e.getClass().getSimpleName();
            }
            System.out.println(name + "," + strategy + "," + row);
        }
    }

    private static String run(File file, int n, long timeLimit) throws Exception {
        JavascriptFile model = Javascript.readModel(file);
        Objective[] objectives = model.getObjectives();
        // The default integer range of the command line (see Utils.resolveScopes).
        Scope scope = model.getScope().toBuilder().intLow(-128).intHigh(127).toScope();
        ClaferSearch search = objectives.length == 0
                ? ClaferCompiler.compile(model.getModel(), scope, model.getOption())
                : ClaferCompiler.compile(model.getModel(), scope, objectives, model.getOption());
        search.limitTime(timeLimit);
        Solver solver = search.getInternalSolver();

        String status = "done";
        int instances = 0;
        Set<String> front = new TreeSet<>();
        long start = System.nanoTime();
        try {
            while (instances < n && search.find()) {
                instances++;
                if (search instanceof ClaferOptimizer) {
                    front.add(Arrays.toString(((ClaferOptimizer) search).optimalValues()).replace(", ", " "));
                }
            }
            if (solver.isStopCriterionMet()) {
                status = "limit";
            }
        } catch (ReachedLimitException e) {
            status = "limit";
        }
        long ms = (System.nanoTime() - start) / 1_000_000;

        return status + "," + instances + "," + ms + ","
                + solver.getNodeCount() + "," + solver.getFailCount() + "," + String.join(" ", front);
    }
}
