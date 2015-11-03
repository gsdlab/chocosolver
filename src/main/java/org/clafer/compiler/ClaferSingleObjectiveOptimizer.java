package org.clafer.compiler;

import org.clafer.instance.InstanceModel;
import org.chocosolver.solver.ResolutionPolicy;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.SolverUtil;
import org.chocosolver.solver.objective.ObjectiveManager;
import org.chocosolver.solver.search.loop.monitors.IMonitorSolution;
import org.chocosolver.solver.search.solution.Solution;
import org.chocosolver.solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
public class ClaferSingleObjectiveOptimizer extends AbstractImprovementOptimizer {

    private int count = 0;
    private boolean more = true;
    private int optimalValue;
    private final Solution firstSolution = new Solution();

    ClaferSingleObjectiveOptimizer(Solver solver, ClaferSolutionMap solutionMap,
            boolean maximize, IntVar score) {
        super(solver, solutionMap, new boolean[]{maximize}, new IntVar[]{score});
    }

    public boolean isMaximize() {
        return maximizes[0];
    }

    public boolean isMinimize() {
        return !isMaximize();
    }

    @Override
    public boolean find() throws ReachedLimitException {
        if (!more || count == 1) {
            more = false;
            return false;
        }
        more &= solveFirst();
        if (solver.hasReachedLimit()) {
            if (firstSolution.hasBeenFound()) {
                InstanceModel bestInstance = solutionMap.getInstance(firstSolution);
                int bestObjectiveValue = firstSolution.getIntVal(scores[0]);
                throw new ReachedLimitBestKnownException(
                        bestInstance,
                        new int[]{bestObjectiveValue});
            }
            throw new ReachedLimitException();
        }
        if (more) {
            optimalValue = firstSolution.getIntVal(scores[0]);
            count++;
        }
        return more;
    }

    /*
     * Implementation of multiple optimal search based on discussion here:
     * https://github.com/chocoteam/choco3/issues/121.
     */
    private boolean solveFirst() {
        IntVar scoreVar = scores[0];
        solver.set(new ObjectiveManager(
                scoreVar,
                isMaximize() ? ResolutionPolicy.MAXIMIZE : ResolutionPolicy.MINIMIZE,
                true));
        solver.plugMonitor((IMonitorSolution) () -> {
            if (count == 0) {
                firstSolution.record(solver);
            }
        });
        SolverUtil.solve(solver);
        return firstSolution.hasBeenFound() && !solver.hasReachedLimit();
    }

    @Override
    public InstanceModel instance() {
        if (count == 0 || !more) {
            throw new IllegalStateException("No instances. Did you forget to call find?");
        }
        return solutionMap.getInstance(firstSolution);
    }

    @Override
    public Solution solution() {
        if (count == 0 || !more) {
            throw new IllegalStateException("No instances. Did you forget to call find?");
        }
        return firstSolution;
    }

    @Override
    public int[] optimalValues() {
        if (count == 0) {
            throw new IllegalStateException("No instances. Did you forget to call find?");
        }
        return new int[]{optimalValue};
    }

    @Override
    public int instanceCount() {
        return count;
    }

    @Override
    public Solver getInternalSolver() {
        return solver;
    }
}
