package org.clafer.compiler;

import org.clafer.collection.Either;
import org.clafer.instance.InstanceModel;
import org.chocosolver.solver.ResolutionPolicy;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.objective.ObjectiveManager;
import org.chocosolver.solver.propagation.NoPropagationEngine;
import org.chocosolver.solver.propagation.hardcoded.SevenQueuesPropagatorEngine;
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
            boolean maximize, Either<Integer, IntVar> score) {
        super(solver, solutionMap, new boolean[]{maximize}, new Either[]{score});
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
                int bestObjectiveValue = scores[0].isLeft()
                        ? scores[0].getLeft()
                        : firstSolution.getIntVal(scores[0].getRight());
                throw new ReachedLimitBestKnownException(
                        bestInstance,
                        new int[]{bestObjectiveValue});
            }
            throw new ReachedLimitException();
        }
        if (more) {
            optimalValue = scores[0].isLeft()
                    ? scores[0].getLeft()
                    : firstSolution.getIntVal(scores[0].getRight());
            count++;
        }
        return more;
    }

    /*
     * Implementation of multiple optimal search based on discussion here:
     * https://github.com/chocoteam/choco3/issues/121.
     */
    private boolean solveFirst() {
        if (scores[0].isLeft()) {
            return solver.findSolution();
        }
        IntVar scoreVar = scores[0].getRight();
        solver.set(new ObjectiveManager(
                scoreVar,
                isMaximize() ? ResolutionPolicy.MAXIMIZE : ResolutionPolicy.MINIMIZE,
                true));
        solver.getSearchLoop().plugSearchMonitor(new IMonitorSolution() {
            private static final long serialVersionUID = 1L;

            @Override
            public void onSolution() {
                if (count == 0) {
                    firstSolution.record(solver);
                }
            }
        });
        if (solver.getEngine() == NoPropagationEngine.SINGLETON) {
            solver.set(new SevenQueuesPropagatorEngine(solver));
        }
        if (!solver.getEngine().isInitialized()) {
            solver.getEngine().initialize();
        }
        solver.getSearchLoop().launch(false);
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
