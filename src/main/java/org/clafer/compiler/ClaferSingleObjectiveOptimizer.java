package org.clafer.compiler;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.clafer.collection.Either;
import org.clafer.common.Check;
import org.clafer.instance.InstanceModel;
import org.chocosolver.solver.ResolutionPolicy;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.ICF;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.objective.ObjectiveManager;
import org.chocosolver.solver.propagation.NoPropagationEngine;
import org.chocosolver.solver.propagation.hardcoded.SevenQueuesPropagatorEngine;
import org.chocosolver.solver.search.loop.monitors.IMonitorSolution;
import org.chocosolver.solver.search.solution.Solution;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;

/**
 *
 * @author jimmy
 */
public class ClaferSingleObjectiveOptimizer implements ClaferOptimizer {

    private final Solver solver;
    private final ClaferSolutionMap solutionMap;
    private final boolean maximize;
    private final Either<Integer, IntVar> score;
    private int count = 0;
    private boolean more = true;
    private int optimalValue;
    private final Solution firstSolution = new Solution();

    ClaferSingleObjectiveOptimizer(Solver solver, ClaferSolutionMap solutionMap,
            boolean maximize, Either<Integer, IntVar> score) {
        this.solver = Check.notNull(solver);
        this.solutionMap = Check.notNull(solutionMap);
        this.maximize = maximize;
        this.score = Check.notNull(score);
    }

    public ClaferSolutionMap getSolutionMap() {
        return solutionMap;
    }

    public boolean isMaximize() {
        return maximize;
    }

    public boolean isMinimize() {
        return !maximize;
    }

    @Override
    public boolean find() {
        if (!more) {
            return false;
        }
        more &= count == 0 ? solveFirst() : solveNext();
        if (more) {
            if (count == 0) {
                optimalValue = score.isLeft()
                        ? score.getLeft()
                        : score.getRight().getValue();
            }
            count++;
        }
        return more;
    }

    /*
     * Implementation of multiple optimal search based on discussion here:
     * https://github.com/chocoteam/choco3/issues/121.
     */
    private boolean solveFirst() {
        if (score.isLeft()) {
            return solver.findSolution();
        }
        IntVar scoreVar = score.getRight();
        solver.set(new ObjectiveManager(
                scoreVar,
                maximize ? ResolutionPolicy.MAXIMIZE : ResolutionPolicy.MINIMIZE,
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
        solver.getSearchLoop().launch(false);
        if (!firstSolution.hasBeenFound()) {
            return false;
        }
        try {
            firstSolution.restore();
        } catch (ContradictionException e) {
            // Should never happen because the solution should not be contradictory.
            throw new IllegalStateException(e);
        }
        return true;
    }

    private boolean solveNext() {
        if (score.isLeft() || count > 1) {
            return solver.nextSolution();
        }
        IntVar scoreVar = score.getRight();
        int best = scoreVar.getValue();
        // TODO: forbid the current solution from happening again.
        solver.getEngine().flush();
        solver.getSearchLoop().reset();
        solver.post(ICF.arithm(scoreVar, "=", best));
        boolean next = solver.findSolution();
        return next && duplicateSolution() ? solver.nextSolution() : next;
    }

    private boolean duplicateSolution() {
        for (IntVar var : solutionMap.getIrSolution().getIntVars()) {
            if ((var.getTypeAndKind() & Variable.CSTE) == 0) {
                if (var.getValue() != firstSolution.getIntVal(var)) {
                    return false;
                }
            }
        }
        for (SetVar var : solutionMap.getIrSolution().getSetVars()) {
            if ((var.getTypeAndKind() & Variable.CSTE) == 0) {
                if (!Arrays.equals(var.getValues(), firstSolution.getSetVal(var))) {
                    return false;
                }
            }
        }
        return true;
    }

    @Override
    public InstanceModel instance() {
        if (count == 0 || !more) {
            throw new IllegalStateException("No instances. Did you forget to call find?");
        }
        return solutionMap.getInstance();
    }

    @Override
    public int[] optimalValues() {
        if (count == 0) {
            throw new IllegalStateException("No instances. Did you forget to call find?");
        }
        return new int[]{optimalValue};
    }

    @Override
    public InstanceModel[] allInstances() {
        List<InstanceModel> instances = new ArrayList<>();
        while (find()) {
            instances.add(instance());
        }
        return instances.toArray(new InstanceModel[instances.size()]);
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
