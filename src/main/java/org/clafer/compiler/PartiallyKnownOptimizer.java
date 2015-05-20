package org.clafer.compiler;

import org.chocosolver.solver.Solver;
import org.clafer.common.Check;
import org.clafer.instance.InstanceModel;

/**
 * An optimizer where at least one objective is known to be optimal.
 *
 * For example, if there are two objectives but one objective is always equal to
 * one, then only need to optimize the second objective, therefore reducing a
 * bi-objective problem to a single-objective problem.
 *
 * @author jimmy
 */
public class PartiallyKnownOptimizer implements ClaferOptimizer {

    private final ClaferOptimizer optimizer;
    private final Integer[] scores;

    public PartiallyKnownOptimizer(ClaferOptimizer optimizer, Integer[] scores) {
        this.optimizer = Check.notNull(optimizer);
        this.scores = Check.notNull(scores);
    }

    @Override
    public boolean find() throws ReachedLimitException {
        return optimizer.find();
    }

    @Override
    public InstanceModel instance() {
        return optimizer.instance();
    }

    @Override
    public InstanceModel[] allInstances() throws ReachedLimitException {
        return optimizer.allInstances();
    }

    @Override
    public int[] optimalValues() {
        int[] variableOptimalValues = optimizer.optimalValues();
        int[] optimalValues = new int[scores.length];
        int j = 0;
        for (int i = 0; i < optimalValues.length; i++) {
            optimalValues[i] = scores[i] == null ? variableOptimalValues[j++] : scores[i];
        }
        assert j == variableOptimalValues.length;
        return optimalValues;
    }

    @Override
    public int instanceCount() {
        return optimizer.instanceCount();
    }

    @Override
    public Solver getInternalSolver() {
        return optimizer.getInternalSolver();
    }
}
