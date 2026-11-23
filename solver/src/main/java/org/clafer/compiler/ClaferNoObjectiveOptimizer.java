package org.clafer.compiler;

import org.chocosolver.solver.Solver;
import org.clafer.instance.InstanceModel;

/**
 *
 * @author jimmy
 */
public class ClaferNoObjectiveOptimizer implements ClaferOptimizer {

    private final ClaferSolver solver;

    public ClaferNoObjectiveOptimizer(ClaferSolver solver) {
        this.solver = solver;
    }

    @Override
    public boolean find() {
        return solver.find();
    }

    @Override
    public InstanceModel instance() {
        return solver.instance();
    }

    @Override
    public int[] optimalValues() {
        return new int[0];
    }

    @Override
    public int instanceCount() {
        return solver.instanceCount();
    }

    @Override
    public InstanceModel[] allInstances() throws ReachedLimitException {
        return solver.allInstances();
    }

    @Override
    public Solver getInternalSolver() {
        return solver.getInternalSolver();
    }

    @Override
    public ClaferSearch limitTime(long ms) {
        return solver.limitTime(ms);
    }
}
