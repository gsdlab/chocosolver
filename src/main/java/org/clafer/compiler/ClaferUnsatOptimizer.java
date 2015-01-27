package org.clafer.compiler;

import org.clafer.instance.InstanceModel;
import org.chocosolver.solver.Solver;

/**
 *
 * @author jimmy
 */
public class ClaferUnsatOptimizer implements ClaferOptimizer {

    private final Solver solver;

    public ClaferUnsatOptimizer() {
        this.solver = new Solver();
        this.solver.post(this.solver.FALSE);
    }

    @Override
    public boolean find() {
        return false;
    }

    @Override
    public int[] optimalValues() {
        throw new IllegalStateException();
    }

    @Override
    public InstanceModel instance() {
        throw new IllegalStateException();
    }

    @Override
    public InstanceModel[] allInstances() {
        return new InstanceModel[0];
    }

    @Override
    public int instanceCount() {
        return 0;
    }

    @Override
    public Solver getInternalSolver() {
        return solver;
    }
}
