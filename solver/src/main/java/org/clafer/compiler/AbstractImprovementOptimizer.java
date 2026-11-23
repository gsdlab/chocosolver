package org.clafer.compiler;

import org.chocosolver.solver.Solution;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
public abstract class AbstractImprovementOptimizer implements ClaferOptimizer {

    protected final Solver solver;
    protected final ClaferSolutionMap solutionMap;
    protected final boolean[] maximizes;
    protected final IntVar[] scores;

    public AbstractImprovementOptimizer(Solver solver, ClaferSolutionMap solutionMap, boolean[] maximizes, IntVar[] scores) {
        this.solver = solver;
        this.solutionMap = solutionMap;
        this.maximizes = maximizes;
        this.scores = scores;
    }

    @Override
    public Solver getInternalSolver() {
        return solver;
    }

    public ClaferSolutionMap getSolutionMap() {
        return solutionMap;
    }

    public boolean[] getMaximizes() {
        return maximizes;
    }

    public IntVar[] getScores() {
        return scores;
    }

    public abstract Solution solution();
}
