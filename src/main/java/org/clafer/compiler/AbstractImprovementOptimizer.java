package org.clafer.compiler;

import org.chocosolver.solver.Solver;
import org.chocosolver.solver.search.solution.Solution;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.collection.Either;

/**
 *
 * @author jimmy
 */
public abstract class AbstractImprovementOptimizer implements ClaferOptimizer {

    protected final Solver solver;
    protected final ClaferSolutionMap solutionMap;
    protected final boolean[] maximizes;
    protected final Either<Integer, IntVar>[] scores;

    public AbstractImprovementOptimizer(Solver solver, ClaferSolutionMap solutionMap, boolean[] maximizes, Either<Integer, IntVar>[] scores) {
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

    public Either<Integer, IntVar>[] getScores() {
        return scores;
    }
    
    public abstract Solution solution();
}
