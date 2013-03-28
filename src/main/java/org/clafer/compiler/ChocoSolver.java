package org.clafer.compiler;

import org.clafer.Check;
import org.clafer.instance.InstanceModel;
import solver.Solver;
import solver.search.measure.IMeasures;

/**
 *
 * @author jimmy
 */
public class ChocoSolver {

    private final Solver solver;
    private final SolutionMap solutionMap;
    private boolean first = true;

    ChocoSolver(Solver solver, SolutionMap solutionMap) {
        this.solver = Check.notNull(solver);
        this.solutionMap = Check.notNull(solutionMap);
    }

    public IMeasures getMeasures() {
        return solver.getMeasures();
    }

    public boolean nextSolution() {
        if (first) {
            first = false;
            return solver.findSolution();
        }
        return solver.nextSolution();
    }

    public InstanceModel solution() {
        return solutionMap.getInstance();
    }

    @Override
    public String toString() {
        return solver.toString();
    }
}
