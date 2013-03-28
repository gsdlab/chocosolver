package org.clafer.constraint;

import java.util.HashSet;
import java.util.Random;
import java.util.Set;
import solver.Solver;
import solver.search.strategy.IntStrategyFactory;
import solver.variables.VariableFactory;

/**
 *
 * @author jimmy
 */
public abstract class ConstraintTest {

    private final Random rand = new Random();

    public int nextInt(int n) {
        return rand.nextInt(n);
    }

    public Set<String> solveAll(Solver solver) {
        Set<String> solutions = new HashSet<String>();
        if (solver.findSolution()) {
            do {
                if (!solutions.add(solver.toString())) {
                    throw new AssertionError();
                }
            } while (solver.nextSolution());
        }
        return solutions;
    }

    public long quickCheckModel(Solver solver) {
        solver.set(IntStrategyFactory.random(VariableFactory.castToIntVar(solver.getVars()), System.nanoTime()));
        return solver.findAllSolutions();
    }
}
