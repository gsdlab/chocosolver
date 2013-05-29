package org.clafer.choco.constraint;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import solver.Solver;
import solver.propagation.PropagationEngineFactory;
import solver.search.strategy.IntStrategyFactory;
import solver.search.strategy.strategy.StrategiesSequencer;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;

/**
 *
 * @author jimmy
 */
public abstract class ConstraintTest {

    protected final Random rand = new Random();

    public int nextInt(int n) {
        return rand.nextInt(n);
    }

    public Solver randomizeStrategy(Solver solver) {
        solver.set(PropagationEngineFactory.PROPAGATORDRIVEN.make(solver));
        List<IntVar> intVars = new ArrayList<IntVar>();
        List<SetVar> setVars = new ArrayList<SetVar>();
        for (Variable var : solver.getVars()) {
            if (var instanceof IntVar) {
                intVars.add((IntVar) var);
            } else if (var instanceof SetVar) {
                setVars.add((SetVar) var);
            } else {
                throw new IllegalStateException();
            }
        }
//        if (rand.nextBoolean()) {
        solver.set(
                new StrategiesSequencer(solver.getEnvironment(),
                new RandomSetSearchStrategy(setVars.toArray(new SetVar[setVars.size()])),
                IntStrategyFactory.random(intVars.toArray(new IntVar[intVars.size()]), System.nanoTime())));
//        } else {
//            solver.set(
//                    new StrategiesSequencer(solver.getEnvironment(),
//                    IntStrategyFactory.random(intVars.toArray(new IntVar[intVars.size()]), System.nanoTime()),
//                    new RandomSetSearchStrategy(setVars.toArray(new SetVar[setVars.size()]))));
//        }
        return solver;
    }
}
