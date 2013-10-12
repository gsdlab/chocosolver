package org.clafer.choco.constraint;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import solver.Solver;
import solver.propagation.PropagationEngineFactory;
import solver.search.strategy.IntStrategyFactory;
import solver.search.strategy.strategy.StrategiesSequencer;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;
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

    public IntVar cardVar(SetVar set) {
        return VF.enumerated("|" + set.getName() + "|", 0, set.getEnvelopeSize(), set.getSolver());
    }

    public boolean[] getValues(BoolVar[] vars) {
        boolean[] values = new boolean[vars.length];
        for (int i = 0; i < values.length; i++) {
            values[i] = vars[i].getValue() == 1;
        }
        return values;
    }

    public int[] getValues(IntVar[] vars) {
        int[] values = new int[vars.length];
        for (int i = 0; i < values.length; i++) {
            values[i] = vars[i].getValue();
        }
        return values;
    }

    public int[][] getValues(SetVar[] vars) {
        int[][] values = new int[vars.length][];
        for (int i = 0; i < values.length; i++) {
            values[i] = vars[i].getValue();
        }
        return values;
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
        if (rand.nextBoolean()) {
            solver.set(
                    new StrategiesSequencer(solver.getEnvironment(),
                    new RandomSetSearchStrategy(setVars.toArray(new SetVar[setVars.size()])),
                    IntStrategyFactory.random(intVars.toArray(new IntVar[intVars.size()]), System.nanoTime())));
        } else {
            solver.set(
                    new StrategiesSequencer(solver.getEnvironment(),
                    IntStrategyFactory.random(intVars.toArray(new IntVar[intVars.size()]), System.nanoTime()),
                    new RandomSetSearchStrategy(setVars.toArray(new SetVar[setVars.size()]))));
        }
        return solver;
    }
}
