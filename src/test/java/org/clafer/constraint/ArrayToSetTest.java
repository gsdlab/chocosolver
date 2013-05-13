package org.clafer.constraint;

import gnu.trove.set.hash.TIntHashSet;
import org.clafer.Util;
import org.clafer.constraint.propagator.PropUtil;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.constraints.set.SetConstraintsFactory;
import solver.exception.ContradictionException;
import solver.search.loop.monitors.SearchMonitorFactory;
import solver.search.strategy.SetStrategyFactory;
import solver.variables.EventType;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;
import solver.variables.VariableFactory;
import util.ESat;

/**
 *
 * @author jimmy
 */
public class ArrayToSetTest extends ConstraintTest {

    private void checkCorrectness(IntVar[] array, SetVar set) {
        int[] $array = PropUtil.getValues(array);
        int[] $set = set.getValue();

        assertEquals(new TIntHashSet($array), new TIntHashSet($set));
    }

    @Test(timeout = 60000)
    public void testArrayToSet() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            int low = nextInt(5) + 1;
            int high = nextInt(5);

            IntVar[] array = VariableFactory.enumeratedArray("array", low + high, -low - nextInt(10), high + nextInt(10), solver);
            SetVar set = VariableFactory.set("set", Util.range(-low - nextInt(10), high + nextInt(10) + 1), solver);

            solver.post(Constraints.arrayToSet(array, set));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(array, set);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(array, set);
            }
        }
    }

    @Test(timeout = 60000)
    public void testLargeDomain() {
        Solver solver = new Solver();

        IntVar[] array = VariableFactory.enumeratedArray("array", 5, 0, 100000, solver);
        SetVar set = VariableFactory.set("set", Util.range(0, 100001), solver);

        solver.post(Constraints.arrayToSet(array, set));

        assertTrue(randomizeStrategy(solver).findSolution());
        checkCorrectness(array, set);
    }

    @Test(timeout = 60000)
    public void quickTest() {
        Solver solver = new Solver();

        IntVar[] ivars = VariableFactory.enumeratedArray("ivar", 3, 0, 5, solver);
        SetVar svar = VariableFactory.set("svar", new int[]{0, 1, 2, 3, 4, 5}, solver);

        solver.post(Constraints.arrayToSet(ivars, svar));

        assertEquals(216, randomizeStrategy(solver).findAllSolutions());
    }

    public static void main(String[] args) {
        Solver solver = new Solver();

        SetVar svar1 = VariableFactory.set("svar1", new int[]{0, 1, 2, 3, 4, 5}, new int[]{3, 4}, solver);
        SetVar svar2 = VariableFactory.set("svar2", new int[]{0, 1, 2, 3, 4, 5}, new int[]{3, 4}, solver);
        SetVar svar3 = VariableFactory.set("svar3", new int[]{0, 1, 2, 3, 4, 5}, new int[]{5}, solver);

        Constraint con = new Constraint(new Variable[]{svar1, svar2}, solver);
        Dud dud = new Dud(svar1, svar2);
        con.setPropagators(dud);
        solver.post(SetConstraintsFactory.union(new SetVar[]{svar1, svar2}, svar3));
        solver.post(con);
        // svar3 is not a decision variable
        solver.set(SetStrategyFactory.setLex(new SetVar[]{svar1, svar2}));

        // Should say no solution because svar1 = svar2 = {3, 4} and svar3 contains 5 hence
        // svar3 cannot be the union of svar1 and svar2
        System.out.println(solver.findSolution() ? "Found solution" : "No solution");
        System.out.println(solver);
    }

    /**
     * Instantiates the variables to {3, 4}
     */
    private static class Dud extends Propagator<SetVar> {

        public Dud(SetVar... vars) {
            super(vars, PropagatorPriority.UNARY);
        }

        @Override
        public int getPropagationConditions(int vIdx) {
            return EventType.ALL_FINE_EVENTS.strengthened_mask;
        }

        @Override
        public void propagate(int evtmask) throws ContradictionException {
            System.out.println("propagate : " + evtmask);

            for (SetVar var : vars) {
                var.instantiateTo(new int[]{3, 4}, aCause);
            }
        }

        @Override
        public void propagate(int idxVarInProp, int mask) throws ContradictionException {
            System.out.println("propagateVar : " + idxVarInProp + " : " + mask);
        }

        @Override
        public ESat isEntailed() {
            return ESat.TRUE;
        }
    }
}
