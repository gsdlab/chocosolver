package org.clafer.constraint;

import gnu.trove.set.hash.TIntHashSet;
import org.clafer.Util;
import org.clafer.constraint.propagator.PropUtil;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VariableFactory;

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

//    @Test(timeout = 60000)
//    public void largeDomainTest() {
//        Model m = new CPModel();
//
//        IntegerVariable[] array = Choco.makeIntVarArray("array", 5, 0, 1000000);
//        SetVariable set = Choco.makeSetVar("set", 0, 1000000, Options.V_NO_DECISION);
//
//        m.addConstraint(arrayToSetManager.arrayToSet(array, set));
//
//        Solver solver = solveOnce(m);
//        checkCorrectness(solver, array, set);
//    }
    @Test(timeout = 60000)
    public void quickTest() {
        Solver solver = new Solver();

        IntVar[] ivars = VariableFactory.enumeratedArray("ivar", 3, 0, 5, solver);
        SetVar svar = VariableFactory.set("svar", new int[]{0, 1, 2, 3, 4, 5}, solver);

        solver.post(Constraints.arrayToSet(ivars, svar));

        assertEquals(216, randomizeStrategy(solver).findAllSolutions());
    }
}
