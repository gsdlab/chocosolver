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
public class TupleToSetTest extends ConstraintTest {

    private void checkCorrectness(IntVar[] tuple, SetVar set) {
        int[] $tuple = PropUtil.getValues(tuple);
        int[] $set = set.getValue();

        assertEquals(new TIntHashSet($tuple), new TIntHashSet($set));
    }

    @Test(timeout = 60000)
    public void testTupleToSet() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();

            int low = nextInt(5) + 1;
            int high = nextInt(5);

            IntVar[] tuple = VariableFactory.enumeratedArray("tuple", low + high, -low - nextInt(10), high + nextInt(10), solver);
            SetVar set = VariableFactory.set("set", Util.range(-low - nextInt(10), high + nextInt(10) + 1), solver);

            solver.post(Constraints.tupleToSet(tuple, set));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(tuple, set);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(tuple, set);
            }
        }
    }

//    @Test(timeout = 60000)
//    public void largeDomainTest() {
//        Model m = new CPModel();
//
//        IntegerVariable[] tuple = Choco.makeIntVarArray("tuple", 5, 0, 1000000);
//        SetVariable set = Choco.makeSetVar("set", 0, 1000000, Options.V_NO_DECISION);
//
//        m.addConstraint(TupleToSetManager.tupleToSet(tuple, set));
//
//        Solver solver = solveOnce(m);
//        checkCorrectness(solver, tuple, set);
//    }
    @Test(timeout = 60000)
    public void quickTest() {
        Solver solver = new Solver();

        IntVar[] ivars = VariableFactory.enumeratedArray("ivar", 3, 0, 5, solver);
        SetVar svar = VariableFactory.set("svar", new int[]{0, 1, 2, 3, 4, 5}, solver);

        solver.post(Constraints.tupleToSet(ivars, svar));

        assertEquals(216, randomizeStrategy(solver).findAllSolutions());
    }
}
