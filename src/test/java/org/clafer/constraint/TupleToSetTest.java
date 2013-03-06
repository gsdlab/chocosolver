package org.clafer.constraint;

import choco.Choco;
import choco.Options;
import choco.cp.model.CPModel;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import gnu.trove.TIntHashSet;
import java.util.Random;
import org.clafer.Util;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class TupleToSetTest extends ConstraintTest {

    void checkCorrectness(Solver solver, IntegerVariable[] tuple, SetVariable set) {
        int[] $tuple = Util.getVals(solver.getVar(tuple));
        int[] $set = solver.getVar(set).getValue();

        assertEquals($tuple.length, $set.length);
        assertEquals(new TIntHashSet($tuple), new TIntHashSet($set));
    }

    @Test
    public void testTupleToSet() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Model m = new CPModel();

            int low = nextInt(5) + 1;
            int high = nextInt(5);

            // TODO
            IntegerVariable[] tuple = Choco.makeIntVarArray("tuple", /*1 +*/ low + high, -low - nextInt(10), high + nextInt(10));
            SetVariable set = Choco.makeSetVar("set", -low - nextInt(10), high + nextInt(10));

            m.addConstraint(TupleToSetManager.tupleToSet(tuple, set));

            for (int restart = 0; restart < 10; restart++) {
                Solver solver = solveRandomly(m);
                checkCorrectness(solver, tuple, set);
            }
        }
    }

    @Test
    public void largeDomainTest() {
        Model m = new CPModel();

        IntegerVariable[] tuple = Choco.makeIntVarArray("tuple", 5, 0, 1000000);
        SetVariable set = Choco.makeSetVar("set", 0, 1000000, Options.V_NO_DECISION);

        m.addConstraint(TupleToSetManager.tupleToSet(tuple, set));

        Solver solver = solveOnce(m);
        checkCorrectness(solver, tuple, set);
    }
    
    @Test
    public void quickTest() {
        Model m = new CPModel();

        IntegerVariable[] tuple = Choco.makeIntVarArray("tuple", 3, 0, 5);
        SetVariable set = Choco.makeSetVar("set", 0, 5);

        m.addConstraint(TupleToSetManager.tupleToSet(tuple, set));

        assertEquals(120, quickCheckModel(m, 10));
    }
}
