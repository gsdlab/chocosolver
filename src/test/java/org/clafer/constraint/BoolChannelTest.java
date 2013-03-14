package org.clafer.constraint;

import choco.Choco;
import choco.cp.model.CPModel;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import org.clafer.Util;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class BoolChannelTest extends ConstraintTest {

    private static void checkCorrectness(Solver solver, IntegerVariable[] b, SetVariable s) {
        int[] $b = Util.getVals(solver.getVar(b));
        int[] $s = solver.getVar(s).getValue();

        for (int i = 0; i < b.length; i++) {
            assertEquals($b[i] == 1, Util.in(i, $s));
        }
    }

    @Test(timeout=60000)
    public void testBoolChannel() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Model m = new CPModel();

            IntegerVariable[] b = Choco.makeBooleanVarArray("i", nextInt(100) + 1);
            SetVariable s = Choco.makeSetVar("s", -nextInt(100), nextInt(100));

            m.addConstraint(BoolChannelManager.boolChannel(b, s));

            for (int restart = 0; restart < 10; restart++) {
                Solver solver = solveRandomly(m);
                checkCorrectness(solver, b, s);
            }
        }
    }

    @Test(timeout=60000)
    public void quickTest() {
        Model m = new CPModel();

        IntegerVariable[] tuple = Choco.makeBooleanVarArray("bools", 4);
        SetVariable set = Choco.makeSetVar("set", 0, 3);

        m.addConstraint(BoolChannelManager.boolChannel(tuple, set));

        assertEquals(16, quickCheckModel(m, 10));
    }
}
