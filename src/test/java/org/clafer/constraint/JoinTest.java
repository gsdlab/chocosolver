package org.clafer.constraint;

import choco.Choco;
import choco.cp.model.CPModel;
import choco.kernel.model.Model;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import gnu.trove.TIntHashSet;
import org.clafer.Util;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class JoinTest extends ConstraintTest {

    private void checkCorrectness(Solver solver, SetVariable take, SetVariable[] children, SetVariable to) {
        int[] $from = solver.getVar(take).getValue();
        int[][] $children = Util.getVals(solver.getVar(children));
        int[] $to = solver.getVar(to).getValue();

        TIntHashSet set = new TIntHashSet();

        for (int f : $from) {
            for (int c : $children[f]) {
                assertTrue(Util.in(c, $to));
                set.add(c);
            }
        }
        assertEquals(set.size(), $to.length);
    }

    @Test
    public void testJoin() {
        for (int rr = 0; rr < 10; rr++) {
            Model m = new CPModel();
            SetVariable take = Choco.makeSetVar("take", 0, nextInt(10));
            SetVariable[] children = Choco.makeSetVarArray("child", nextInt(10), 0, nextInt(10));
            SetVariable to = Choco.makeSetVar("to", 0, nextInt(10));

            m.addConstraint(JoinManager.join(take, children, to));

            for (int repeat = 0; repeat < 10; repeat++) {
                Solver s = solveRandomly(m, false);
                checkCorrectness(s, take, children, to);
            }
        }
    }

    @Test
    public void quickCheck() {
        Model m = new CPModel();
        SetVariable take = Choco.makeSetVar("take", 0, 2);
        SetVariable[] children = Choco.makeSetVarArray("child", 3, 0, 4);
        SetVariable to = Choco.makeSetVar("to", 0, 4);

        m.addConstraint(JoinManager.join(take, children, to));

        assertEquals(8192, quickCheckModel(m, 10, false));
    }
}
