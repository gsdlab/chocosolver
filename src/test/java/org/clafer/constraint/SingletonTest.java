package org.clafer.constraint;

import org.junit.Test;
import choco.Choco;
import choco.cp.model.CPModel;
import choco.kernel.model.Model;
import choco.kernel.solver.Solver;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import static org.junit.Assert.*;

/**
 *
 * @author jimmy
 */
public class SingletonTest extends ConstraintTest {

    void checkCorrectness(Solver solver, IntegerVariable i, SetVariable s) {
        int $i = solver.getVar(i).getVal();
        int[] $s = solver.getVar(s).getValue();

        assertEquals(1, $s.length);
        assertEquals($i, $s[0]);
    }

    @Test(timeout = 60000)
    public void testSingleton() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Model m = new CPModel();

            IntegerVariable i = Choco.makeIntVar("i", -nextInt(1000), nextInt(1000));
            SetVariable s = Choco.makeSetVar("s", -nextInt(1000), nextInt(1000));

            m.addConstraint(SingletonManager.singleton(i, s));

            for (int restart = 0; restart < 10; restart++) {
                Solver solver = solveRandomly(m);
                checkCorrectness(solver, i, s);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        Model m = new CPModel();

        IntegerVariable i = Choco.makeIntVar("i", 0, 100);
        SetVariable s = Choco.makeSetVar("set", 0, 1000);

        m.addConstraint(SingletonManager.singleton(i, s));

        assertEquals(101, quickCheckModel(m, 10));
    }
}
