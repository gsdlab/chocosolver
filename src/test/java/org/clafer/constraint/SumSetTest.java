package org.clafer.constraint;

import java.util.Arrays;
import choco.Choco;
import choco.cp.model.CPModel;
import choco.kernel.common.logging.ChocoLogging;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import org.clafer.Util;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author jimmy
 */
public class SumSetTest extends ConstraintTest {

    void checkCorrectness(Solver solver, SetVariable set, IntegerVariable sum) {
        int[] $set = solver.getVar(set).getValue();
        int $sum = solver.getVar(sum).getVal();

        assertEquals($sum, Util.sum($set));
    }

    @Test(timeout = 60000)
    public void testSumSetFixedCardinality() {
        Model m = new CPModel();

        SetVariable set = Choco.makeSetVar("set", 0, nextInt(1000));
        IntegerVariable sum = Choco.makeIntVar("sum", 0, nextInt(1000));

        System.out.println(set.pretty());
        System.out.println(sum.pretty());

        m.addConstraint(SumSetManager.sumSet(set, sum));
        int i = set.getUppB() == 0 ? 0 : nextInt(set.getUppB());
        while (i * (i + 1) > 2 * sum.getUppB()) {
            i = nextInt(i);
        }
        m.addConstraint(Choco.eqCard(set, i));

//            for (int restart = 0; restart < 10; restart++) {
        Solver solver = solveRandomly(m);
        System.out.println(Arrays.toString(solver.getVar(set).getValue()));
        System.out.println(solver.getVar(sum).getVal());
        checkCorrectness(solver, set, sum);

//                bc += solver.getBackTrackCount();
//                no += solver.getNodeCount();
//            }
//        }
//        System.out.println(bc);
//        System.out.println(no);
    }

    @Test(timeout = 60000)
    public void testSumSetFixedSum() {
        Model m = new CPModel();

        SetVariable set = Choco.makeSetVar("set", 0, nextInt(1000));
        IntegerVariable sum = Choco.makeIntVar("sum", 0, nextInt(1000));

        m.addConstraint(SumSetManager.sumSet(set, sum));
        m.addConstraint(Choco.eq(sum, sum.getUppB()));

        for (int restart = 0; restart < 10; restart++) {
            Solver solver = solveRandomly(m);
            checkCorrectness(solver, set, sum);
        }
    }

    @Test(timeout = 60000)
    public void testSumSet() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Model m = new CPModel();

            SetVariable set = Choco.makeSetVar("set", 0, nextInt(1000));
            IntegerVariable sum = Choco.makeIntVar("sum", 0, nextInt(1000));

            m.addConstraint(SumSetManager.sumSet(set, sum));

            for (int restart = 0; restart < 10; restart++) {
                Solver solver = solveRandomly(m);
                checkCorrectness(solver, set, sum);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        Model m = new CPModel();

        IntegerVariable sum = Choco.makeIntVar("sum", 0, 50);
        SetVariable set = Choco.makeSetVar("set", 0, 10);

        m.addConstraint(SumSetManager.sumSet(set, sum));

        assertEquals(2034, quickCheckModel(m, 10));
    }
}
