package org.clafer.constraint;

import gnu.trove.set.hash.TIntHashSet;
import org.clafer.Util;
import org.clafer.constraint.propagator.PropUtil;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.set.SetConstraintsFactory;
import solver.variables.SetVar;
import solver.variables.VariableFactory;

/**
 *
 * @author jimmy
 */
public class JoinTest extends ConstraintTest {

    private void checkCorrectness(SetVar take, SetVar[] children, SetVar to) {
        int[] $take = take.getValue();
        int[][] $children = PropUtil.getValues(children);
        int[] $to = to.getValue();

        TIntHashSet set = new TIntHashSet();

        for (int t : $take) {
            for (int c : $children[t]) {
                assertTrue(Util.in(c, $to));
                set.add(c);
            }
        }
        assertEquals(set.size(), $to.length);
    }

    @Test(timeout = 60000)
    public void testJoin() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();
            int num = nextInt(20);

            SetVar take = VariableFactory.set("take", Util.range(0, num), solver);
            SetVar[] children = new SetVar[num];
            for (int i = 0; i < children.length; i++) {
                children[i] = VariableFactory.set("child" + i, Util.range(0, nextInt(20) + 1), solver);
            }
            SetVar to = VariableFactory.set("to", Util.range(0, nextInt(20) + 1), solver);

            solver.post(Constraints.join(take, children, to));
            if (num > 1) {
                solver.post(SetConstraintsFactory.all_disjoint(children));
            }

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(take, children, to);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(take, children, to);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        Solver solver = new Solver();

        SetVar take = VariableFactory.set("take", new int[]{0, 1, 2}, solver);
        SetVar[] children = new SetVar[3];
        for (int i = 0; i < children.length; i++) {
            children[i] = VariableFactory.set("child" + i, new int[]{0, 1, 2, 3, 4}, solver);
        }
        SetVar to = VariableFactory.set("to", new int[]{0, 1, 2, 3, 4}, solver);

        solver.post(Constraints.join(take, children, to));
        solver.post(SetConstraintsFactory.all_disjoint(children));

        assertEquals(8192, randomizeStrategy(solver).findAllSolutions());
    }
}
