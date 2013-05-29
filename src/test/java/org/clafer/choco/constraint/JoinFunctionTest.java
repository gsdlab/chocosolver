package org.clafer.choco.constraint;

import gnu.trove.set.hash.TIntHashSet;
import org.clafer.common.Util;
import org.clafer.choco.constraint.propagator.PropUtil;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class JoinFunctionTest extends ConstraintTest {

    private void checkCorrectness(SetVar take, IntVar[] refs, SetVar to) {
        int[] $take = take.getValue();
        int[] $refs = PropUtil.getValues(refs);
        int[] $to = to.getValue();

        TIntHashSet set = new TIntHashSet();

        for (int f : $take) {
            assertTrue(Util.in($refs[f], $to));
            set.add($refs[f]);
        }
        assertEquals(set.size(), $to.length);
    }

    @Test()
    public void quickTest() throws Throwable {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();
            int num = nextInt(10);

            SetVar take = VF.set("take", Util.fromTo(0, num), solver);
            IntVar[] refs = new IntVar[num];
            for (int i = 0; i < refs.length; i++) {
                refs[i] = VF.enumerated("ref" + i, 0, nextInt(10), solver);
            }
            SetVar to = VF.set("to", Util.fromTo(0, nextInt(10)), solver);

            solver.post(Constraints.joinFunction(take, refs, to));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(take, refs, to);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(take, refs, to);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTestSingleValue() {
        Solver solver = new Solver();

        SetVar take = VF.set("take", Util.fromTo(0, 2), solver);
        IntVar[] refs = new IntVar[2];
        for (int i = 0; i < refs.length; i++) {
            refs[i] = VF.enumerated("ref" + i, 5, 5, solver);
        }
        SetVar to = VF.set("to", Util.range(0, 5), solver);

        solver.post(Constraints.joinFunction(take, refs, to));

        assertEquals(4, randomizeStrategy(solver).findAllSolutions());
    }

    @Test(timeout = 60000)
    public void testJoinFunction() {
        Solver solver = new Solver();

        SetVar take = VF.set("take", new int[]{0, 1, 2}, solver);
        IntVar[] refs = new IntVar[3];
        for (int i = 0; i < refs.length; i++) {
            refs[i] = VF.enumerated("ref" + i, 0, 4, solver);
        }
        SetVar to = VF.set("to", new int[]{0, 1, 2, 3, 4}, solver);

        solver.post(Constraints.joinFunction(take, refs, to));

        assertEquals(1000, randomizeStrategy(solver).findAllSolutions());
    }
}
