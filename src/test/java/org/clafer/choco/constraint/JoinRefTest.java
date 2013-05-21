package org.clafer.choco.constraint;

import gnu.trove.set.hash.TIntHashSet;
import org.clafer.common.Util;
import org.clafer.choco.constraint.propagator.PropUtil;
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
public class JoinRefTest extends ConstraintTest {

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

    @Test(timeout = 60000)
    public void testJoinRef() throws Throwable {
        for (int repeat = 0; repeat < 10; repeat++) {
            Solver solver = new Solver();
            int num = nextInt(10);

            SetVar take = VariableFactory.set("take", Util.fromTo(0, num), solver);
            IntVar[] refs = new IntVar[num];
            for (int i = 0; i < refs.length; i++) {
                refs[i] = VariableFactory.enumerated("ref" + i, 0, nextInt(10), solver);
            }
            SetVar to = VariableFactory.set("to", Util.fromTo(0, nextInt(10)), solver);

            solver.post(Constraints.joinRef(take, refs, to));

            assertTrue(randomizeStrategy(solver).findSolution());
            checkCorrectness(take, refs, to);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                checkCorrectness(take, refs, to);
            }
        }
    }

    @Test
    public void testJoinRefSingleValue() {
        Solver solver = new Solver();

        SetVar take = VariableFactory.set("take", Util.fromTo(0, 2), solver);
        IntVar[] refs = new IntVar[2];
        for (int i = 0; i < refs.length; i++) {
            refs[i] = VariableFactory.enumerated("ref" + i, 5, 5, solver);
        }
        SetVar to = VariableFactory.set("to", Util.range(0, 5), solver);

        solver.post(Constraints.joinRef(take, refs, to));

        assertEquals(4, randomizeStrategy(solver).findAllSolutions());
    }

//    @Test(timeout = 60000)
//    public void testJoinLargeDomain() {
//        Random rand = new Random();
//        Model m = new CPModel();
//
//        SetVariable take = Choco.makeSetVar("take", 0, rand.nextInt(10));
//        IntegerVariable[] refs = Choco.makeIntVarArray("ref", 1 + rand.nextInt(10), 0, 1000000, Options.V_ENUM);
//        SetVariable to = Choco.makeSetVar("to", 0, 1000000, Options.V_NO_DECISION);
//
//        m.addConstraint(joinRef(take, refs, to));
//
//        Solver solver = solveOnce(m);
//        checkCorrectness(solver, take, refs, to);
//    }
    @Test(timeout = 60000)
    public void quickTest() {
        Solver solver = new Solver();

        SetVar take = VariableFactory.set("take", new int[]{0, 1, 2}, solver);
        IntVar[] refs = new IntVar[3];
        for (int i = 0; i < refs.length; i++) {
            refs[i] = VariableFactory.enumerated("ref" + i, 0, 4, solver);
        }
        SetVar to = VariableFactory.set("to", new int[]{0, 1, 2, 3, 4}, solver);

        solver.post(Constraints.joinRef(take, refs, to));

        assertEquals(1000, randomizeStrategy(solver).findAllSolutions());
    }
}
