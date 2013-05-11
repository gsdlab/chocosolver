package org.clafer.constraint;

import static org.junit.Assert.*;
import org.junit.Test;
import solver.Configuration;
import solver.Solver;
import solver.constraints.set.SetConstraintsFactory;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VariableFactory;

/**
 *
 * @author jimmy
 */
public class JoinRefTest extends ConstraintTest {

//    private void checkCorrectness(Solver solver, SetVariable take, IntegerVariable[] refs, SetVariable to) {
//        int[] $from = solver.getVar(take).getValue();
//        int[] $refs = Util.getVals(solver.getVar(refs));
//        int[] $to = solver.getVar(to).getValue();
//
//        TIntHashSet set = new TIntHashSet();
//
//        for (int f : $from) {
//            assertTrue(Util.in($refs[f], $to));
//            set.add($refs[f]);
//        }
//        assertEquals(set.size(), $to.length);
//    }
//
//    @Test(timeout = 60000)
//    public void testJoinRef() {
//        Random rand = new Random();
//        for (int rr = 0; rr < 10; rr++) {
//            Model m = new CPModel();
//
//            SetVariable take = Choco.makeSetVar("take", 0, rand.nextInt(50));
//            IntegerVariable[] refs = Choco.makeIntVarArray("ref", 1 + rand.nextInt(50), 0, rand.nextInt(50));
//            SetVariable to = Choco.makeSetVar("to", 0, rand.nextInt(50));
//
//            m.addConstraint(joinRef(take, refs, to));
//
//            for (int repeat = 0; repeat < 10; repeat++) {
//                Solver solver = solveRandomly(m, false);
//                checkCorrectness(solver, take, refs, to);
//            }
//        }
//    }
//
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

        assertEquals(1000, quickCheckModel(solver));
    }
}
