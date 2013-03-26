//package org.clafer.constraint;
//
//import choco.Choco;
//import choco.cp.model.CPModel;
//import choco.kernel.model.Model;
//import choco.kernel.model.variables.integer.IntegerVariable;
//import choco.kernel.solver.Solver;
//import java.util.Arrays;
//import org.clafer.Util;
//import static org.junit.Assert.*;
//import org.junit.Test;
//
///**
// *
// * @author jimmy
// */
//public class SelectNTest extends ConstraintTest {
//
//    private static void checkCorrectness(Solver solver, IntegerVariable[] bools, IntegerVariable n) {
//        int[] $bools = Util.getVals(solver.getVar(bools));
//        int $n = solver.getVar(n).getVal();
//
//        for (int i = 0; i < $bools.length; i++) {
//            assertEquals("Select " + $n + " on " + Arrays.toString($bools),
//                    $bools[i], i < $n ? 1 : 0);
//        }
//    }
//
//    @Test(timeout = 60000)
//    public void testSelectN() {
//        for (int repeat = 0; repeat < 10; repeat++) {
//            Model m = new CPModel();
//
//            IntegerVariable[] bools = Choco.makeBooleanVarArray("bools", 10);
//            IntegerVariable n = Choco.makeIntVar("n", 0, 10);
//
//            m.addConstraint(SelectNManager.selectN(bools, n));
//            for (int restart = 0; restart < 10; restart++) {
//                Solver solver = solveRandomly(m);
//                checkCorrectness(solver, bools, n);
//            }
//        }
//    }
//
//    @Test(timeout = 60000)
//    public void quickTest() {
//        Model m = new CPModel();
//
//        IntegerVariable[] bools = Choco.makeBooleanVarArray("bools", 20);
//        IntegerVariable n = Choco.makeIntVar("n", 0, 20);
//
//        m.addConstraint(SelectNManager.selectN(bools, n));
//
//        assertEquals(21, quickCheckModel(m, 10));
//    }
//}
