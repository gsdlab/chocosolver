//package org.clafer.constraint;
//
//import choco.Choco;
//import choco.cp.model.CPModel;
//import choco.kernel.model.Model;
//import choco.kernel.model.variables.integer.IntegerVariable;
//import choco.kernel.solver.Solver;
//import org.clafer.Util;
//import static org.junit.Assert.*;
//import org.junit.Test;
//
///**
// *
// * @author jimmy
// */
//public class IncreasingTest extends ConstraintTest {
//
//    private static void checkCorrectness(Solver solver, IntegerVariable[] vars) {
//        int[] $vars = Util.getVals(solver.getVar(vars));
//
//        for (int i = 1; i < vars.length; i++) {
//            assertTrue($vars[i - 1] <= $vars[i]);
//        }
//    }
//
//    @Test(timeout = 60000)
//    public void testIncreasing() {
//        for (int repeat = 0; repeat < 10; repeat++) {
//            Model m = new CPModel();
//
//            int sup = nextInt(10) + 1;
//            int inf = nextInt(sup);
//            IntegerVariable[] vars = Choco.makeIntVarArray("var", nextInt(10), inf, sup);
//
//            m.addConstraint(IncreasingManager.increasing(vars));
//            m.addVariables(vars);
//
//            for (int restart = 0; restart < 10; restart++) {
//                Solver solver = solveRandomly(m);
//                checkCorrectness(solver, vars);
//            }
//        }
//    }
//
//    @Test(timeout = 60000)
//    public void quickTest() {
//        Model m = new CPModel();
//
//        IntegerVariable[] vars = Choco.makeIntVarArray("var", 5, 0, 10);
//
//        m.addConstraint(IncreasingManager.increasing(vars));
//        m.addVariables(vars);
//
//        assertEquals(3003, quickCheckModel(m, 10));
//    }
//}
