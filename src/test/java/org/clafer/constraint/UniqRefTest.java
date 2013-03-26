//package org.clafer.constraint;
//
//import static org.junit.Assert.*;
//import choco.Choco;
//import choco.cp.model.CPModel;
//import choco.cp.solver.search.integer.branching.AssignVar;
//import choco.cp.solver.search.integer.valiterator.DecreasingDomain;
//import choco.cp.solver.search.integer.varselector.MinDomain;
//import choco.kernel.model.Model;
//import choco.kernel.model.variables.integer.IntegerVariable;
//import choco.kernel.solver.Solver;
//import java.util.Random;
//import org.clafer.Util;
//import org.junit.Test;
//
///**
// *
// * @author jimmy
// */
//public class UniqRefTest extends ConstraintTest {
//
//    private void checkCorrectness(Solver solver, IntegerVariable[] parents, IntegerVariable[] refs) {
//        int[] $parents = Util.getVals(solver.getVar(parents));
//        int[] $refs = Util.getVals(solver.getVar(refs));
//
//        int unused = parents[0].getUppB();
//
//        for (int i = 0; i < refs.length; i++) {
//            for (int j = 0; j < refs.length; j++) {
//                if (i != j && $refs[i] == $refs[j]) {
//                    assertTrue(i + " : " + j, $parents[i] != $parents[j] || $parents[i] == unused || $parents[j] == unused);
//                }
//            }
//        }
//    }
//
//    @Test(timeout = 60000)
//    public void testUniqRef() {
//        Random rand = new Random();
//        for (int rr = 0; rr < 10; rr++) {
//            Model m = new CPModel();
//
//            int len = 1 + rand.nextInt(50);
//
//            IntegerVariable[] parents = Choco.makeIntVarArray("parent", len, 0, rand.nextInt(50));
//            IntegerVariable[] refs = Choco.makeIntVarArray("ref", len, 0, rand.nextInt(50));
//
//            m.addConstraint(UniqRefManager.uniqRef(parents, refs));
//
//            for (int repeat = 0; repeat < 10; repeat++) {
//                Solver solver = solveRandomly(m);
//                checkCorrectness(solver, parents, refs);
//            }
//        }
//    }
//
//    /**
//     * Increase the chance of all parents being the same. Since the domain for
//     * refs is small, and if all parents are the same, then it will keep
//     * back tracking on refs, causing poor performance. Noticable when
//     * using uniqRefPrim.
//     */
//    @Test(timeout = 60000)
//    public void testManyParentSmallDomainRefs() {
//        Model m = new CPModel();
//
//        IntegerVariable[] parents = Choco.makeIntVarArray("parent", 10, 0, 5);
//        IntegerVariable[] refs = Choco.makeIntVarArray("ref", 10, 0, 5);
//
//        m.addConstraint(UniqRefManager.uniqRef(parents, refs));
//
//        Solver solver = solveOnce(m);
//        checkCorrectness(solver, parents, refs);
//    }
//
//    /**
//     * Similar to testManyParentSmallDomainRefs but force refs to branch first.
//     */
//    @Test(timeout = 60000)
//    public void testManyRefsSmallDomainParents() {
//        Model m = new CPModel();
//
//        IntegerVariable[] parents = Choco.makeIntVarArray("parent", 40, 0, 3);
//        IntegerVariable[] refs = Choco.makeIntVarArray("ref", 40, 0, 1);
//
//        m.addConstraint(UniqRefManager.uniqRef(parents, refs));
//
//        Solver solver = newSolver(m);
//        solver.addGoal(new AssignVar(new MinDomain(solver), new DecreasingDomain()));
//        assertTrue(solver.solve());
//        checkCorrectness(solver, parents, refs);
//    }
//
//    @Test(timeout = 60000)
//    public void quickTest() {
//        Model m = new CPModel();
//
//        IntegerVariable[] parents = Choco.makeIntVarArray("parent", 4, 0, 2);
//        IntegerVariable[] refs = Choco.makeIntVarArray("ref", 4, -2, 2);
//        m.addVariables(parents);
//        m.addVariables(refs);
//
//        m.addConstraint(UniqRefManager.uniqRef(parents, refs));
//
//        assertEquals(8501, quickCheckModel(m, 10));
//    }
//}
