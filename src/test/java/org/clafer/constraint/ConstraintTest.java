package org.clafer.constraint;

import choco.cp.solver.search.integer.branching.AssignVar;
import choco.cp.solver.search.integer.valiterator.IncreasingDomain;
import choco.cp.solver.search.integer.valselector.RandomIntValSelector;
import choco.cp.solver.search.integer.varselector.MinDomain;
import choco.cp.solver.search.integer.varselector.RandomIntVarSelector;
import choco.cp.solver.search.set.AssignSetVar;
import choco.cp.solver.search.set.MinDomSet;
import choco.cp.solver.search.set.MinEnv;
import choco.cp.solver.search.set.RandomSetValSelector;
import choco.cp.solver.search.set.RandomSetVarSelector;
import static org.junit.Assert.*;
import choco.kernel.model.Model;
import choco.kernel.solver.Solver;
import org.clafer.Util;
import org.clafer.Util.Solution;
import org.clafer.Util.SolutionTest;

/**
 *
 * @author jimmy
 */
public abstract class ConstraintTest {

    public static Solver newSolver(Model m) {
        return Util.newSolver(m);
    }

    public static Solver solveOnce(Model m) {
        Solver solver = newSolver(m);
        assertTrue(solver.solve());
        assertTrue(solver.checkSolution());
        return solver;
    }

    public static Solver solveRandomly(Model m) {
        return solveRandomly(m, true);
    }

    public static Solver solveRandomly(Model m, boolean is) {
        Solver solver = newSolver(m);
        if (!is || Math.random() > 0.5) {
            solver.addGoal(new AssignSetVar(new RandomSetVarSelector(solver), new RandomSetValSelector()));
            solver.addGoal(new AssignVar(new RandomIntVarSelector(solver), new RandomIntValSelector()));
        } else {
            solver.addGoal(new AssignVar(new RandomIntVarSelector(solver), new RandomIntValSelector()));
            solver.addGoal(new AssignSetVar(new RandomSetVarSelector(solver), new RandomSetValSelector()));
        }
        solver.addGoal(new AssignVar(new MinDomain(solver), new RandomIntValSelector()));
        solver.addGoal(new AssignSetVar(new MinDomSet(solver), new MinEnv()));
        assertTrue(solver.solve());
        assertTrue(solver.checkSolution());
        return solver;
    }

    public int quickCheckModel(Model model) {
        return quickCheckModel(model, 1);
    }

    public int quickCheckModel(Model model, int repeat) {
        SolutionTest test = Util.testSolutions(model, repeat);

        assertTrue("Custom solution not equal to default solution: "
                + test.getDefaultSolution().getSolutions().size() + "/" + test.getCustomSolution().getSolutions().size() + ".",
                test.getDefaultSolution().getSolutions().equals(test.getCustomSolution().getSolutions()));
        for (Solution randomSI : test.getRandomSISolutions()) {
            assertTrue("RandomSI solution not equal default solution: "
                    + test.getDefaultSolution().getSolutions().size() + "/" + randomSI.getSolutions().size() + ".",
                    test.getDefaultSolution().getSolutions().equals(randomSI.getSolutions()));
        }
        for (Solution randomIS : test.getRandomISSolutions()) {
            assertTrue("RandomIS solution not equal to default solution: "
                    + test.getDefaultSolution().getSolutions().size() + "/" + randomIS.getSolutions().size() + ".",
                    test.getDefaultSolution().getSolutions().equals(randomIS.getSolutions()));
        }

        return test.getDefaultSolution().size();
    }
}
