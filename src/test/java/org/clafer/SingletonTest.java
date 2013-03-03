package org.clafer;

import static choco.Choco.*;
import choco.cp.model.CPModel;
import choco.cp.solver.CPSolver;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;
import static org.clafer.SingletonManager.*;

/**
 *
 * @author jimmy
 */
public class SingletonTest {

    private Model m;
    private IntegerVariable i;
    private SetVariable s;
    private CPSolver solver;

    @Before
    public void setUp() {
        m = new CPModel();
        i = makeIntVar("iv1", 1, 10);
        s = makeSetVar("sv1", 3, 13);
        solver = new CPSolver();
        trySolve = false;
    }

    @After
    public void tearDown() {
    }
    private boolean trySolve;

    private boolean solve() {
        solver.read(m);
        trySolve = true;
        return solver.solve();
    }

    private boolean solutions() {
        if (trySolve) {
            return solver.nextSolution();
        }
        return solve();
    }

    public int getI() {
        return solver.getVar(i).getVal();
    }

    public int[] getS() {
        return solver.getVar(s).getValue();
    }

    public int[] array(int... a) {
        return a;
    }

    @Test
    public void testSimple() {
        m.addConstraint(singleton(i, s));

        solve();

        while (solutions()) {
            assertArrayEquals(array(getI()), getS());
        }
    }
}
