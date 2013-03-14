package org.clafer.constraint;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.clafer.constraint.UpcastManager.*;
import choco.Choco;
import choco.cp.model.CPModel;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import choco.kernel.solver.Solver;
import java.util.Random;

/**
 *
 * @author jimmy
 */
public class UpcastTest extends ConstraintTest {

    @Test(timeout = 60000)
    public void testUpcast() {
        Random rand = new Random();
        Model m = new CPModel();
        SetVariable from = Choco.makeSetVar("from", 0, rand.nextInt(50));
        IntegerVariable offset = Choco.makeIntVar("offset", 0, rand.nextInt(50));
        SetVariable to = Choco.makeSetVar("to", 0, rand.nextInt(50));

        m.addConstraint(upcast(from, offset, to));

        for (int repeat = 0; repeat < 100; repeat++) {
            Solver solver = solveRandomly(m);

            int[] $from = solver.getVar(from).getValue();
            int $offset = solver.getVar(offset).getVal();
            int[] $to = solver.getVar(to).getValue();

            assertEquals($from.length, $to.length);
            for (int i = 0; i < $from.length; i++) {
                assertEquals($from[i] + $offset, $to[i]);
            }
        }
    }

    @Test(timeout = 60000)
    public void testSmall() {
        Model m = new CPModel();
        SetVariable from = Choco.makeSetVar("from", new int[]{0});
        IntegerVariable offset = Choco.makeIntVar("offset", new int[]{0, 1});
        SetVariable to = Choco.makeSetVar("to", new int[]{0, 1});

        m.addConstraint(upcast(from, offset, to));

        for (int repeat = 0; repeat < 100; repeat++) {
            Solver solver = solveRandomly(m);

            int[] $from = solver.getVar(from).getValue();
            int $offset = solver.getVar(offset).getVal();
            int[] $to = solver.getVar(to).getValue();

            assertEquals($from.length, $to.length);
            for (int i = 0; i < $from.length; i++) {
                assertEquals($from[i] + $offset, $to[i]);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        Model m = new CPModel();

        SetVariable from = Choco.makeSetVar("from", 0, 5);
        IntegerVariable offset = Choco.makeIntVar("offset", 0, 5);
        SetVariable to = Choco.makeSetVar("to", 0, 5);

        m.addConstraint(upcast(from, offset, to));

        assertEquals(126, quickCheckModel(m, 10));
    }
}
