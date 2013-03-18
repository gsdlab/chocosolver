package org.clafer.constraint;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.clafer.constraint.UpcastManager.*;
import choco.Choco;
import choco.cp.model.CPModel;
import choco.kernel.model.Model;
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
        SetVariable to = Choco.makeSetVar("to", 0, from.getUppB() + rand.nextInt(50));
        int offset = rand.nextInt(to.getUppB() - from.getUppB() + 1);

        m.addConstraint(upcast(from, to, offset));

        for (int repeat = 0; repeat < 100; repeat++) {
            Solver solver = solveRandomly(m);

            int[] $from = solver.getVar(from).getValue();
            int[] $to = solver.getVar(to).getValue();

            assertEquals($from.length, $to.length);
            for (int i = 0; i < $from.length; i++) {
                assertEquals($from[i] + offset, $to[i]);
            }
        }
    }

    @Test(timeout = 60000)
    public void testSmall() {
        Model m = new CPModel();
        SetVariable from = Choco.makeSetVar("from", new int[]{0});
        SetVariable to = Choco.makeSetVar("to", new int[]{0, 1});
        int offset = 1;

        m.addConstraint(upcast(from, to, offset));

        for (int repeat = 0; repeat < 100; repeat++) {
            Solver solver = solveRandomly(m);

            int[] $from = solver.getVar(from).getValue();
            int[] $to = solver.getVar(to).getValue();

            assertEquals($from.length, $to.length);
            for (int i = 0; i < $from.length; i++) {
                assertEquals($from[i] + offset, $to[i]);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        Model m = new CPModel();

        SetVariable from = Choco.makeSetVar("from", 0, 5);
        SetVariable to = Choco.makeSetVar("to", 0, 5);
        int offset = 3;

        m.addConstraint(upcast(from, to, offset));

        assertEquals(8, quickCheckModel(m, 10));
    }
}
