package org.clafer.constraint;

import choco.Choco;
import choco.cp.model.CPModel;
import choco.kernel.model.Model;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.solver.Solver;
import org.clafer.Util;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class BoolSlideTest extends ConstraintTest {

    private static void checkCorrectness(Solver solver, IntegerVariable[] base, IntegerVariable[] slide, IntegerVariable offset) {
        int[] $base = Util.getVals(solver.getVar(base));
        int[] $slide = Util.getVals(solver.getVar(slide));
        int $offset = solver.getVar(offset).getVal();

        assertTrue($slide.length - $base.length >= $offset);
        for (int i = 0; i < $base.length; i++) {
            assertEquals($base[i], $slide[i + $offset]);
        }
    }

    @Test(timeout = 60000)
    public void testBoolSlide() {
        for (int repeat = 0; repeat < 10; repeat++) {
            Model m = new CPModel();

            IntegerVariable[] base = Choco.makeBooleanVarArray("i", nextInt(100) + 1);
            IntegerVariable[] slide = Choco.makeBooleanVarArray("i", nextInt(100) + base.length);
            IntegerVariable offset = Choco.makeIntVar("i", 0, nextInt(slide.length - base.length + 1));

            m.addConstraint(BoolSlideManager.boolSlide(base, slide, offset));

            for (int restart = 0; restart < 10; restart++) {
                Solver solver = solveRandomly(m);
                checkCorrectness(solver, base, slide, offset);
            }
        }
    }

    @Test(timeout = 60000)
    public void quickTest() {
        Model m = new CPModel();
        IntegerVariable[] base = Choco.makeBooleanVarArray("base", 4);
        IntegerVariable[] slide = Choco.makeBooleanVarArray("base", 6);
        IntegerVariable offset = Choco.makeIntVar("base", 0, 3);

        m.addConstraint(BoolSlideManager.boolSlide(base, slide, offset));

        assertEquals(192, quickCheckModel(m, 10));
    }
}
