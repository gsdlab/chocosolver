package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.common.Util;
import org.clafer.test.NonEmpty;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.IntVar;
import solver.variables.SetVar;
import static solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class IntChannelTest {

    @Input(solutions = 27)
    public Object testIntChannel(Solver solver) {
        SetVar[] sets = new SetVar[3];
        for (int i = 0; i < sets.length; i++) {
            sets[i] = set("set_" + i, Util.fromTo(0, 3), solver);
        }
        IntVar[] ints = enumeratedArray("int", 3, Util.fromTo(0, 3), solver);
        return $(sets, ints);
    }

    @Check
    public void check(int[][] sets, int[] ints) {
        for (int i = 0; i < sets.length; i++) {
            for (int j : sets[i]) {
                assertTrue(j >= 0 && j < ints.length);
                assertEquals(i, ints[j]);
            }
        }
        for (int i = 0; i < ints.length; i++) {
            assertTrue(ints[i] >= 0 && ints[i] < sets.length);
            assertTrue(Util.in(i, sets[ints[i]]));
        }
    }

    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty SetVar[] sets, @NonEmpty IntVar[] ints) {
        return Constraints.intChannel(sets, ints);
    }
}
