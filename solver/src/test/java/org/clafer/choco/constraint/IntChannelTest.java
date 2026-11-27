package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.env;
import static org.chocosolver.solver.variables.Var.ker;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.common.Util;
import org.clafer.test.NoCard;
import org.clafer.test.NonEmpty;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class IntChannelTest {

    @Input(solutions = 27)
    public Object testIntChannel(Model model) {
        return $(model.setVarArray("set", 3, ker(), env(0, 1, 2)),
                model.intVarArray("int", 3, env(0, 1, 2)));
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

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint setup(@NoCard @NonEmpty SetVar[] sets, @NonEmpty IntVar[] ints) {
        return Constraints.intChannel(sets, ints);
    }
}
