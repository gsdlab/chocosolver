package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class EqualXY_ZTest {

    @Input(solutions = 7)
    public Object testEqualXY_Z(Model model) {
        return $(model.intVar("x", -1, 1),
                model.intVar("y", -1, 1),
                model.intVar("z", -1, 1)
        );
    }

    @Check
    public void check(int x, int y, int sum) {
        assertEquals(x + y, sum);
    }

    @ArcConsistent(entailed = true, opposite = true)
    @Test(timeout = 60000)
    public Constraint setup(IntVar x, IntVar y, IntVar sum) {
        return Constraints.equalArcConsistent(x, y, sum);
    }
}
