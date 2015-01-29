package org.clafer.choco.constraint;

import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class EqualXY_ZTest {

    @Check
    public void check(int x, int y, int sum) {
        assertEquals(x + y, sum);
    }

    @ArcConsistent(opposite = true)
    @Test(timeout = 60000)
    public Constraint setup(IntVar x, IntVar y, IntVar sum) {
        return Constraints.equalArcConsistent(x, y, sum);
    }
}
