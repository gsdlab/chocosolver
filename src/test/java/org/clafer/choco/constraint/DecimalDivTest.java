package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.common.Util;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * The solution counts are from a brute-force enumeration; the last argument
 * selects the decimal one, see {@link DecimalMulTest#toOne(int)}.
 *
 * @author Michał Antkiewicz
 */
@RunWith(ConstraintQuickTest.class)
public class DecimalDivTest {

    @Input(solutions = 154)
    public Object testDecimalDiv(Model model) {
        return $(model.intVar("dividend", -12, 12), model.intVar("divisor", -5, 5),
                model.intVar("quotient", -25, 25), 1);
    }

    @Input(solutions = 10)
    public Object testDecimalDivSmallQuotient(Model model) {
        return $(model.intVar("dividend", -12, 12), model.intVar("divisor", -5, 5),
                model.intVar("quotient", -8, 8), 2);
    }

    @Check
    public void check(int dividend, int divisor, int quotient, int one) {
        assertNotEquals(0, divisor);
        assertEquals(Util.divRound((long) dividend * DecimalMulTest.toOne(one), divisor), quotient);
    }

    @Test(timeout = 60000)
    public Constraint setup(IntVar dividend, IntVar divisor, IntVar quotient, int one) {
        return Constraints.decimalDiv(dividend, divisor, quotient, DecimalMulTest.toOne(one));
    }
}
