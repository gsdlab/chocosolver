package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.common.Util;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * The solution counts are from a brute-force enumeration; the last argument
 * selects the decimal one, see {@link DecimalMulTest#toOne(int)}.
 *
 * @author Michał Antkiewicz
 */
@RunWith(ConstraintQuickTest.class)
public class DecimalMulTest {

    @Input(solutions = 625)
    public Object testDecimalMul(Model model) {
        return $(model.intVar("multiplicand", -12, 12), model.intVar("multiplier", -12, 12),
                model.intVar("product", -15, 15), 1);
    }

    @Input(solutions = 445)
    public Object testDecimalMulSmallProduct(Model model) {
        return $(model.intVar("multiplicand", -12, 12), model.intVar("multiplier", -12, 12),
                model.intVar("product", -5, 5), 1);
    }

    @Input(solutions = 81)
    public Object testDecimalMulScaleZero(Model model) {
        return $(model.intVar("multiplicand", -4, 4), model.intVar("multiplier", -4, 4),
                model.intVar("product", -20, 20), 0);
    }

    /**
     * The decimal one is a power of 10; the random tests choose any int.
     */
    static int toOne(int n) {
        return new int[]{1, 10, 100, 1000}[Math.abs(n % 4)];
    }

    @Check
    public void check(int multiplicand, int multiplier, int product, int one) {
        assertEquals(Util.divRound((long) multiplicand * multiplier, toOne(one)), product);
    }

    @Test(timeout = 60000)
    public Constraint setup(IntVar multiplicand, IntVar multiplier, IntVar product, int one) {
        return Constraints.decimalMul(multiplicand, multiplier, product, toOne(one));
    }
}
