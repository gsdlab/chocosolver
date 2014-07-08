package org.clafer.ir;

import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.clafer.test.Positive;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.constraints.Constraint;
import solver.variables.CStringVar;
import solver.variables.IntVar;
import static solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrStringElementTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrStringVar element, IrStringVar[] array, @Positive IrIntVar index) {
        assumeTrue(index.getHighBound() < array.length);
        return equal(element, element(array, index));
    }

    @Solution
    public Constraint setup(CStringVar element, CStringVar[] array, IntVar index) {
        return Constraints.element(index,
                mapChars(array), mapLength(array),
                element.getChars(), element.getLength());
    }
}
