package org.clafer.ir;

import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.clafer.test.Positive;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.constraints.Constraint;
import solver.constraints.ICF;
import solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrElementTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrIntVar element, IrIntVar[] array, @Positive IrIntVar index) {
        assumeTrue(index.getHighBound() < array.length);
        return equal(element, element(array, index));
    }

    @Solution
    public Constraint setup(IntVar element, IntVar[] array, IntVar index) {
        return ICF.element(element, array, index, 0);
    }
}
