package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.element;
import static org.clafer.ir.Irs.equal;
import org.clafer.test.Positive;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

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
        return element.getModel().element(element, array, index, 0);
    }
}
