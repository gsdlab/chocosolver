package org.clafer.ir;

import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.clafer.test.Positive;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.constraints.Constraint;
import solver.constraints.set.SCF;
import solver.variables.IntVar;
import solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrSetElementTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrSetVar element, IrSetVar[] array, @Positive IrIntVar index) {
        assumeTrue(index.getHighBound() < array.length);
        return equal(element, element(array(array), index));
    }

    @Solution
    public Constraint setup(SetVar element, SetVar[] array, IntVar index) {
        return SCF.element(index, array, 0, element);
    }
}
