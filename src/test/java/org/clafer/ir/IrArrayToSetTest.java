package org.clafer.ir;

import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.clafer.test.NonEmpty;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.constraints.Constraint;
import solver.variables.CSetVar;
import solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrArrayToSetTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(@NonEmpty IrIntVar[] array, int globalCardinality, IrSetVar set) {
        return equal(arrayToSet(array, null), set);
    }

    @Solution
    public Constraint setup(IntVar[] array, int globalCardinality, CSetVar set) {
        return Constraints.arrayToSet(array, set.getSet(), set.getCard(), null);
    }
}
