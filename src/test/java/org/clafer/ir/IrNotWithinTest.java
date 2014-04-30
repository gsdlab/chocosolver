package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.clafer.test.NonEmpty;
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
public class IrNotWithinTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrIntVar value, @NonEmpty Domain range) {
        return notWithin(value, range);
    }

    @Solution
    public Constraint setup(IntVar value, int[] range) {
        return ICF.not_member(value, range);
    }
}
