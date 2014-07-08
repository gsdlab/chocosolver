package org.clafer.ir;

import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.constraints.Constraint;
import solver.variables.CSetVar;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrSetEqualityTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrSetVar left, IrSetEquality.Op op, IrSetVar right) {
        return equality(left, op, right);
    }

    @Solution
    public Constraint setup(CSetVar var1, IrSetEquality.Op op, CSetVar var2) {
        switch (op) {
            case Equal:
                return Constraints.equal(var1.getSet(), var1.getCard(), var2.getSet(), var2.getCard());
            case NotEqual:
                return Constraints.notEqual(var1.getSet(), var1.getCard(), var2.getSet(), var2.getCard());
            default:
                throw new IllegalArgumentException();
        }
    }
}
