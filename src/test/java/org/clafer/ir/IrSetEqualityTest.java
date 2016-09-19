package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.SetVar;
import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.equality;
import org.junit.Test;
import org.junit.runner.RunWith;

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
    public Constraint setup(SetVar var1, IrSetEquality.Op op, SetVar var2) {
        switch (op) {
            case Equal:
                return Constraints.equal(var1, var1.getCard(), var2, var2.getCard());
            case NotEqual:
                return Constraints.notEqual(var1, var1.getCard(), var2, var2.getCard());
            default:
                throw new IllegalArgumentException();
        }
    }
}
