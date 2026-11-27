package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.CStringVar;
import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.compare;
import org.clafer.test.NonEmpty;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrStringCompareTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(@NonEmpty IrStringVar left, IrStringCompare.Op op, @NonEmpty IrStringVar right) {
        return compare(left, op, right);
    }

    @Solution
    public Constraint setup(CStringVar left, IrStringCompare.Op op, CStringVar right) {
        switch (op) {
            case Equal:
                return Constraints.equal(
                        left.getChars(), left.getLength(),
                        right.getChars(), right.getLength());
            case NotEqual:
                return Constraints.notEqual(
                        left.getChars(), left.getLength(),
                        right.getChars(), right.getLength());
            case LessThan:
                return Constraints.lessThan(left.getChars(), right.getChars());
            case LessThanEqual:
                return Constraints.lessThanEqual(left.getChars(), right.getChars());
            default:
                throw new IllegalArgumentException();
        }
    }
}
