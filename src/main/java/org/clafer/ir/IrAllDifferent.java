package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class IrAllDifferent implements IrConstraint {

    private final IrIntExpr[] operands;

    IrAllDifferent(IrIntExpr[] operands) {
        this.operands = Check.noNulls(operands);
    }

    public IrIntExpr[] getOperands() {
        return operands;
    }

    @Override
    public <A, B> B accept(IrConstraintVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if(obj instanceof IrAllDifferent) {
            IrAllDifferent other = (IrAllDifferent) obj;
            return Arrays.equals(operands, other.operands);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(operands);
    }

    @Override
    public String toString() {
        return "allDifferent(" + Util.commaSeparate(operands) + ")";
    }
}
