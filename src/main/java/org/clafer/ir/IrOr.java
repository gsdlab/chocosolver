package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class IrOr extends IrAbstractBool {

    private final IrBoolExpr[] operands;

    IrOr(IrBoolExpr[] operands, BoolDomain domain) {
        super(domain);
        this.operands = Check.noNullsNotEmpty(operands);
    }

    public IrBoolExpr[] getOperands() {
        return operands;
    }

    @Override
    public IrBoolExpr negate() {
        IrBoolExpr[] negativeOperands = new IrBoolExpr[operands.length];
        for (int i = 0; i < negativeOperands.length; i++) {
            negativeOperands[i] = operands[i].negate();
        }
        return new IrAnd(negativeOperands, getDomain().invert());
    }

    @Override
    public boolean isNegative() {
        return false;
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrOr) {
            IrOr other = (IrOr) obj;
            return Arrays.equals(operands, other.operands) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 3 * Arrays.hashCode(operands);
    }

    @Override
    public String toString() {
        return '(' + Util.intercalate(") | (", operands) + ')';
    }
}
