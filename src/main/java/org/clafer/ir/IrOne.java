package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class IrOne extends IrAbstractBool {

    private final IrBoolExpr[] operands;

    IrOne(IrBoolExpr[] operands, BoolDomain domain) {
        super(domain);
        this.operands = Check.noNullsNotEmpty(operands);
    }

    public IrBoolExpr[] getOperands() {
        return operands;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrNot(this, getDomain().invert());
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
        if (obj instanceof IrOne) {
            IrOne other = (IrOne) obj;
            return Arrays.equals(operands, other.operands) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 31 * Arrays.hashCode(operands);
    }

    @Override
    public String toString() {
        return "one(" + Util.commaSeparate(operands) + ")";
    }
}
