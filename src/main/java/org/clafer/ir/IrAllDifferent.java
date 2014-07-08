package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class IrAllDifferent extends IrAbstractBool {

    private final IrIntExpr[] operands;

    IrAllDifferent(IrIntExpr[] operands, BoolDomain domain) {
        super(domain);
        this.operands = Check.noNullsNotEmpty(operands);
    }

    public IrIntExpr[] getOperands() {
        return operands;
    }

    @Override
    public boolean isNegative() {
        return false;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrNot(this, getDomain().invert());
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
        if (obj instanceof IrAllDifferent) {
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
