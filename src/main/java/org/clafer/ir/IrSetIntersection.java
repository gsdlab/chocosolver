package org.clafer.ir;

import org.clafer.domain.Domain;
import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class IrSetIntersection extends IrAbstractSet {

    private final IrSetExpr[] operands;

    IrSetIntersection(IrSetExpr[] operands, Domain env, Domain ker, Domain card) {
        super(env, ker, card);
        this.operands = Check.noNullsNotEmpty(operands);
    }

    public IrSetExpr[] getOperands() {
        return operands;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrSetIntersection) {
            IrSetIntersection other = (IrSetIntersection) obj;
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
        return "(" + Util.intercalate(") ∪ (", operands) + ")";
    }
}