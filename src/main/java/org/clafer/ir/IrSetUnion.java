package org.clafer.ir;

import org.clafer.domain.Domain;
import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class IrSetUnion extends IrAbstractSet {

    private final IrSetExpr[] operands;
    private final boolean disjoint;

    IrSetUnion(IrSetExpr[] operands, Domain env, Domain ker, Domain card, boolean disjoint) {
        super(env, ker, card);
        this.operands = Check.noNullsNotEmpty(operands);
        this.disjoint = disjoint;
    }

    public IrSetExpr[] getOperands() {
        return operands;
    }

    public boolean isDisjoint() {
        return disjoint;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrSetUnion) {
            IrSetUnion other = (IrSetUnion) obj;
            return Arrays.equals(operands, other.operands)
                    && disjoint == other.disjoint
                    && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(operands) ^ (disjoint ? 1231 : 1237);
    }

    @Override
    public String toString() {
        return "(" + Util.intercalate(") ∪ (", operands) + ")" + (disjoint ? " where disjoint" : "");
    }
}
