package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class IrSetUnion extends IrAbstractSet implements IrSetExpr {

    private final IrSetExpr[] operands;

    IrSetUnion(IrSetExpr[] operands, IrDomain env, IrDomain ker, IrDomain card) {
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
        if (obj instanceof IrSetUnion) {
            IrSetUnion other = (IrSetUnion) obj;
            return Arrays.equals(operands, other.operands) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(operands);
    }

    @Override
    public String toString() {
        return "(" + Util.intercalate(") ∪ (", operands) + ")";
    }
}
