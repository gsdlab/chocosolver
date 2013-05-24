package org.clafer.ir;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSetDifference extends IrAbstractSet implements IrSetExpr {

    private final IrSetExpr minuend, subtrahend;

    IrSetDifference(IrSetExpr minuend, IrSetExpr subtrahend,
            IrDomain env, IrDomain ker, IrDomain card) {
        super(env, ker, card);
        this.minuend = Check.notNull(minuend);
        this.subtrahend = Check.notNull(subtrahend);
    }

    public IrSetExpr getMinuend() {
        return minuend;
    }

    public IrSetExpr getSubtrahend() {
        return subtrahend;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrSetDifference) {
            IrSetDifference other = (IrSetDifference) obj;
            return minuend.equals(other.minuend) && subtrahend.equals(other.subtrahend) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 7 * minuend.hashCode() * subtrahend.hashCode();
    }

    @Override
    public String toString() {
        return "(" + minuend + ") - (" + subtrahend + ")";
    }
}