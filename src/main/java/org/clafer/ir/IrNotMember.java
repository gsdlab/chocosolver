package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrNotMember implements IrDualExpr {

    private final IrIntExpr var;
    private final int low, high;

    IrNotMember(IrIntExpr var, int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        this.var = Check.notNull(var);
        this.low = low;
        this.high = high;
    }

    public IrIntExpr getVar() {
        return var;
    }

    public int getLow() {
        return low;
    }

    public int getHigh() {
        return high;
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public IrBoolExpr opposite() {
        return Irs.member(var, low, high);
    }

    @Override
    public String toString() {
        return var + " ∉ [" + low + ", " + high + "]";
    }
}
