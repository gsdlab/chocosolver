package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrBetween extends IrAbstractBoolExpr {

    private final IrIntExpr var;
    private final int low, high;

    IrBetween(IrIntExpr var, int low, int high, IrBoolDomain domain) {
        super(domain);
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
    public IrBoolExpr negate() {
        return new IrNotBetween(var, low, high, getDomain().invert());
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return var + " ∈ [" + low + ", " + high + "]";
    }
}
