package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrNotBetween extends IrAbstractBool implements IrBoolExpr {

    private final IrIntExpr var;
    private final int low, high;

    IrNotBetween(IrIntExpr var, int low, int high, IrBoolDomain domain) {
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
        return new IrBetween(var, low, high, getDomain().invert());
    }

    @Override
    public boolean isNegative() {
        return true;
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrNotBetween) {
            IrNotBetween other = (IrNotBetween) obj;
            return var.equals(other.var) && low == other.low && high == other.high && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return ~(var.hashCode() ^ low ^ high);
    }

    @Override
    public String toString() {
        return var + " ∉ [" + low + ", " + high + "]";
    }
}
