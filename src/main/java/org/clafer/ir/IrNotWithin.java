package org.clafer.ir;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrNotWithin extends IrAbstractBool implements IrBoolExpr {

    private final IrIntExpr var;
    private final IrDomain range;

    IrNotWithin(IrIntExpr var, IrDomain range, IrBoolDomain domain) {
        super(domain);
        this.var = Check.notNull(var);
        this.range = Check.notNull(range);

        if (range.isEmpty()) {
            throw new IllegalArgumentException();
        }
    }

    public IrIntExpr getVar() {
        return var;
    }

    public IrDomain getRange() {
        return range;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrWithin(var, range, getDomain().invert());
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
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrNotWithin) {
            IrNotWithin other = (IrNotWithin) obj;
            return var.equals(other.var) && range.equals(other.range) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return ~(var.hashCode() ^ range.hashCode());
    }

    @Override
    public String toString() {
        return var + " ∉ " + range;
    }
}
