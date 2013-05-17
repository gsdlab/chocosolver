package org.clafer.ir;

import org.clafer.Check;

/**
 * Casts an integer to a boolean.
 * 
 * @author jimmy
 */
public class IrBoolCast extends IrAbstractBool implements IrBoolExpr {

    // !flipped => casts 1 to true and 0 to false
    // flipped => casts 1 to false and 0 to true
    private final boolean flipped;
    private final IrIntExpr expr;

    IrBoolCast(IrIntExpr expr, IrBoolDomain domain) {
        this(false, expr, domain);
    }

    IrBoolCast(boolean flip, IrIntExpr expr, IrBoolDomain domain) {
        super(domain);
        this.flipped = flip;
        this.expr = Check.notNull(expr);
        if (expr.getDomain().getLowerBound() < 0) {
            throw new IllegalArgumentException();
        }
        if (expr.getDomain().getUpperBound() > 1) {
            throw new IllegalArgumentException();
        }
    }

    public boolean isFlipped() {
        return flipped;
    }

    public IrIntExpr getExpr() {
        return expr;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrBoolCast(!flipped, expr, getDomain().invert());
    }

    @Override
    public boolean isNegative() {
        return flipped;
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrBoolCast) {
            IrBoolCast other = (IrBoolCast) obj;
            return flipped == other.flipped && expr.equals(other.expr) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 255 * (flipped ? ~expr.hashCode() : expr.hashCode());
    }

    @Override
    public String toString() {
        return expr + " == " + (flipped ? "0" : "1");
    }
}
