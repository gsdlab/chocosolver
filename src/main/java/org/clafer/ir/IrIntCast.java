package org.clafer.ir;

import org.clafer.common.Check;

/**
 * Casts an boolean to an integer.
 *
 * @author jimmy
 */
public class IrIntCast extends IrAbstractInt implements IrIntExpr {

    private final IrBoolExpr expr;

    IrIntCast(IrBoolExpr expr, IrDomain domain) {
        super(domain);
        this.expr = Check.notNull(expr);
    }

    public IrBoolExpr getExpr() {
        return expr;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrIntCast) {
            IrIntCast other = (IrIntCast) obj;
            return expr.equals(other.expr) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 511 * expr.hashCode();
    }

    @Override
    public String toString() {
        return expr + " ? 1 : 0";
    }
}
