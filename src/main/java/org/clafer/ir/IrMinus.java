package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrMinus extends IrAbstractInt {

    private final IrIntExpr expr;

    public IrMinus(IrIntExpr expr, Domain domain) {
        super(domain);
        this.expr = Check.notNull(expr);
    }

    public IrIntExpr getExpr() {
        return expr;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrMinus) {
            IrMinus other = (IrMinus) obj;
            return expr.equals(other.expr) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return ~expr.hashCode();
    }

    @Override
    public String toString() {
        return "-(" + expr + ")";
    }
}
