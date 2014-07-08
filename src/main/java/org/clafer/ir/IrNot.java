package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrNot extends IrAbstractBool {

    private final IrBoolExpr expr;

    public IrNot(IrBoolExpr expr, BoolDomain domain) {
        super(domain);
        this.expr = Check.notNull(expr);
    }

    public IrBoolExpr getExpr() {
        return expr;
    }

    @Override
    public IrBoolExpr negate() {
        return expr;
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
        if (obj instanceof IrNot) {
            IrNot other = (IrNot) obj;
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
        return "!(" + expr + ")";
    }
}
