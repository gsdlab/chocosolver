package org.clafer.ir;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSetNop extends IrAbstractBool implements IrBoolExpr, IrNop {

    private final IrSetExpr expr;

    public IrSetNop(IrSetExpr expr) {
        super(IrBoolDomain.BoolDomain);
        this.expr = Check.notNull(expr);
    }

    @Override
    public IrSetExpr getExpr() {
        return expr;
    }

    @Override
    public IrBoolExpr negate() {
        return this;
    }

    @Override
    public boolean isNegative() {
        return false;
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
        if (obj instanceof IrSetNop) {
            IrSetNop other = (IrSetNop) obj;
            return expr.equals(other.expr) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return expr.hashCode();
    }

    @Override
    public String toString() {
        return "nop(" + expr + ")";
    }
}
