package org.clafer.ir;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrBoolConstraint implements IrConstraint {

    private final IrBoolExpr expr;

    IrBoolConstraint(IrBoolExpr expr) {
        this.expr = Check.notNull(expr);
    }

    public IrBoolExpr getExpr() {
        return expr;
    }

    @Override
    public <A, B> B accept(IrConstraintVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrBoolConstraint) {
            IrBoolConstraint other = (IrBoolConstraint) obj;
            return expr.equals(other.expr);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 37 * expr.hashCode();
    }

    @Override
    public String toString() {
        return expr.toString();
    }
}
