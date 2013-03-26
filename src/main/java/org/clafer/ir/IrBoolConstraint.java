package org.clafer.ir;

import org.clafer.Check;

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
    public String toString() {
        return expr.toString();
    }
}
