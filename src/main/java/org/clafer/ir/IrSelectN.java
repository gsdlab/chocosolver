package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrSelectN implements IrConstraint {

    private final IrBoolExpr[] bools;
    private final IrIntExpr n;

    IrSelectN(IrBoolExpr[] bools, IrIntExpr n) {
        this.bools = Check.noNulls(bools);
        this.n = Check.notNull(n);
    }

    public IrBoolExpr[] getBools() {
        return bools;
    }

    public IrIntExpr getN() {
        return n;
    }

    @Override
    public <A, B> B accept(IrConstraintVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
