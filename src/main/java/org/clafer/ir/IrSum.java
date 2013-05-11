package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrSum implements IrIntExpr {

    private final IrIntExpr[] addends;

    IrSum(IrIntExpr[] addends) {
        this.addends = Check.noNulls(addends);
    }

    public IrIntExpr[] getAddends() {
        return addends;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
