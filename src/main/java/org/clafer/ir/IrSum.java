package org.clafer.ir;

import org.clafer.Check;
import org.clafer.ir.IrDomain.IrBoundDomain;

/**
 *
 * @author jimmy
 */
public class IrSum implements IrIntExpr {

    private final IrIntExpr[] addends;

    IrSum(IrIntExpr[] addends) {
        if (addends.length == 0) {
            throw new IllegalArgumentException();
        }
        this.addends = Check.noNulls(addends);
    }

    @Override
    public IrDomain getDomain() {
        int low = 0;
        int high = 0;
        for (IrIntExpr addend : addends) {
            IrDomain domain = addend.getDomain();
            low += domain.getLowerBound();
            high += domain.getUpperBound();
        }
        return new IrBoundDomain(low, high);
    }

    public IrIntExpr[] getAddends() {
        return addends;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
