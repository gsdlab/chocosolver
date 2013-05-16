package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrCard extends IrAbstractIntExpr {

    private final IrSetExpr set;

    IrCard(IrSetExpr set, IrDomain domain) {
        super(domain);
        this.set = Check.notNull(set);
        if (!IrUtil.isSubsetOf(domain, set.getCard())) {
            throw new IllegalArgumentException();
        }
    }

    public IrSetExpr getSet() {
        return set;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return "|" + set + "|";
    }
}
