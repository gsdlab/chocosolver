package org.clafer.ir;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrCard extends IrAbstractInt {

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
    public boolean equals(Object obj) {
        if (obj instanceof IrCard) {
            IrCard other = (IrCard) obj;
            return set.equals(other.set) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 31 * set.hashCode();
    }

    @Override
    public String toString() {
        return "|" + set + "|";
    }
}
