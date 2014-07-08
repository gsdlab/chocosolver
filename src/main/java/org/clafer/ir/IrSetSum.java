package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSetSum extends IrAbstractInt {

    private final IrSetExpr set;

    public IrSetSum(IrSetExpr set, Domain domain) {
        super(domain);
        this.set = Check.notNull(set);
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
        if (obj instanceof IrSetSum) {
            IrSetSum other = (IrSetSum) obj;
            return set.equals(other.set);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 3 * set.hashCode();
    }

    @Override
    public String toString() {
        return "sum(" + set + ")";
    }
}
