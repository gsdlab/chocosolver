package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSubsetEq extends IrAbstractBool {

    private final IrSetExpr subset, superset;

    public IrSubsetEq(IrSetExpr subset, IrSetExpr superset, BoolDomain domain) {
        super(domain);
        this.subset = Check.notNull(subset);
        this.superset = Check.notNull(superset);
    }

    public IrSetExpr getSubset() {
        return subset;
    }

    public IrSetExpr getSuperset() {
        return superset;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrNot(this, getDomain().invert());
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
        if (obj instanceof IrSubsetEq) {
            IrSubsetEq other = (IrSubsetEq) obj;
            return subset.equals(other.subset) && superset.equals(other.superset) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return subset.hashCode() ^ superset.hashCode();
    }

    @Override
    public String toString() {
        return subset + " ⊆ " + superset;
    }
}
