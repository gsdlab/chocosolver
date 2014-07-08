package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrNotMember extends IrAbstractBool {

    private final IrIntExpr element;
    private final IrSetExpr set;

    IrNotMember(IrIntExpr element, IrSetExpr set, BoolDomain domain) {
        super(domain);
        this.element = Check.notNull(element);
        this.set = Check.notNull(set);
    }

    public IrIntExpr getElement() {
        return element;
    }

    public IrSetExpr getSet() {
        return set;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrMember(element, set, getDomain().invert());
    }

    @Override
    public boolean isNegative() {
        return true;
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
        if (obj instanceof IrNotMember) {
            IrNotMember other = (IrNotMember) obj;
            return element.equals(other.element) && set.equals(other.set) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return ~(element.hashCode() ^ set.hashCode());
    }

    @Override
    public String toString() {
        return element + " ∉ " + set;
    }
}
