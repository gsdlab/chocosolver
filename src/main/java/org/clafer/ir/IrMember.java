package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrMember extends IrAbstractBool implements IrBoolExpr {

    private final IrIntExpr element;
    private final IrSetExpr set;

    IrMember(IrIntExpr element, IrSetExpr set, IrBoolDomain domain) {
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
        return new IrNotMember(element, set, getDomain().invert());
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
    public boolean equals(Object obj) {
        if (obj instanceof IrMember) {
            IrMember other = (IrMember) obj;
            return element.equals(other.element) && set.equals(other.set) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return element.hashCode() ^ set.hashCode();
    }

    @Override
    public String toString() {
        return element + " ∈ " + set;
    }
}
