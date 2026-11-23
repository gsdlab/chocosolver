package org.clafer.ir;

import org.clafer.common.Check;
import org.clafer.domain.Domain;

/**
 * dividend % divisor
 *
 * @author jimmy
 */
public class IrMod extends IrAbstractInt {

    private final IrIntExpr dividend, divisor;

    IrMod(IrIntExpr dividend, IrIntExpr divisor, Domain domain) {
        super(domain);
        this.dividend = Check.notNull(dividend);
        this.divisor = Check.notNull(divisor);
    }

    public IrIntExpr getDividend() {
        return dividend;
    }

    public IrIntExpr getDivisor() {
        return divisor;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor< A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrMod) {
            IrMod other = (IrMod) obj;
            return dividend.equals(other.dividend) && divisor.equals(other.divisor);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return dividend.hashCode() ^ divisor.hashCode();
    }

    @Override
    public String toString() {
        return "(" + dividend + ") % (" + divisor + ")";
    }
}
