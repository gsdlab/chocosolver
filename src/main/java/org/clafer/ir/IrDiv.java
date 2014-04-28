package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;

/**
 * dividend / divisor
 * 
 * @author jimmy
 */
public class IrDiv extends IrAbstractInt {

    /**
     * Division is internally represented as a binary operation unlike
     * addition and subtraction. The reason is that this makes optimizing for
     * Choco easier.
     */
    private final IrIntExpr dividend, divisor;

    IrDiv(IrIntExpr dividend, IrIntExpr divisor, Domain domain) {
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
        if (obj instanceof IrDiv) {
            IrDiv other = (IrDiv) obj;
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
        return "(" + dividend + ") / (" + divisor + ")";
    }
}