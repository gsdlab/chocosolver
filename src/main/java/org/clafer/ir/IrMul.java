package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;

/**
 * multiplicant * multiplier
 *
 * @author jimmy
 */
public class IrMul extends IrAbstractInt {

    /**
     * Multiplication is internally represented as a binary operation unlike
     * addition and subtraction. The reason is that this makes optimizing for
     * Choco easier.
     */
    private final IrIntExpr multiplicand, multiplier;
    private final Domain intRange;

    IrMul(IrIntExpr multiplicand, IrIntExpr multiplier, Domain intRange, Domain domain) {
        super(domain);
        this.multiplicand = Check.notNull(multiplicand);
        this.multiplier = Check.notNull(multiplier);
        this.intRange = intRange;
    }

    public IrIntExpr getMultiplicand() {
        return multiplicand;
    }

    public IrIntExpr getMultiplier() {
        return multiplier;
    }

    public Domain getIntRange() {
        return intRange;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor< A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrMul) {
            IrMul other = (IrMul) obj;
            return multiplicand.equals(other.multiplicand) && multiplier.equals(other.multiplier)
                    && intRange.equals(other.intRange);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return multiplicand.hashCode() ^ multiplier.hashCode() ^ intRange.hashCode();
    }

    @Override
    public String toString() {
        return "(" + multiplicand + ") * (" + multiplier + ")";
    }
}
