package org.clafer.ir;

import org.clafer.common.Check;
import org.clafer.domain.Domain;

/**
 * Multiplication or division of decimals scaled by {@code one}, rounded half
 * away from zero: {@code round(left * right / one)} or
 * {@code round(left * one / right)}.
 *
 * @author Michał Antkiewicz
 */
public class IrDecimalArithm extends IrAbstractInt {

    private final Op op;
    private final IrIntExpr left, right;
    private final int one;
    /**
     * The range of decimals, which limits the domain like the intRange of
     * {@link IrMul}, and is kept for rewriting.
     */
    private final Domain decimalRange;

    IrDecimalArithm(Op op, IrIntExpr left, IrIntExpr right, int one, Domain decimalRange, Domain domain) {
        super(domain);
        this.op = Check.notNull(op);
        this.left = Check.notNull(left);
        this.right = Check.notNull(right);
        this.one = one;
        this.decimalRange = Check.notNull(decimalRange);
    }

    public Op getOp() {
        return op;
    }

    public IrIntExpr getLeft() {
        return left;
    }

    public IrIntExpr getRight() {
        return right;
    }

    public int getOne() {
        return one;
    }

    public Domain getDecimalRange() {
        return decimalRange;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrDecimalArithm) {
            IrDecimalArithm other = (IrDecimalArithm) obj;
            return op.equals(other.op) && left.equals(other.left) && right.equals(other.right)
                    && one == other.one && decimalRange.equals(other.decimalRange);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return op.ordinal() ^ left.hashCode() ^ right.hashCode() ^ one;
    }

    @Override
    public String toString() {
        return "round((" + left + ") " + (op == Op.Mul ? "*" : "/") + " (" + right + ") @ " + one + ")";
    }

    public static enum Op {

        Mul,
        Div
    }
}
