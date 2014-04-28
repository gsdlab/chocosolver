package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrXor extends IrAbstractBool {

    private final IrBoolExpr left, right;

    public IrXor(IrBoolExpr left, IrBoolExpr right, BoolDomain domain) {
        super(domain);
        this.left = Check.notNull(left);
        this.right = Check.notNull(right);
    }

    public IrBoolExpr getLeft() {
        return left;
    }

    public IrBoolExpr getRight() {
        return right;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrIfOnlyIf(left, right, getDomain().invert());
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
        if (obj instanceof IrXor) {
            IrXor other = (IrXor) obj;
            return left.equals(other.left) && right.equals(other.right) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return left.hashCode() ^ right.hashCode();
    }

    @Override
    public String toString() {
        return left + " ^ " + right;
    }
}
