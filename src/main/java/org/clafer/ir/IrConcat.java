package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrConcat extends IrAbstractString {

    private final IrStringExpr left, right;

    IrConcat(IrStringExpr left, IrStringExpr right, 
            Domain[] charDomains, Domain lengthDomain) {
        super(charDomains, lengthDomain);
        this.left = Check.notNull(left);
        this.right = Check.notNull(right);
    }

    public IrStringExpr getLeft() {
        return left;
    }

    public IrStringExpr getRight() {
        return right;
    }

    @Override
    public <A, B> B accept(IrStringExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrConcat) {
            IrConcat other = (IrConcat) obj;
            return left.equals(other.left) && left.equals(other.right);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return left.hashCode() ^ right.hashCode();
    }

    @Override
    public String toString() {
        return left + " ++ " + right;
    }
}
