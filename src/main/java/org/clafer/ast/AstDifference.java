package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstDifference implements AstSetExpr {

    private final AstSetExpr left, right;

    public AstDifference(AstSetExpr left, AstSetExpr right) {
        this.left = Check.notNull(left);
        this.right = Check.notNull(right);
    }

    public AstSetExpr getLeft() {
        return left;
    }

    public AstSetExpr getRight() {
        return right;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AstDifference) {
            AstDifference other = (AstDifference) obj;
            return left.equals(other.left) && right.equals(other.right);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return left.hashCode() ^ right.hashCode();
    }

    @Override
    public String toString() {
        return "(" + left + ") -- (" + right + ")";
    }
}
