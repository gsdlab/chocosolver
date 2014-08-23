package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public class AstConcat implements AstSetExpr {

    private final AstSetExpr left, right;

    AstConcat(AstSetExpr left, AstSetExpr right) {
        this.left = left;
        this.right = right;
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
        if (obj instanceof AstConcat) {
            AstConcat other = (AstConcat) obj;
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
        return left + " ++ " + right;
    }
}
