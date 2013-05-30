package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstJoin implements AstSetExpr {

    private final AstSetExpr left;
    private final AstConcreteClafer right;

    AstJoin(AstSetExpr left, AstConcreteClafer right) {
        this.left = Check.notNull(left);
        this.right = Check.notNull(right);
    }

    public AstSetExpr getLeft() {
        return left;
    }

    public AstConcreteClafer getRight() {
        return right;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return left + " . " + right;
    }
}
