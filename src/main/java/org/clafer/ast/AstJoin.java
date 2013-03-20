package org.clafer.ast;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class AstJoin implements AstSetExpression {

    private final AstSetExpression left;
    private final AstConcreteClafer right;

    public AstJoin(AstSetExpression left, AstConcreteClafer right) {
        if (!right.hasParent()) {
            throw new IllegalArgumentException();
        }
        this.left = Check.notNull(left);
        this.right = Check.notNull(right);
    }

    public AstSetExpression getLeft() {
        return left;
    }

    public AstConcreteClafer getRight() {
        return right;
    }

    @Override
    public <A, B> B accept(AstExpressionVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
