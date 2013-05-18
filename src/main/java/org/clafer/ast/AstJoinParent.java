package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstJoinParent implements AstSetExpr {

    private final AstSetExpr children;

    public AstJoinParent(AstSetExpr children) {
        this.children = Check.notNull(children);
    }

    public AstSetExpr getChildren() {
        return children;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return children + " . parent";
    }
}
