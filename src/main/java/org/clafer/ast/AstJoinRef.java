package org.clafer.ast;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class AstJoinRef implements AstSetExpr {

    private final AstSetExpr deref;

    AstJoinRef(AstSetExpr deref) {
        this.deref = Check.notNull(deref);
    }

    public AstSetExpr getDeref() {
        return deref;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
