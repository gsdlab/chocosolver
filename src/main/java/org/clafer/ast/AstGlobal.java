package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstGlobal implements AstSetExpr {

    private final AstClafer type;

    AstGlobal(AstClafer type) {
        this.type = Check.notNull(type);
    }

    public AstClafer getType() {
        return type;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
