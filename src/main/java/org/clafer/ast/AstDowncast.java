package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstDowncast implements AstSetExpr {

    private final AstSetExpr base;
    private final AstClafer target;

    AstDowncast(AstSetExpr base, AstClafer target) {
        this.base = Check.notNull(base);
        this.target = Check.notNull(target);
    }

    public AstSetExpr getBase() {
        return base;
    }

    public AstClafer getTarget() {
        return target;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return "(" + target + ") " + base;
    }
}
