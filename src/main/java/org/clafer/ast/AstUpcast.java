package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstUpcast implements AstSetExpr {

    private final AstSetExpr base;
    private final AstAbstractClafer target;

    AstUpcast(AstSetExpr base, AstAbstractClafer target) {
        this.base = Check.notNull(base);
        this.target = Check.notNull(target);
    }

    public AstSetExpr getBase() {
        return base;
    }

    public AstAbstractClafer getTarget() {
        return target;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
