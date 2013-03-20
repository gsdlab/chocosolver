package org.clafer.ast;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class AstUpcast implements AstSetExpression {

    private final AstSetExpression base;
    private final AstAbstractClafer target;

    public AstUpcast(AstSetExpression base, AstAbstractClafer target) {
        this.base = Check.notNull(base);
        this.target = Check.notNull(target);
    }

    public AstSetExpression getBase() {
        return base;
    }

    public AstAbstractClafer getTarget() {
        return target;
    }

    @Override
    public <A, B> B accept(AstExpressionVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
