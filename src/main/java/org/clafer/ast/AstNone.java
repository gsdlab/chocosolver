package org.clafer.ast;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class AstNone implements AstBoolExpression {

    private final AstSetExpression set;

    public AstNone(AstSetExpression set) {
        this.set = Check.notNull(set);
    }

    public AstSetExpression getSet() {
        return set;
    }

    @Override
    public <A, B> B accept(AstExpressionVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
