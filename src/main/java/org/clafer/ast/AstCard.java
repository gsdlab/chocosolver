package org.clafer.ast;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class AstCard implements AstSetExpression {

    private final AstSetExpression set;

    public AstCard(AstSetExpression set) {
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
