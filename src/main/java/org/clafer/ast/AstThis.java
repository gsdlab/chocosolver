package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public class AstThis implements AstSetExpression {

    public static final AstThis Singleton = new AstThis();

    private AstThis() {
    }

    @Override
    public <A, B> B accept(AstExpressionVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
