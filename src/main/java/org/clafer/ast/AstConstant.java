package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public class AstConstant implements AstExpression {

    private final int value;

    public AstConstant(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    @Override
    public <A, B> B accept(AstExpressionVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
