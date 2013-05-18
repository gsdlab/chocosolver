package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public class AstConstant implements AstSetExpr {

    private final int value;

    AstConstant(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return Integer.toString(value);
    }
}
