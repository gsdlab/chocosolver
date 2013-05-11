package org.clafer.ast;

/**
 * Do NOT reuse for different expressions.
 * 
 * @author jimmy
 */
public class AstThis implements AstSetExpr {

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return "this";
    }
}
