package org.clafer.ast;

/**
 *
 * @author jimmy
 */
public class AstLocal implements AstSetExpr {

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
