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
    public boolean equals(Object obj) {
        return obj instanceof AstThis;
    }

    @Override
    public int hashCode() {
        return 1317;
    }

    @Override
    public String toString() {
        return "this";
    }
}
