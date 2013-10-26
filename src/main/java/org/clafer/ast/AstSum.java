package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstSum implements AstSetExpr {

    private final AstSetExpr set;

    AstSum(AstSetExpr set) {
        this.set = Check.notNull(set);
    }

    public AstSetExpr getSet() {
        return set;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return "sum(" + set + ")";
    }
}
