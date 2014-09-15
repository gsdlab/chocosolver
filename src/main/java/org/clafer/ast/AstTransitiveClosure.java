package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstTransitiveClosure  implements AstSetExpr {

    private final AstSetExpr relation;

    AstTransitiveClosure(AstSetExpr relation) {
        this.relation = Check.notNull(relation);
    }

    public AstSetExpr getRelation() {
        return relation;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
