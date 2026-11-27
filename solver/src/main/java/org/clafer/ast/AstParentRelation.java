package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstParentRelation(AstConcreteClafer parentRelation) implements AstSetExpr {

    public AstParentRelation {
        parentRelation = Check.notNull(parentRelation);
    }
}
