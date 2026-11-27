package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstChildRelation(AstClafer childType) implements AstSetExpr {

    public AstChildRelation {
        childType = Check.notNull(childType);
    }

    @Override
    public String toString() {
        return childType.toString();
    }
}
