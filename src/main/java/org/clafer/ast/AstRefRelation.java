package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstRefRelation(AstRef ref) implements AstSetExpr {

    public AstRefRelation {
        ref = Check.notNull(ref);
    }

    @Override
    public String toString() {
        return "ref(" + ref.getSourceType() + ")";
    }
}
