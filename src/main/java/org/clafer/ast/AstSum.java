package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstSum(AstSetExpr set) implements AstSetExpr {

    public AstSum {
        set = Check.notNull(set);
    }

    @Override
    public String toString() {
        return "sum(" + set + ")";
    }
}
