package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstLength(AstSetExpr string) implements AstSetExpr {

    public AstLength {
        string = Check.notNull(string);
    }

    @Override
    public String toString() {
        return "length(" + string + ")";
    }
}
