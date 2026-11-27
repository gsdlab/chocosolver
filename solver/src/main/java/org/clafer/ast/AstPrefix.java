package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstPrefix(AstSetExpr prefix, AstSetExpr word) implements AstBoolExpr {

    public AstPrefix {
        prefix = Check.notNull(prefix);
        word = Check.notNull(word);
    }

    @Override
    public String toString() {
        return prefix + " prefix " + word;
    }
}
