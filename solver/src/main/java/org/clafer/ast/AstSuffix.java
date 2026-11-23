package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstSuffix(AstSetExpr suffix, AstSetExpr word) implements AstBoolExpr {

    public AstSuffix {
        suffix = Check.notNull(suffix);
        word = Check.notNull(word);
    }

    @Override
    public String toString() {
        return suffix + " suffix " + word;
    }
}
