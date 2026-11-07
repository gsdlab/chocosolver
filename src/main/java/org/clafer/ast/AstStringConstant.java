package org.clafer.ast;

import org.clafer.common.Check;

/**
 * A constant string.
 *
 * @author jimmy
 */
public record AstStringConstant(String value) implements AstSetExpr {

    public AstStringConstant {
        value = Check.notNull(value);
    }

    @Override
    public String toString() {
        return value;
    }
}
