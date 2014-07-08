package org.clafer.ast;

import org.clafer.common.Check;

/**
 * A constant string.
 *
 * @author jimmy
 */
public class AstStringConstant implements AstSetExpr {

    private final String value;

    /**
     *
     * @param value the value, must be sorted
     */
    AstStringConstant(String value) {
        this.value = Check.notNull(value);
    }

    public String getValue() {
        return value;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return value;
    }
}
