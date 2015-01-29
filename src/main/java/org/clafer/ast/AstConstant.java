package org.clafer.ast;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 * A constant set.
 *
 * @author jimmy
 */
public class AstConstant implements AstSetExpr {

    private final ProductType type;
    private final int[][] value;

    /**
     *
     * @param type the type
     * @param value the value, must be sorted
     */
    AstConstant(ProductType type, int[]... value) {
        if (type.arity() != value.length) {
            throw new IllegalArgumentException();
        }
        this.type = Check.notNull(type);
        this.value = Check.notNull(value);
    }

    public ProductType getType() {
        return type;
    }

    public int[][] getValue() {
        return value;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        if (type.isInt()) {
            return Integer.toString(value[0][0]);
        }
        return Arrays.deepToString(value) + "::" + type;
    }
}
