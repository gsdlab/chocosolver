package org.clafer.ast;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 * A constant set.
 *
 * @author jimmy
 */
public record AstConstant(ProductType type, int[]... value) implements AstSetExpr {

    public AstConstant {
        Check.notNull(type);
        Check.notNull(value);
        if (type.arity() != value.length) {
            throw new IllegalArgumentException();
        }
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof AstConstant other
                && type.equals(other.type) && Arrays.deepEquals(value, other.value);
    }

    @Override
    public int hashCode() {
        return type.hashCode() ^ Arrays.deepHashCode(value);
    }

    @Override
    public String toString() {
        if (value[0].length == 0) {
            return "[]";
        }
        if (type.isInt()) {
            return Integer.toString(value[0][0]);
        }
        return Arrays.deepToString(value) + "::" + type;
    }
}
