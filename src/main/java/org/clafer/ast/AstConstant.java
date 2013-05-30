package org.clafer.ast;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 * A constant set.
 * 
 * @author jimmy
 */
public class AstConstant implements AstSetExpr {

    private final AstClafer type;
    private final int[] value;

    /**
     * 
     * @param type the type
     * @param value the value, must be sorted
     */
    AstConstant(AstClafer type, int... value) {
        this.type = Check.notNull(type);
        this.value = Check.notNull(value);
    }

    public AstClafer getType() {
        return type;
    }

    public int[] getValue() {
        return value;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        if (value.length == 1 && type instanceof AstIntClafer) {
            return Integer.toString(value[0]);
        }
        return Arrays.toString(value) + "::" + type;
    }
}
