package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstMod(AstSetExpr dividend, AstSetExpr divisor) implements AstSetExpr {

    public AstMod {
        dividend = Check.notNull(dividend);
        divisor = Check.notNull(divisor);
    }

    @Override
    public String toString() {
        return dividend + " % " + divisor;
    }
}
