package org.clafer.ast;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class AstMod implements AstSetExpr {

    private final AstSetExpr dividend, divisor;

    AstMod(AstSetExpr dividend, AstSetExpr divisor) {
        this.dividend = Check.notNull(dividend);
        this.divisor = Check.notNull(dividend);
    }

    public AstSetExpr getDividend() {
        return dividend;
    }

    public AstSetExpr getDivisor() {
        return divisor;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AstMod) {
            AstMod other = (AstMod) obj;
            return dividend.equals(other.dividend) && divisor.equals(other.divisor);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return dividend.hashCode() ^ divisor.hashCode();
    }

    @Override
    public String toString() {
        return dividend + " % " + divisor;
    }
}
