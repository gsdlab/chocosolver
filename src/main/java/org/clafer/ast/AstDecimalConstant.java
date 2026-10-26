package org.clafer.ast;

import java.math.BigDecimal;
import org.clafer.common.Check;

/**
 * A constant decimal, as written in the model. Type analysis replaces it with
 * an {@link AstConstant} of {@link AstDecimalClafer} holding the value scaled
 * to the decimal scale of the model.
 *
 * @author Michał Antkiewicz
 */
public class AstDecimalConstant implements AstSetExpr {

    private final BigDecimal value;

    AstDecimalConstant(BigDecimal value) {
        this.value = Check.notNull(value);
    }

    public BigDecimal getValue() {
        return value;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof AstDecimalConstant) {
            AstDecimalConstant other = (AstDecimalConstant) obj;
            return value.equals(other.value);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }

    @Override
    public String toString() {
        return value.toPlainString();
    }
}
