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
public record AstDecimalConstant(BigDecimal value) implements AstSetExpr {

    public AstDecimalConstant {
        value = Check.notNull(value);
    }

    @Override
    public String toString() {
        return value.toPlainString();
    }
}
