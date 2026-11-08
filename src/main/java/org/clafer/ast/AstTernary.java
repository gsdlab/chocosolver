package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstTernary(AstBoolExpr antecedent, AstSetExpr consequent, AstSetExpr alternative) implements AstSetExpr {

    public AstTernary {
        antecedent = Check.notNull(antecedent);
        consequent = Check.notNull(consequent);
        alternative = Check.notNull(alternative);
    }

    @Override
    public String toString() {
        return "if " + antecedent + " then " + consequent + " else " + alternative;
    }
}
