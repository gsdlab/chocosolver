package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public record AstIfThenElse(AstBoolExpr antecedent, AstBoolExpr consequent, AstBoolExpr alternative) implements AstBoolExpr {

    public AstIfThenElse {
        antecedent = Check.notNull(antecedent);
        consequent = Check.notNull(consequent);
        alternative = Check.notNull(alternative);
    }

    @Override
    public String toString() {
        return "if " + antecedent + " then " + consequent + " else " + alternative;
    }
}
