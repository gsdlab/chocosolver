package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstIfThenElse implements AstBoolExpr {

    private final AstBoolExpr antecedent;
    private final AstBoolExpr consequent;
    private final AstBoolExpr alternative;

    AstIfThenElse(AstBoolExpr antecedent, AstBoolExpr consequent, AstBoolExpr alternative) {
        this.antecedent = Check.notNull(antecedent);
        this.consequent = Check.notNull(consequent);
        this.alternative = Check.notNull(alternative);
    }

    public AstBoolExpr getAntecedent() {
        return antecedent;
    }

    public AstBoolExpr getConsequent() {
        return consequent;
    }

    public AstBoolExpr getAlternative() {
        return alternative;
    }

    @Override
    public <A, B> B accept(AstExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return "if " + antecedent + " then " + consequent + " else " + alternative;
    }
}
