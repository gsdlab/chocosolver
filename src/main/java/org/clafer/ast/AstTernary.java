package org.clafer.ast;

import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class AstTernary implements AstSetExpr {

    private final AstBoolExpr antecedent;
    private final AstSetExpr consequent;
    private final AstSetExpr alternative;

    AstTernary(AstBoolExpr antecedent, AstSetExpr consequent, AstSetExpr alternative) {
        this.antecedent = Check.notNull(antecedent);
        this.consequent = Check.notNull(consequent);
        this.alternative = Check.notNull(alternative);
    }

    public AstBoolExpr getAntecedent() {
        return antecedent;
    }

    public AstSetExpr getConsequent() {
        return consequent;
    }

    public AstSetExpr getAlternative() {
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
