package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSetTernary extends IrAbstractSet {

    private final IrBoolExpr antecedent;
    private final IrSetExpr consequent;
    private final IrSetExpr alternative;

    public IrSetTernary(IrBoolExpr antecedent, IrSetExpr consequent, IrSetExpr alternative, Domain env, Domain ker, Domain card) {
        super(env, ker, card);
        this.antecedent = Check.notNull(antecedent);
        this.consequent = Check.notNull(consequent);
        this.alternative = Check.notNull(alternative);
    }

    public IrBoolExpr getAntecedent() {
        return antecedent;
    }

    public IrSetExpr getConsequent() {
        return consequent;
    }

    public IrSetExpr getAlternative() {
        return alternative;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrSetTernary) {
            IrSetTernary other = (IrSetTernary) obj;
            return antecedent.equals(other.antecedent)
                    && consequent.equals(other.consequent)
                    && alternative.equals(other.alternative)
                    && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return antecedent.hashCode() ^ consequent.hashCode() ^ alternative.hashCode();
    }

    @Override
    public String toString() {
        return "if (" + antecedent + ") then (" + consequent + ") else (" + alternative + ")";
    }
}
