package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrTernary extends IrAbstractInt {

    private final IrBoolExpr antecedent;
    private final IrIntExpr consequent;
    private final IrIntExpr alternative;

    public IrTernary(IrBoolExpr antecedent, IrIntExpr consequent, IrIntExpr alternative, Domain domain) {
        super(domain);
        this.antecedent = Check.notNull(antecedent);
        this.consequent = Check.notNull(consequent);
        this.alternative = Check.notNull(alternative);
    }

    public IrBoolExpr getAntecedent() {
        return antecedent;
    }

    public IrIntExpr getConsequent() {
        return consequent;
    }

    public IrIntExpr getAlternative() {
        return alternative;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrTernary) {
            IrTernary other = (IrTernary) obj;
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
