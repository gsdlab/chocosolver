package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrIfThenElse extends IrAbstractBool {

    private final IrBoolExpr antecedent;
    private final IrBoolExpr consequent;
    private final IrBoolExpr alternative;

    public IrIfThenElse(IrBoolExpr antecedent, IrBoolExpr consequent, IrBoolExpr alternative, BoolDomain domain) {
        super(domain);
        this.antecedent = Check.notNull(antecedent);
        this.consequent = Check.notNull(consequent);
        this.alternative = Check.notNull(alternative);
    }

    public IrBoolExpr getAntecedent() {
        return antecedent;
    }

    public IrBoolExpr getConsequent() {
        return consequent;
    }

    public IrBoolExpr getAlternative() {
        return alternative;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrIfThenElse(antecedent, consequent.negate(), alternative.negate(), getDomain().invert());
    }

    @Override
    public boolean isNegative() {
        return false;
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrIfThenElse) {
            IrIfThenElse other = (IrIfThenElse) obj;
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
