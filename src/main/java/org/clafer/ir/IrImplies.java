package org.clafer.ir;

import org.clafer.domain.BoolDomain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrImplies extends IrAbstractBool {

    private final IrBoolExpr antecedent;
    private final IrBoolExpr consequent;

    IrImplies(IrBoolExpr antecedent, IrBoolExpr consequent, BoolDomain domain) {
        super(domain);
        this.antecedent = Check.notNull(antecedent);
        this.consequent = Check.notNull(consequent);
    }

    public IrBoolExpr getAntecedent() {
        return antecedent;
    }

    public IrBoolExpr getConsequent() {
        return consequent;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrNotImplies(antecedent, consequent, getDomain().invert());
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
        if (obj instanceof IrImplies) {
            IrImplies other = (IrImplies) obj;
            return antecedent.equals(other.antecedent) && consequent.equals(other.consequent) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return antecedent.hashCode() ^ consequent.hashCode();
    }

    @Override
    public String toString() {
        return antecedent + " => " + consequent;
    }
}
