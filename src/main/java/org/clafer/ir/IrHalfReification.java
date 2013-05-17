package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrHalfReification implements IrConstraint {

    private final IrBoolExpr antecedent;
    private final IrConstraint consequent;

    IrHalfReification(IrBoolExpr antecedent, IrConstraint consequent) {
        this.antecedent = Check.notNull(antecedent);
        this.consequent = Check.notNull(consequent);
    }

    public IrBoolExpr getAntecedent() {
        return antecedent;
    }

    public IrConstraint getConsequent() {
        return consequent;
    }

    @Override
    public <A, B> B accept(IrConstraintVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrHalfReification) {
            IrHalfReification other = (IrHalfReification) obj;
            return antecedent.equals(other.antecedent) && consequent.equals(other.consequent);
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
