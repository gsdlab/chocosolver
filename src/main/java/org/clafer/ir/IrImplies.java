package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrImplies implements IrBoolExpr {

    private final IrBoolExpr antecedent;
    private final IrBoolExpr consequent;

    IrImplies(IrBoolExpr antecedent, IrBoolExpr consequent) {
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
    public IrBoolExpr opposite() {
        return Irs.and(antecedent, Irs.not(consequent));
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return antecedent + " => " + consequent;
    }
}
