package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrNotImplies extends IrAbstractBool implements IrBoolExpr {

    private final IrBoolExpr antecedent;
    private final IrBoolExpr consequent;

    IrNotImplies(IrBoolExpr antecedent, IrBoolExpr consequent, IrBoolDomain domain) {
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
        return new IrImplies(antecedent, consequent, getDomain().invert());
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return antecedent + " =/> " + consequent;
    }
}
