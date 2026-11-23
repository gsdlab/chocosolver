package org.clafer.ir;

import org.clafer.common.Check;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class IrContainsSetTernary extends IrAbstractSet {

    private final IrSetExpr antecedent;
    private final int x;
    private final IrSetExpr consequent;

    IrContainsSetTernary(IrSetExpr antecedent, int x, IrSetExpr consequent, Domain env, Domain ker, Domain card) {
        super(env, ker, card);
        this.antecedent = Check.notNull(antecedent);
        this.x = x;
        this.consequent = Check.notNull(consequent);
    }

    public IrSetExpr getAntecedent() {
        return antecedent;
    }

    public int getX() {
        return x;
    }

    public IrSetExpr getConsequent() {
        return consequent;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrContainsSetTernary) {
            IrContainsSetTernary other = (IrContainsSetTernary) obj;
            return antecedent.equals(other.antecedent) && x == other.x
                    && consequent.equals(other.consequent) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return antecedent.hashCode() ^ Integer.hashCode(x) ^ consequent.hashCode();
    }

    @Override
    public String toString() {
        return "if " + x + " in " + antecedent + " then " + consequent + " else {}";
    }
}
