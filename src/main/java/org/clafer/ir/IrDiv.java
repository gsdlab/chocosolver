package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrDiv implements IrIntExpr {

    private final IrIntExpr numerator;
    private final IrIntExpr denominator;

    IrDiv(IrIntExpr numerator, IrIntExpr denominator) {
        this.numerator = Check.notNull(numerator);
        this.denominator = Check.notNull(denominator);
    }

    public IrIntExpr getNumerator() {
        return numerator;
    }

    public IrIntExpr getDenominator() {
        return denominator;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
