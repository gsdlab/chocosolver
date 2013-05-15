package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrNot implements IrBoolExpr {

    private final IrBoolVar proposition;

    public IrNot(IrBoolVar proposition) {
        this.proposition = Check.notNull(proposition);
    }

    public IrBoolExpr getProposition() {
        return proposition;
    }

    @Override
    public IrBoolExpr opposite() {
        return proposition;
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return "!(" + proposition + ")";
    }
}
