package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrNot implements IrDualExpr {

    private final IrBoolExpr proposition;

    public IrNot(IrBoolExpr proposition) {
        this.proposition = Check.notNull(proposition);
    }

    public IrBoolExpr getProposition() {
        return proposition;
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public IrBoolExpr opposite() {
        return proposition;
    }

    @Override
    public String toString() {
        return "!(" + proposition + ")";
    }
}
