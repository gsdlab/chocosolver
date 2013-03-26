package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrSetCard implements IrIntExpr {

    private final IrSetExpr set;

    IrSetCard(IrSetExpr set) {
        this.set = Check.notNull(set);
    }

    public IrSetExpr getSet() {
        return set;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return "|" + set + "|";
    }
}
