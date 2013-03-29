package org.clafer.ir;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrJoinRef implements IrSetExpr {

    private final IrSetExpr take;
    private final IrIntExpr[] refs;

    IrJoinRef(IrSetExpr take, IrIntExpr[] refs) {
        this.take = Check.notNull(take);
        this.refs = Check.noNulls(refs);
    }

    public IrSetExpr getTake() {
        return take;
    }

    public IrIntExpr[] getRefs() {
        return refs;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
