package org.clafer.ir;

import org.clafer.Check;

/**
 * Assumption: children are disjoint
 * 
 * @author jimmy
 */
public class IrJoin extends IrAbstractSet implements IrSetExpr {

    private final IrSetExpr take;
    private final IrSetExpr[] children;

    IrJoin(IrSetExpr take, IrSetExpr[] children, IrDomain env, IrDomain ker, IrDomain card) {
        super(env, ker, card);
        this.take = Check.notNull(take);
        this.children = Check.noNulls(children);
    }

    public IrSetExpr getTake() {
        return take;
    }

    public IrSetExpr[] getChildren() {
        return children;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }
}
