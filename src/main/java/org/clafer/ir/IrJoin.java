package org.clafer.ir;

import java.util.Arrays;
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

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrJoin) {
            IrJoin other = (IrJoin) obj;
            return take.equals(other.take) && Arrays.equals(children, other.children) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return take.hashCode() ^ Arrays.hashCode(children);
    }

    @Override
    public String toString() {
        return take + " . " + Arrays.toString(children);
    }
}
