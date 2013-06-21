package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrJoinRelation extends IrAbstractSet implements IrSetExpr {

    private final IrSetExpr take;
    private final IrSetExpr[] children;
    private final boolean injective = false;

    IrJoinRelation(IrSetExpr take, IrSetExpr[] children, IrDomain env, IrDomain ker, IrDomain card) {
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

    public boolean isInjective() {
        return injective;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrJoinRelation) {
            IrJoinRelation other = (IrJoinRelation) obj;
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
