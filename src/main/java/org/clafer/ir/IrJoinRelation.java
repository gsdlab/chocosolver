package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrJoinRelation extends IrAbstractSet {

    private final IrSetExpr take;
    private final IrSetExpr[] children;
    private final boolean injective;

    IrJoinRelation(IrSetExpr take, IrSetExpr[] children, IrDomain env, IrDomain ker, IrDomain card, boolean injective) {
        super(env, ker, card);
        this.take = Check.notNull(take);
        this.children = Check.noNulls(children);
        this.injective = injective;
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
            return take.equals(other.take) && Arrays.equals(children, other.children) && injective == other.injective && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return take.hashCode() ^ Arrays.hashCode(children) ^ (isInjective() ? 1 : 0);
    }

    @Override
    public String toString() {
        return take + " . " + Arrays.toString(children) + (isInjective() ? " where injective" : "");
    }
}
