package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrJoinRelation extends IrAbstractSet {

    private final IrSetExpr take;
    private final IrSetArrayExpr children;
    private final boolean injective;

    IrJoinRelation(IrSetExpr take, IrSetArrayExpr children, Domain env, Domain ker, Domain card, boolean injective) {
        super(env, ker, card);
        this.take = Check.notNull(take);
        this.children = Check.notNull(children);
        this.injective = injective;
    }

    public IrSetExpr getTake() {
        return take;
    }

    public IrSetArrayExpr getChildren() {
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
            return take.equals(other.take) && children.equals(other.children) && injective == other.injective;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return take.hashCode() ^ children.hashCode() ^ (isInjective() ? 1 : 0);
    }

    @Override
    public String toString() {
        return take + " . " + children + (isInjective() ? " where injective" : "");
    }
}
