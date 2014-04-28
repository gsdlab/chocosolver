package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrMask extends IrAbstractSet {

    private final IrSetExpr set;
    private final int from;
    private final int to;

    IrMask(IrSetExpr set, int from, int to, Domain env, Domain ker, Domain card) {
        super(env, ker, card);

        if (from > to) {
            throw new IllegalArgumentException();
        }

        this.set = Check.notNull(set);
        this.from = from;
        this.to = to;
    }

    public IrSetExpr getSet() {
        return set;
    }

    public int getFrom() {
        return from;
    }

    public int getTo() {
        return to;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrMask) {
            IrMask other = (IrMask) obj;
            return set.equals(other.set) & from == other.from && to == other.to && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return set.hashCode() ^ from ^ to;
    }

    @Override
    public String toString() {
        return set + " mask [" + from + ", " + to + "]";
    }
}
