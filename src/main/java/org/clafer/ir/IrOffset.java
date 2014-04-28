package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrOffset extends IrAbstractSet {

    private final IrSetExpr set;
    private final int offset;

    IrOffset(IrSetExpr set, int offset, Domain env, Domain ker, Domain card) {
        super(env, ker, card);
        this.set = Check.notNull(set);

        this.offset = offset;
    }

    public IrSetExpr getSet() {
        return set;
    }

    public int getOffset() {
        return offset;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrOffset) {
            IrOffset other = (IrOffset) obj;
            return set.equals(other.set) & offset == other.offset && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return set.hashCode() ^ offset;
    }

    @Override
    public String toString() {
        return set + " << " + offset;
    }
}
