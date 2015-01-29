package org.clafer.ir;

import org.clafer.domain.Domain;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class IrJoinFunction extends IrAbstractSet {

    private final IrSetExpr take;
    private final IrIntArrayExpr refs;
    private final Integer globalCardinality;

    IrJoinFunction(IrSetExpr take, IrIntArrayExpr refs, Domain env, Domain ker, Domain card, Integer globalCardinality) {
        super(env, ker, card);
        this.take = Check.notNull(take);
        this.refs = Check.notNull(refs);
        this.globalCardinality = globalCardinality;
    }

    public IrSetExpr getTake() {
        return take;
    }

    public IrIntArrayExpr getRefs() {
        return refs;
    }

    public boolean hasGlobalCardinality() {
        return globalCardinality != null;
    }

    public Integer getGlobalCardinality() {
        return globalCardinality;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrJoinFunction) {
            IrJoinFunction other = (IrJoinFunction) obj;
            return take.equals(other.take) && refs.equals(other.refs) && Util.equals(globalCardinality, other.globalCardinality) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return take.hashCode() ^ refs.hashCode() ^ Util.hashCode(globalCardinality);
    }

    @Override
    public String toString() {
        return take + " . " + refs + (hasGlobalCardinality() ? " with global cardinality " + getGlobalCardinality() : "");
    }
}
