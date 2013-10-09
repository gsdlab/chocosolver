package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class IrJoinFunction extends IrAbstractSet {

    private final IrSetExpr take;
    private final IrIntExpr[] refs;
    private final Integer globalCardinality;

    IrJoinFunction(IrSetExpr take, IrIntExpr[] refs, IrDomain env, IrDomain ker, IrDomain card, Integer globalCardinality) {
        super(env, ker, card);
        this.take = Check.notNull(take);
        this.refs = Check.noNullsNotEmpty(refs);
        this.globalCardinality = globalCardinality;
    }

    public IrSetExpr getTake() {
        return take;
    }

    public IrIntExpr[] getRefs() {
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
            return take.equals(other.take) && Arrays.equals(refs, other.refs) && Util.equals(globalCardinality, other.globalCardinality) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return take.hashCode() ^ Arrays.hashCode(refs) ^ Util.hashCode(globalCardinality);
    }

    @Override
    public String toString() {
        return take + " . " + Arrays.toString(refs)
                + (hasGlobalCardinality() ? " with global cardinality " + getGlobalCardinality() : "");
    }
}
