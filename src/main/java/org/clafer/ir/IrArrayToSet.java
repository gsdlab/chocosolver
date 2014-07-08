package org.clafer.ir;

import org.clafer.domain.Domain;
import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class IrArrayToSet extends IrAbstractSet {

    private final IrIntExpr[] array;
    private final Integer globalCardinality;

    IrArrayToSet(IrIntExpr[] array, Domain env, Domain ker, Domain card, Integer globalCardinality) {
        super(env, ker, card);
        this.array = Check.noNullsNotEmpty(array);
        if (ker.size() > array.length) {
            throw new IllegalArgumentException();
        }
        this.globalCardinality = globalCardinality;
    }

    public IrIntExpr[] getArray() {
        return array;
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
        if (obj instanceof IrArrayToSet) {
            IrArrayToSet other = (IrArrayToSet) obj;
            return Arrays.equals(array, other.array) && Util.equals(globalCardinality, other.globalCardinality) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(array) ^ Util.hashCode(globalCardinality);
    }

    @Override
    public String toString() {
        return '{' + Util.commaSeparate(array)
                + (hasGlobalCardinality() ? " with global cardinality " + getGlobalCardinality() : "")
                + '}';
    }
}
