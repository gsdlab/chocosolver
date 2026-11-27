package org.clafer.ir;

import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class IrSubarray extends IrAbstractIntArray {

    private final IrIntArrayExpr array;
    private final IrIntExpr index, sublength;

    public IrSubarray(IrIntArrayExpr array, IrIntExpr index, IrIntExpr sublength, Domain[] charDomains) {
        super(charDomains);
        this.array = array;
        this.index = index;
        this.sublength = sublength;
    }

    public IrIntArrayExpr getArray() {
        return array;
    }

    public IrIntExpr getIndex() {
        return index;
    }

    public IrIntExpr getSublength() {
        return sublength;
    }

    @Override
    public <A, B> B accept(IrIntArrayExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrSubarray) {
            IrSubarray other = (IrSubarray) obj;
            return array.equals(other.array) && index.equals(other.index) && sublength.equals(other.sublength);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return array.hashCode() ^ index.hashCode() ^ sublength.hashCode();
    }

    @Override
    public String toString() {
        return "subarray(" + array + ", " + index + ", " + sublength + ")";
    }
}
