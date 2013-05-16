package org.clafer.ir;

import gnu.trove.iterator.TIntIterator;
import java.util.Arrays;
import org.clafer.Check;
import org.clafer.ir.IrDomain.IrBoundDomain;

/**
 * Returns array[index].
 * 
 * @author jimmy
 */
public class IrElement implements IrIntExpr {

    private final IrIntExpr[] array;
    private final IrIntExpr index;

    IrElement(IrIntExpr[] array, IrIntExpr index) {
        this.array = Check.notNull(array);
        this.index = Check.notNull(index);
    }

    public IrIntExpr getIndex() {
        return index;
    }

    public IrIntExpr[] getArray() {
        return array;
    }

    @Override
    public IrDomain getDomain() {
        TIntIterator iter = index.getDomain().iterator();
        assert iter.hasNext();

        IrDomain domain = array[iter.next()].getDomain();
        int low = domain.getLowerBound();
        int high = domain.getUpperBound();
        while (iter.hasNext()) {
            domain = array[iter.next()].getDomain();
            low = Math.min(low, domain.getLowerBound());
            high = Math.max(high, domain.getUpperBound());
        }
        return new IrBoundDomain(low, high);
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return Arrays.toString(array) + "[" + index + "]";
    }
}
