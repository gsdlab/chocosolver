package org.clafer.ir;

import org.clafer.domain.Domain;
import java.util.Arrays;
import org.clafer.common.Check;

/**
 * Returns array[index].
 *
 * @author jimmy
 */
public class IrElement extends IrAbstractInt {

    private final IrIntArrayExpr array;
    private final IrIntExpr index;

    IrElement(IrIntArrayExpr array, IrIntExpr index, Domain domain) {
        super(domain);
        this.array = Check.notNull(array);
        this.index = Check.notNull(index);
    }

    public IrIntArrayExpr getArray() {
        return array;
    }

    public IrIntExpr getIndex() {
        return index;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrElement) {
            IrElement other = (IrElement) obj;
            return array.equals(other.array) && index.equals(other.index);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return array.hashCode() ^ index.hashCode();
    }

    @Override
    public String toString() {
        return array + "[" + index + "]";
    }
}
