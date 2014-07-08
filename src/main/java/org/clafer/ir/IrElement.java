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

    private final IrIntExpr[] array;
    private final IrIntExpr index;

    IrElement(IrIntExpr[] array, IrIntExpr index, Domain domain) {
        super(domain);
        this.array = Check.noNullsNotEmpty(array);
        this.index = Check.notNull(index);
    }

    public IrIntExpr[] getArray() {
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
            return Arrays.equals(array, other.array) && index.equals(other.index);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(array) ^ index.hashCode();
    }

    @Override
    public String toString() {
        return Arrays.toString(array) + "[" + index + "]";
    }
}
