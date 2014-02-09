package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 * Returns array[index].
 *
 * @author jimmy
 */
public class IrElementString extends IrAbstractString {

    private final IrStringExpr[] array;
    private final IrIntExpr index;

    IrElementString(IrStringExpr[] array, IrIntExpr index,
            IrDomain lengthDomain, IrDomain[] charDomains) {
        super(lengthDomain, charDomains);
        this.array = Check.noNullsNotEmpty(array);
        this.index = Check.notNull(index);
    }

    public IrStringExpr[] getArray() {
        return array;
    }

    public IrIntExpr getIndex() {
        return index;
    }

    @Override
    public <A, B> B accept(IrStringExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrElementString) {
            IrElementString other = (IrElementString) obj;
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
