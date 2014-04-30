package org.clafer.ir;

import org.clafer.domain.Domain;
import java.util.Arrays;
import org.clafer.common.Check;

/**
 * Returns array[index].
 *
 * @author jimmy
 */
public class IrStringElement extends IrAbstractString {

    private final IrStringExpr[] array;
    private final IrIntExpr index;

    IrStringElement(IrStringExpr[] array, IrIntExpr index,
            Domain[] charDomains, Domain lengthDomain) {
        super(charDomains, lengthDomain);
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
        if (obj instanceof IrStringElement) {
            IrStringElement other = (IrStringElement) obj;
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
