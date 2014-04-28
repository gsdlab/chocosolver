package org.clafer.ir;

import org.clafer.domain.Domain;
import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrCount extends IrAbstractInt {

    private final int value;
    private final IrIntExpr[] array;

    public IrCount(int value, IrIntExpr[] array, Domain domain) {
        super(domain);
        this.array = Check.noNullsNotEmpty(array);
        this.value = Check.notNull(value);
}

    public int getValue() {
        return value;
    }

    public IrIntExpr[] getArray() {
        return array;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrCount) {
            IrCount other = (IrCount) obj;
            return value == other.value && Arrays.equals(array, other.array)
                    && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return value ^ Arrays.hashCode(array);
    }

    @Override
    public String toString() {
        return "count(" + value + " in " + Arrays.toString(array) + ")";
    }
}
