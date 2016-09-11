package org.clafer.ir;

import java.util.Objects;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class IrCount extends IrAbstractInt {

    private final int value;
    private final IrIntArrayExpr array;

    public IrCount(int value, IrIntArrayExpr array, Domain domain) {
        super(domain);
        this.array = Objects.requireNonNull(array);
        this.value = Objects.requireNonNull(value);
    }

    public int getValue() {
        return value;
    }

    public IrIntArrayExpr getArray() {
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
            return value == other.value && array.equals(other.array);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return value ^ array.hashCode();
    }

    @Override
    public String toString() {
        return "count(" + value + " in " + array + ")";
    }
}
