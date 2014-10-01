package org.clafer.ir;

import org.clafer.common.Check;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class IrCountNotEqual extends IrAbstractInt {

    private final int value;
    private final IrIntArrayExpr array;

    public IrCountNotEqual(int value, IrIntArrayExpr array, Domain domain) {
        super(domain);
        this.array = Check.notNull(array);
        this.value = Check.notNull(value);
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
        if (obj instanceof IrCountNotEqual) {
            IrCountNotEqual other = (IrCountNotEqual) obj;
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
        return "countNotEqual(" + value + " in " + array + ")";
    }
}
