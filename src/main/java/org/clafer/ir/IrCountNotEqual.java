package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class IrCountNotEqual extends IrAbstractInt {

    private final int value;
    private final IrIntExpr[] array;

    public IrCountNotEqual(int value, IrIntExpr[] array, Domain domain) {
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
        if (obj instanceof IrCountNotEqual) {
            IrCountNotEqual other = (IrCountNotEqual) obj;
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
        return "countNotEqual(" + value + " in " + Arrays.toString(array) + ")";
    }
}
