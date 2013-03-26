package org.clafer.ir;

import java.util.Arrays;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class IrSort implements IrConstraint {

    private final IrIntExpr[] array;

    IrSort(IrIntExpr[] array) {
        this.array = Check.noNulls(array);
    }

    public IrIntExpr[] getArray() {
        return array;
    }

    @Override
    public <A, B> B accept(IrConstraintVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public String toString() {
        return "sort(" + Arrays.toString(array) + ")";
    }
}
