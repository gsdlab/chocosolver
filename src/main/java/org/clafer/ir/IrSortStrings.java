package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSortStrings implements IrConstraint {

    private final IrIntExpr[][] strings;

    IrSortStrings(IrIntExpr[][] strings) {
        this.strings = Check.noNulls(strings);
    }

    public IrIntExpr[][] getStrings() {
        return strings;
    }

    @Override
    public <A, B> B accept(IrConstraintVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrSortStrings) {
            IrSortStrings other = (IrSortStrings) obj;
            return Arrays.deepEquals(strings, other.strings);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.deepHashCode(strings);
    }

    @Override
    public String toString() {
        return super.toString();
    }
}
