package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class IrSortInts extends IrAbstractBool implements IrBoolExpr {

    private final IrIntExpr[] array;

    IrSortInts(IrIntExpr[] array, IrBoolDomain domain) {
        super(domain);
        this.array = Check.noNullsNotEmpty(array);
    }

    public IrIntExpr[] getArray() {
        return array;
    }

    @Override
    public boolean isNegative() {
        return false;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrNot(this, getDomain().invert());
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrSortInts) {
            IrSortInts other = (IrSortInts) obj;
            return Arrays.equals(array, other.array);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(array);
    }

    @Override
    public String toString() {
        return "sort(" + Util.commaSeparate(array) + ")";
    }
}
