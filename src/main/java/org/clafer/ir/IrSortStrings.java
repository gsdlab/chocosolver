package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class IrSortStrings extends IrAbstractBool implements IrBoolExpr {

    private final IrIntExpr[][] strings;

    IrSortStrings(IrIntExpr[][] strings, IrBoolDomain domain) {
        super(domain);
        this.strings = Check.noNulls(strings);
    }

    public IrIntExpr[][] getStrings() {
        return strings;
    }

    @Override
    public IrBoolExpr negate() {
        IrIntExpr[][] reverse = strings;
        Util.reverse(reverse);
        return new IrSortStrings(reverse, getDomain().invert());
    }

    @Override
    public boolean isNegative() {
        return false;
    }

    @Override
    public <A, B> B accept(IrBoolExprVisitor<A, B> visitor, A a) {
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
