package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSortStringsChannel extends IrAbstractBool implements IrBoolExpr {

    private final IrIntExpr[][] strings;
    private final IrIntExpr[] ints;

    public IrSortStringsChannel(IrIntExpr[][] strings, IrIntExpr[] ints, IrBoolDomain domain) {
        super(domain);
        this.strings = Check.noNullsNotEmpty(strings);
        for (IrIntExpr[] string : strings) {
            Check.noNullsNotEmpty(string);
            if (string.length != strings[0].length) {
                throw new IllegalArgumentException();
            }
        }
        this.ints = Check.noNullsNotEmpty(ints);
        if (strings.length != ints.length) {
            throw new IllegalArgumentException();
        }
    }

    public IrIntExpr[][] getStrings() {
        return strings;
    }

    public IrIntExpr[] getInts() {
        return ints;
    }

    @Override
    public IrBoolExpr negate() {
        return new IrNot(this, getDomain().invert());
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
        if (obj instanceof IrSortStringsChannel) {
            IrSortStringsChannel other = (IrSortStringsChannel) obj;
            return Arrays.deepEquals(strings, other.strings)
                    && Arrays.equals(ints, other.ints)
                    && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.deepHashCode(strings) ^ Arrays.hashCode(ints);
    }

    @Override
    public String toString() {
        return "lexChainChannel(" + Arrays.deepToString(strings) + ", " + Arrays.toString(ints) + ")";
    }
}
