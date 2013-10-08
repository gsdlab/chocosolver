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
    private final boolean strict;

    IrSortStrings(IrIntExpr[][] strings, boolean strict, IrBoolDomain domain) {
        super(domain);
        this.strings = Check.noNullsNotEmpty(strings);
        this.strict = strict;
    }

    public IrIntExpr[][] getStrings() {
        return strings;
    }

    public boolean isStrict() {
        return strict;
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
    public <A, B> B accept(IrIntExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrSortStrings) {
            IrSortStrings other = (IrSortStrings) obj;
            return Arrays.deepEquals(strings, other.strings) && strict == other.strict;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.deepHashCode(strings) ^ (strict ? 1 : 0);
    }
}
