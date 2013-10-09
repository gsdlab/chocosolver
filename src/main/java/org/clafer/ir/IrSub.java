package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 * foldl1 (-) subtrahends
 * 
 * @author jimmy
 */
public class IrSub extends IrAbstractInt {

    private final IrIntExpr[] subtrahends;

    IrSub(IrIntExpr[] subtrahends, IrDomain domain) {
        super(domain);
        this.subtrahends = Check.noNullsNotEmpty(subtrahends);
    }

    public IrIntExpr[] getSubtrahends() {
        return subtrahends;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor< A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrSub) {
            IrSub other = (IrSub) obj;
            return Arrays.equals(subtrahends, other.subtrahends) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(subtrahends);
    }

    @Override
    public String toString() {
        return "(" + Util.intercalate(") - (", getSubtrahends()) + ")";
    }
}
