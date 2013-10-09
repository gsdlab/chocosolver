package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 * foldl (+) addends
 *
 * @author jimmy
 */
public class IrAdd extends IrAbstractInt {

    private final IrIntExpr[] addends;

    IrAdd(IrIntExpr[] addends, IrDomain domain) {
        super(domain);
        this.addends = Check.noNullsNotEmpty(addends);
    }

    public IrIntExpr[] getAddends() {
        return addends;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor< A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrAdd) {
            IrAdd other = (IrAdd) obj;
            return Arrays.equals(addends, other.addends) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(addends);
    }

    @Override
    public String toString() {
        return "(" + Util.intercalate(") + (", getAddends()) + ")";
    }
}
