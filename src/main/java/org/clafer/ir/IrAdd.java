package org.clafer.ir;

import org.clafer.domain.Domain;
import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 * offset + foldl (+) addends
 *
 * @author jimmy
 */
public class IrAdd extends IrAbstractInt {

    private final IrIntExpr[] addends;
    private final int offset;

    IrAdd(IrIntExpr[] addends, int offset, Domain domain) {
        super(domain);
        this.addends = Check.noNullsNotEmpty(addends);
        this.offset = offset;
    }

    public IrIntExpr[] getAddends() {
        return addends;
    }

    public int getOffset() {
        return offset;
    }

    @Override
    public <A, B> B accept(IrIntExprVisitor< A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrAdd) {
            IrAdd other = (IrAdd) obj;
            return Arrays.equals(addends, other.addends) && offset == other.offset && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(addends) ^ offset;
    }

    @Override
    public String toString() {
        return "(" + Util.intercalate(") + (", getAddends()) + ")" + (offset == 0 ? "" : " + "
                + offset);
    }
}
