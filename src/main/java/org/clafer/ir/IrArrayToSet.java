package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 *
 * @author jimmy
 */
public class IrArrayToSet extends IrAbstractSet implements IrSetExpr {

    private final IrIntExpr[] array;

    IrArrayToSet(IrIntExpr[] array, IrDomain env, IrDomain ker, IrDomain card) {
        super(env, ker, card);
        this.array = Check.noNullsNotEmpty(array);
        if (ker.size() > array.length) {
            throw new IllegalArgumentException();
        }
    }

    public IrIntExpr[] getArray() {
        return array;
    }

    @Override
    public <A, B> B accept(IrSetExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrArrayToSet) {
            IrArrayToSet other = (IrArrayToSet) obj;
            return Arrays.equals(array, other.array) && super.equals(other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(array);
    }

    @Override
    public String toString() {
        return '{' + Util.commaSeparate(array) + '}';
    }
}
