package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;
import org.clafer.domain.Domain;

/**
 *
 * @author jimmy
 */
public class IrIntArrayVar extends IrAbstractIntArray {

    private final IrIntExpr[] array;

    IrIntArrayVar(IrIntExpr[] array) {
        super(getDomains(array));
        this.array = Check.noNulls(array);
    }

    private static Domain[] getDomains(IrIntExpr[] array) {
        Domain[] envDomains = new Domain[array.length];
        for (int i = 0; i < envDomains.length; i++) {
            envDomains[i] = array[i].getDomain();
        }
        return envDomains;
    }

    public IrIntExpr[] getArray() {
        return array;
    }

    @Override
    public <A, B> B accept(IrIntArrayExprVisitor<A, B> visitor, A a) {
        return visitor.visit(this, a);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrIntArrayVar) {
            IrIntArrayVar other = (IrIntArrayVar) obj;
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
        return Arrays.toString(array);
    }
}
