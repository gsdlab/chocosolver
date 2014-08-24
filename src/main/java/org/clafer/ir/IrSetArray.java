package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrSetArray implements IrExpr {

    private final IrSetExpr[] array;

    IrSetArray(IrSetExpr[] array) {
        this.array = Check.noNullsNotEmpty(array);
    }

    public IrSetExpr[] getArray() {
        return array;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrIntArray) {
            IrSetArray other = (IrSetArray) obj;
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
