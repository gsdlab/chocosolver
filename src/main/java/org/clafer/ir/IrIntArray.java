package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class IrIntArray implements IrExpr {
    
    private final IrIntExpr[] array;
    
    IrIntArray(IrIntExpr[] array) {
        this.array = Check.noNullsNotEmpty(array);
    }

    public IrIntExpr[] getArray() {
        return array;
    }
    
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof IrIntArray) {
            IrIntArray other = (IrIntArray) obj;
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
