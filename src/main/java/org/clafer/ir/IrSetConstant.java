package org.clafer.ir;

import java.util.Arrays;

/**
 *
 * @author jimmy
 */
public class IrSetConstant extends IrSetVar implements IrConstant {

    private final int[] value;

    IrSetConstant(IrDomain value) {
        super(value.toString(), value, value, new IrBoundDomain(value.size(), value.size()));
        this.value = value.getValues();
    }

    public int[] getValue() {
        return value;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof IrSetConstant) {
            IrSetConstant other = (IrSetConstant) obj;
            // Don't need to call super.hashCode since the domain is the same as value.
            return Arrays.equals(value, other.value);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(value);
    }

    @Override
    public String toString() {
        return Arrays.toString(value);
    }
}
