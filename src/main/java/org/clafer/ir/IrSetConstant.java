package org.clafer.ir;

import org.clafer.domain.Domain;
import java.util.Arrays;

/**
 *
 * @author jimmy
 */
public class IrSetConstant extends IrSetVar implements IrConstant {

    private final int[] value;

    IrSetConstant(Domain value) {
        super(value.toString(), value, value, Irs.constant(value.size()));
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
