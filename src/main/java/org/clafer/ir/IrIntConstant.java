package org.clafer.ir;

import org.clafer.domain.BoundDomain;

/**
 *
 * @author jimmy
 */
public class IrIntConstant extends IrIntVar implements IrConstant {

    private final int value;

    IrIntConstant(int value) {
        super(Integer.toString(value), new BoundDomain(value, value));
        this.value = value;
    }

    public int getValue() {
        return value;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof IrIntConstant) {
            IrIntConstant other = (IrIntConstant) obj;
            return value == other.value;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return value;
    }

    @Override
    public String toString() {
        return Integer.toString(value);
    }
}
