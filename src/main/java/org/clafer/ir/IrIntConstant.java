package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public class IrIntConstant extends IrIntVar {

    private final int value;

    IrIntConstant(int value) {
        super(Integer.toString(value), new IrBoundDomain(value, value));
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
            // Don't need to call super.hashCode since the domain is the same as value.
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
