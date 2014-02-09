package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public class IrBoolConstant extends IrBoolVar implements IrConstant {

    private final boolean value;

    IrBoolConstant(boolean value) {
        super(Boolean.toString(value), value ? IrBoolDomain.TrueDomain : IrBoolDomain.FalseDomain);
        this.value = value;
    }

    public boolean getValue() {
        return value;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof IrBoolConstant) {
            IrBoolConstant other = (IrBoolConstant) obj;
            return value == other.value;
        }
        return false;
    }

    @Override
    public int hashCode() {
        // Same values as java.lang.Boolean.hashCode
        return value ? 1231 : 1237;
    }

    @Override
    public String toString() {
        return getName();
    }
}
