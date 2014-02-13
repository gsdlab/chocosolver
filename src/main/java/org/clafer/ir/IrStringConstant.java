package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public class IrStringConstant extends IrStringVar implements IrConstant {

    private final String value;

    IrStringConstant(String value) {
        super(value, constants(value.toCharArray()), Irs.constant(value.length()));
        this.value = value;
    }

    private static IrIntVar[] constants(char[] value) {
        IrIntVar[] vars = new IrIntVar[value.length];
        for (int i = 0; i < vars.length; i++) {
            vars[i] = Irs.constant(value[i]);
        }
        return vars;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof IrStringConstant) {
            IrStringConstant other = (IrStringConstant) obj;
            return value.equals(other.value);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }

    @Override
    public String toString() {
        return value;
    }
}
