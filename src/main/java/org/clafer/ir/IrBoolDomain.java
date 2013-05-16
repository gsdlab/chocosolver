package org.clafer.ir;

/**
 *
 * @author jimmy
 */
public enum IrBoolDomain {

    TrueDomain(true, false),
    FalseDomain(false, true),
    BoolDomain(true, true);
    private final boolean hasTrue, hasFalse;

    private IrBoolDomain(boolean hasTrue, boolean hasFalse) {
        // At least one has to be true.
        this.hasTrue = hasTrue;
        this.hasFalse = hasFalse;
    }

    public boolean contains(boolean value) {
        return value ? hasTrue : hasFalse;
    }

    public boolean isConstant() {
        return hasTrue != hasFalse;
    }

    public int size() {
        if (hasTrue) {
            return hasFalse ? 2 : 1;
        }
        return 1;
    }

    public IrBoolDomain invert() {
        switch (this) {
            case TrueDomain:
                return FalseDomain;
            case FalseDomain:
                return TrueDomain;
            case BoolDomain:
                return BoolDomain;
            default:
                throw new IllegalStateException();
        }
    }
}
