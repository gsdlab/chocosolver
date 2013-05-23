package org.clafer.ir;

/**
 * Boolean domain.
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

    /**
     * @param value test this value
     * @return {@code true} if and only if the domain contains the {@code value},
     *         {@code false} otherwise
     */
    public boolean contains(boolean value) {
        return value ? hasTrue : hasFalse;
    }

    /**
     * @return the size of the domain
     */
    public int size() {
        return hasTrue && hasFalse ? 2 : 1;
    }

    /**
     * Reverse the domain. Maps {0}->{1}, {1}->{0}, and {0,1}->{0,1}.
     *
     * @return the inverted domain
     */
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
