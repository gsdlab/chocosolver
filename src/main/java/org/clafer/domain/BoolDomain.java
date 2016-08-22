package org.clafer.domain;

/**
 * Boolean domain.
 *
 * @author jimmy
 */
public class BoolDomain extends Domain {

    public static final BoolDomain TrueDomain = new BoolDomain(true, false);
    public static final BoolDomain FalseDomain = new BoolDomain(false, true);
    public static final BoolDomain TrueFalseDomain = new BoolDomain(true, true);

    private BoolDomain(boolean hasTrue, boolean hasFalse) {
        super(hasFalse ? 0 : 1, hasTrue ? 2 : 1);
    }

    public BoolDomain invert() {
        if (isTrue()) {
            return FalseDomain;
        }
        return isFalse() ? TrueDomain : TrueFalseDomain;
    }

    public boolean isTrue() {
        return bounds[0] == 1;
    }

    public boolean isFalse() {
        return bounds[1] == 1;
    }

    public boolean isUnknown() {
        return !isTrue() && !isFalse();
    }
}
