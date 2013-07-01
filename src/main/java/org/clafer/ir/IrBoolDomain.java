package org.clafer.ir;

import gnu.trove.iterator.TIntIterator;
import org.clafer.collection.BoundIntIterator;
import org.clafer.collection.ReverseBoundIntIterator;
import org.clafer.collection.SingleIntIterator;

/**
 * Boolean domain.
 *
 * @author jimmy
 */
public enum IrBoolDomain implements IrDomain {

    TrueDomain(true, false),
    FalseDomain(false, true),
    BoolDomain(true, true);
    private final boolean hasTrue, hasFalse;

    private IrBoolDomain(boolean hasTrue, boolean hasFalse) {
        assert hasTrue || hasFalse;
        // At least one has to be true.
        this.hasTrue = hasTrue;
        this.hasFalse = hasFalse;
    }

    @Override
    public boolean isBounded() {
        return true;
    }

    /**
     * @param value test this value
     * @return {@code true} if and only if the domain contains the {@code value},
     *         {@code false} otherwise
     */
    public boolean contains(boolean value) {
        return value ? hasTrue : hasFalse;
    }

    @Override
    public boolean contains(int value) {
        switch (value) {
            case 0:
                return hasFalse;
            case 1:
                return hasTrue;
            default:
                return false;
        }
    }

    @Override
    public int getLowBound() {
        return hasFalse ? 0 : 1;
    }

    @Override
    public int getHighBound() {
        return hasTrue ? 1 : 0;
    }

    @Override
    public boolean isEmpty() {
        return false;
    }

    @Override
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

    @Override
    public int[] getValues() {
        switch (this) {
            case TrueDomain:
                return new int[]{1};
            case FalseDomain:
                return new int[]{0};
            case BoolDomain:
                return new int[]{0, 1};
            default:
                throw new IllegalStateException();
        }
    }

    @Override
    public TIntIterator iterator() {
        return iterator(true);
    }

    @Override
    public TIntIterator iterator(boolean increasing) {
        switch (this) {
            case TrueDomain:
                return new SingleIntIterator(1);
            case FalseDomain:
                return new SingleIntIterator(0);
            case BoolDomain:
                return increasing ? new BoundIntIterator(0, 1) : new ReverseBoundIntIterator(0, 1);
            default:
                throw new IllegalStateException();
        }
    }
}
