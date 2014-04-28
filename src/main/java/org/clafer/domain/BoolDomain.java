package org.clafer.domain;

import gnu.trove.TIntCollection;
import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.hash.TIntHashSet;
import org.clafer.collection.BoundIntIterator;
import org.clafer.collection.ReverseBoundIntIterator;
import org.clafer.collection.SingleIntIterator;

/**
 * Boolean domain.
 *
 * @author jimmy
 */
public enum BoolDomain implements Domain {

    TrueDomain(true, false),
    FalseDomain(false, true),
    TrueFalseDomain(true, true);
    private final boolean hasTrue, hasFalse;

    private BoolDomain(boolean hasTrue, boolean hasFalse) {
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
     * @return {@code true} if and only if the domain contains the
     * {@code value}, {@code false} otherwise
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

    @Override
    public boolean isSubsetOf(Domain superset) {
        return (!hasTrue || superset.contains(1))
                && (!hasFalse || superset.contains(0));
    }

    @Override
    public boolean intersects(Domain other) {
        return (hasTrue && other.contains(1))
                || (hasFalse && other.contains(0));
    }

    @Override
    public Domain insert(int value) {
        switch (value) {
            case 0:
                return hasFalse ? this : TrueFalseDomain;
            case 1:
                return hasTrue ? this : TrueFalseDomain;
            default:
                if (value == getLowBound() - 1) {
                    return Domains.boundDomain(getLowBound() - 1, getHighBound());
                }
                if (value == getHighBound() + 1) {
                    return Domains.boundDomain(getLowBound(), getHighBound() + 1);
                }
                TIntHashSet insert = new TIntHashSet(size() + 1);
                transferTo(insert);
                insert.add(value);
                return Domains.enumDomain(insert);
        }
    }

    @Override
    public Domain remove(int value) {
        switch (value) {
            case 0:
                return hasFalse ? (hasTrue ? TrueDomain : Domains.EmptyDomain) : this;
            case 1:
                return hasTrue ? (hasFalse ? FalseDomain : Domains.EmptyDomain) : this;
            default:
                return this;
        }
    }

    @Override
    public Domain boundLow(int low) {
        if (low > 1) {
            return Domains.EmptyDomain;
        }
        if (hasFalse && low == 1) {
            return hasTrue ? TrueDomain : Domains.EmptyDomain;
        }
        return this;
    }

    @Override
    public Domain boundHigh(int high) {
        if (high < 0) {
            return Domains.EmptyDomain;
        }
        if (hasTrue && high == 0) {
            return hasFalse ? FalseDomain : Domains.EmptyDomain;
        }
        return this;
    }

    @Override
    public Domain boundBetween(int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        if (low > 1 || high < 0) {
            return Domains.EmptyDomain;
        }
        if (low == 1) {
            return hasTrue ? TrueDomain : Domains.EmptyDomain;
        }
        if (high == 0) {
            return hasFalse ? FalseDomain : Domains.EmptyDomain;
        }
        return this;
    }

    @Override
    public Domain minus() {
        switch (this) {
            case TrueDomain:
                return Domains.constantDomain(-1);
            case FalseDomain:
                return this;
            default:
                return Domains.boundDomain(-1, 0);
        }
    }

    @Override
    public Domain difference(Domain other) {
        return hasTrue && !other.contains(1)
                ? (hasFalse && !other.contains(0) ? this : TrueDomain)
                : (hasFalse && !other.contains(0) ? FalseDomain : Domains.EmptyDomain);
    }

    @Override
    public Domain intersection(Domain other) {
        return hasTrue && other.contains(1)
                ? (hasFalse && other.contains(0) ? this : TrueDomain)
                : (hasFalse && other.contains(0) ? FalseDomain : Domains.EmptyDomain);
    }

    @Override
    public Domain union(Domain other) {
        if (isSubsetOf(other)) {
            return other;
        }
        if (other.isSubsetOf(this)) {
            return this;
        }
        if (other.isBounded()) {
            if (getLowBound() <= other.getLowBound()
                    && getHighBound() >= other.getLowBound()) {
                return Domains.boundDomain(getLowBound(), other.getHighBound());
            }
            if (other.getLowBound() <= getLowBound()
                    && other.getHighBound() >= getLowBound()) {
                return Domains.boundDomain(other.getLowBound(), getHighBound());
            }
        }
        TIntHashSet union = new TIntHashSet(size() + other.size());
        transferTo(union);
        other.transferTo(union);
        return Domains.enumDomain(union);
    }

    @Override
    public Domain offset(int c) {
        if (c == 0) {
            return this;
        }
        return Domains.boundDomain(getLowBound() + c, getHighBound() + c);
    }

    /**
     * Reverse the domain. Maps {0}->{1}, {1}->{0}, and {0,1}->{0,1}.
     *
     * @return the inverted domain
     */
    public BoolDomain invert() {
        switch (this) {
            case TrueDomain:
                return FalseDomain;
            case FalseDomain:
                return TrueDomain;
            case TrueFalseDomain:
                return TrueFalseDomain;
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
            case TrueFalseDomain:
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
            case TrueFalseDomain:
                return increasing ? new BoundIntIterator(0, 1) : new ReverseBoundIntIterator(0, 1);
            default:
                throw new IllegalStateException();
        }
    }

    @Override
    public void transferTo(TIntCollection collection) {
        if (hasTrue) {
            collection.add(1);
        }
        if (hasFalse) {
            collection.add(0);
        }
    }

    @Override
    public String toString() {
        switch (this) {
            case TrueDomain:
                return "{1}";
            case FalseDomain:
                return "{0}";
            case TrueFalseDomain:
                return "{0, 1}";
            default:
                throw new IllegalStateException();
        }
    }
}
