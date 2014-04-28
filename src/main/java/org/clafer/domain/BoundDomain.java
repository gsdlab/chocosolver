package org.clafer.domain;

import gnu.trove.TIntCollection;
import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.hash.TIntHashSet;
import org.clafer.collection.BoundIntIterator;
import org.clafer.collection.ReverseBoundIntIterator;

/**
 * A contiguous domain between a low and high bound.
 *
 * @author jimmy
 */
public class BoundDomain implements Domain {

    private final int low;
    private final int high;

    /**
     * @param low lowest value in the domain, inclusive
     * @param high highest value in the domain, inclusive
     */
    public BoundDomain(int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException(low + ">" + high);
        }
        this.low = low;
        this.high = high;
    }

    @Override
    public boolean isBounded() {
        return true;
    }

    @Override
    public boolean contains(int value) {
        return value >= low && value <= high;
    }

    @Override
    public int getLowBound() {
        return low;
    }

    @Override
    public int getHighBound() {
        return high;
    }

    @Override
    public boolean isEmpty() {
        return false;
    }

    @Override
    public int size() {
        return high + 1 - low;
    }

    @Override
    public boolean isSubsetOf(Domain superset) {
        if (this == superset) {
            return true;
        }
        if (size() > superset.size()) {
            return false;
        }
        if (getLowBound() < superset.getLowBound()
                || getHighBound() > superset.getHighBound()) {
            return false;
        }
        if (superset.isBounded()) {
            return true;
        }
        for (int i = low; i <= high; i++) {
            if (!superset.contains(i)) {
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean intersects(Domain other) {
        if (other.isEmpty()) {
            return false;
        }
        if (contains(other.getLowBound()) || contains(other.getHighBound())) {
            return true;
        }
        if (other.contains(getLowBound()) || other.contains(getHighBound())) {
            return true;
        }
        if (getLowBound() > other.getHighBound()
                || getHighBound() < other.getLowBound()) {
            return false;
        }
        if (other.getLowBound() >= getLowBound()
                || other.getHighBound() <= getHighBound()) {
            return true;
        }
        if (other.isBounded()
                && (getLowBound() >= other.getLowBound()
                || getHighBound() <= other.getHighBound())) {
            // Bounds are already checked.
            return true;
        }
        if (size() <= other.size()) {
            for (int i = getLowBound(); i <= getHighBound(); i++) {
                if (other.contains(i)) {
                    return true;
                }
            }
        } else {
            for (int i : other.getValues()) {
                if (contains(i)) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public Domain insert(int value) {
        if (contains(value)) {
            return this;
        }
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

    @Override
    public Domain remove(int value) {
        if (!contains(value)) {
            return this;
        }
        if (value == getLowBound()) {
            return size() == 1
                    ? Domains.EmptyDomain
                    : Domains.boundDomain(getLowBound() + 1, getHighBound());
        }
        if (value == getHighBound()) {
            return size() == 1
                    ? Domains.EmptyDomain
                    : Domains.boundDomain(getLowBound(), getHighBound() - 1);
        }
        TIntHashSet remove = new TIntHashSet(size() - 1);
        for (int i = low; i <= high; i++) {
            if (i != value) {
                remove.add(i);
            }
        }
        return Domains.enumDomain(remove);
    }

    @Override
    public Domain boundLow(int low) {
        if (low > getHighBound()) {
            return Domains.EmptyDomain;
        }
        if (low <= getLowBound()) {
            return this;
        }
        return Domains.boundDomain(low, getHighBound());
    }

    @Override
    public Domain boundHigh(int high) {
        if (high < getLowBound()) {
            return Domains.EmptyDomain;
        }
        if (high >= getHighBound()) {
            return this;
        }
        return Domains.boundDomain(getLowBound(), high);
    }

    @Override
    public Domain boundBetween(int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        if (low > getHighBound() || high < getLowBound()) {
            return Domains.EmptyDomain;
        }
        if (low <= getLowBound() && high >= getHighBound()) {
            return this;
        }
        return Domains.boundDomain(
                Math.max(low, getLowBound()),
                Math.min(high, getHighBound()));
    }

    @Override
    public Domain minus() {
        return Domains.boundDomain(-getHighBound(), -getLowBound());
    }

    @Override
    public Domain difference(Domain other) {
        if (!intersects(other)) {
            return this;
        }
        if (other.isBounded()) {
            if (getLowBound() < other.getLowBound()
                    && getHighBound() <= other.getHighBound()) {
                return Domains.boundDomain(getLowBound(), other.getLowBound() - 1);
            }
            if (getHighBound() > other.getHighBound()
                    && getLowBound() >= other.getLowBound()) {
                return Domains.boundDomain(other.getHighBound() + 1, getHighBound());
            }
        }
        TIntHashSet difference = new TIntHashSet(size());
        transferTo(difference);
        difference.removeAll(other.getValues());
        if (size() == difference.size()) {
            return this;
        }
        return Domains.enumDomain(difference);
    }

    @Override
    public Domain intersection(Domain other) {
        if (isSubsetOf(other)) {
            return this;
        }
        if (other.isSubsetOf(this)) {
            return other;
        }
        if (other.isBounded()) {
            if (getLowBound() <= other.getLowBound()
                    && getHighBound() >= other.getLowBound()) {
                return Domains.boundDomain(other.getLowBound(), getHighBound());
            }
            if (other.getLowBound() <= getLowBound()
                    && other.getHighBound() >= getLowBound()) {
                return Domains.boundDomain(getLowBound(), other.getHighBound());
            }
        }
        TIntHashSet intersection = new TIntHashSet(size());
        transferTo(intersection);
        intersection.retainAll(other.getValues());
        return Domains.enumDomain(intersection);
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

    @Override
    public int[] getValues() {
        int[] values = new int[size()];
        for (int i = 0; i < values.length; i++) {
            values[i] = low + i;
        }
        return values;
    }

    @Override
    public TIntIterator iterator() {
        return iterator(true);
    }

    @Override
    public TIntIterator iterator(boolean increasing) {
        return increasing ? new BoundIntIterator(low, high) : new ReverseBoundIntIterator(low, high);
    }

    @Override
    public void transferTo(TIntCollection collection) {
        for (int i = low; i <= high; i++) {
            collection.add(i);
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof Domain) {
            Domain other = (Domain) obj;
            return size() == other.size()
                    && low == other.getLowBound()
                    && high == other.getHighBound();
        }
        return false;
    }

    @Override
    public int hashCode() {
        return low ^ high;
    }

    @Override
    public String toString() {
        if (low == high) {
            return "{" + low + "}";
        }
        if (low + 1 == high) {
            return "{" + low + ", " + high + "}";
        }
        return "{" + low + ", ..., " + high + "}";
    }
}
