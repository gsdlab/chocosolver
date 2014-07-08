package org.clafer.domain;

import gnu.trove.TIntCollection;
import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import org.clafer.collection.ArrayIntIterator;
import org.clafer.collection.ReverseArrayIntIterator;

/**
 * A domain over explicitly defined values.
 *
 * @author jimmy
 */
public class EnumDomain implements Domain {

    private final int[] values;

    /**
     * @param values sorted, unique, and immutable integers
     */
    public EnumDomain(int... values) {
        if (values.length == 0) {
            throw new IllegalArgumentException();
        }
        this.values = values;
    }

    @Override
    public boolean isBounded() {
        // Although technically the domain might be contigious, it is too expensive
        // to check.
        return false;
    }

    @Override
    public boolean contains(int value) {
        return Arrays.binarySearch(values, value) >= 0;
    }

    @Override
    public int getLowBound() {
        return values[0];
    }

    @Override
    public int getHighBound() {
        return values[values.length - 1];
    }

    @Override
    public boolean isEmpty() {
        return false;
    }

    @Override
    public int size() {
        return values.length;
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
        for (int i : values) {
            if (!superset.contains(i)) {
                return false;
            }
        }
        return true;
    }

    @Override
    public Domain insert(int value) {
        if (contains(value)) {
            return this;
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
        TIntHashSet remove = new TIntHashSet(size() - 1);
        for (int i : values) {
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
        if (low == getHighBound()) {
            return Domains.constantDomain(low);
        }
        if (low <= getLowBound()) {
            return this;
        }
        TIntHashSet boundLow = new TIntHashSet(
                Math.min(size(), getHighBound() - low + 1));
        for (int i : getValues()) {
            if (i >= low) {
                boundLow.add(i);
            }
        }
        return Domains.enumDomain(boundLow);
    }

    @Override
    public Domain boundHigh(int high) {
        if (high < getLowBound()) {
            return Domains.EmptyDomain;
        }
        if (high == getLowBound()) {
            return Domains.constantDomain(high);
        }
        if (high >= getHighBound()) {
            return this;
        }
        TIntHashSet boundHigh = new TIntHashSet(
                Math.min(size(), high - getLowBound() + 1));
        for (int i : getValues()) {
            if (i <= high) {
                boundHigh.add(i);
            }
        }
        return Domains.enumDomain(boundHigh);
    }

    @Override
    public Domain boundBetween(int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        if (low > getHighBound() || high < getLowBound()) {
            return Domains.EmptyDomain;
        }
        if (low == getHighBound()) {
            return Domains.constantDomain(low);
        }
        if (high == getLowBound()) {
            return Domains.constantDomain(high);
        }
        if (low <= getLowBound() && high >= getHighBound()) {
            return this;
        }
        TIntHashSet boundBetween = new TIntHashSet(
                Math.min(size(), high - low + 1));
        for (int i : getValues()) {
            if (i >= low && i <= high) {
                boundBetween.add(i);
            }
        }
        return Domains.enumDomain(boundBetween);
    }

    @Override
    public Domain minus() {
        int[] minus = new int[getValues().length];
        for (int i = 0; i < minus.length; i++) {
            minus[i] = -values[i];
        }
        return Domains.enumDomain(minus);
    }

    @Override
    public Domain difference(Domain other) {
        if (!intersects(other)) {
            return this;
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
        if (other.isBounded()
                && (getLowBound() >= other.getLowBound()
                || (getHighBound() <= other.getHighBound()))) {
            // Bounds are already checked.
            return true;
        }
        if (size() <= other.size()) {
            for (int i : getValues()) {
                if (other.contains(i)) {
                    return true;
                }
            }
        } else {
            for (int i = other.getLowBound(); i <= other.getHighBound(); i++) {
                if (contains(i)) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public Domain intersection(Domain other) {
        if (isSubsetOf(other)) {
            return this;
        }
        if (other.isSubsetOf(this)) {
            return other;
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
        int[] offset = new int[getValues().length];
        for (int i = 0; i < getValues().length; i++) {
            offset[i] = getValues()[i] + c;
        }
        return Domains.enumDomain(offset);
    }

    @Override
    public int[] getValues() {
        return values;
    }

    @Override
    public TIntIterator iterator() {
        return new ArrayIntIterator(values);
    }

    @Override
    public TIntIterator iterator(boolean increasing) {
        return increasing ? new ArrayIntIterator(values) : new ReverseArrayIntIterator(values);
    }

    @Override
    public void transferTo(TIntCollection collection) {
        collection.addAll(values);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof Domain) {
            Domain other = (Domain) obj;
            if (size() != other.size()) {
                return false;
            }
            return Arrays.equals(values, other.getValues());
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(values);
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        boolean elipse = false;
        result.append('{');
        result.append(values[0]);
        for (int i = 1; i < values.length; i++) {
            if (i > 0) {
                if (i < values.length - 1 && values[i - 1] + 1 == values[i] && values[i] + 1 == values[i + 1]) {
                    if (!elipse) {
                        result.append(", ...");
                        elipse = true;
                    }
                } else {
                    result.append(", ").append(values[i]);
                    elipse = false;
                }
            }
        }
        result.append('}');
        return result.toString();
    }
}
