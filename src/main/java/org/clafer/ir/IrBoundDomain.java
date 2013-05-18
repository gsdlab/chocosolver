package org.clafer.ir;

import gnu.trove.iterator.TIntIterator;
import org.clafer.collection.BoundIntIterator;

/**
 *
 * @author jimmy
 */
public class IrBoundDomain extends IrDomain {

    private final int low;
    private final int high;

    /**
     * @param low - Inclusive
     * @param high - Inclusive
     */
    public IrBoundDomain(int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException();
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
    public int getLowerBound() {
        return low;
    }

    @Override
    public int getUpperBound() {
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
    public int[] getValues() {
        int[] values = new int[size()];
        for (int i = 0; i < values.length; i++) {
            values[i] = low + i;
        }
        return values;
    }

    @Override
    public TIntIterator iterator() {
        return new BoundIntIterator(low, high);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof IrDomain) {
            IrDomain other = (IrDomain) obj;
            if (other.isEmpty()) {
                return false;
            }
            if (other.isBounded()) {
                return low == other.getLowerBound() && high == other.getUpperBound();
            }
            int[] otherValues = other.getValues();
            if (size() != other.size()) {
                return false;
            }
            for (int i = 0; i < otherValues.length; i++) {
                if (otherValues[i] != low + i) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return low ^ high;
    }

    @Override
    public String toString() {
        return "{" + low + ", ..., " + high + "}";
    }
}
