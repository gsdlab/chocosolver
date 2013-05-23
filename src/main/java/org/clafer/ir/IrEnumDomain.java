package org.clafer.ir;

import gnu.trove.iterator.TIntIterator;
import java.util.Arrays;
import org.clafer.collection.ArrayIntIterator;
import org.clafer.collection.ReverseArrayIntIterator;

/**
 * A domain over explicitly defined values.
 * 
 * @author jimmy
 */
public class IrEnumDomain implements IrDomain {

    private final int[] values;

    /**
     * @param values sorted, unique, and immutable integers
     */
    public IrEnumDomain(int[] values) {
        if (values.length == 0) {
            throw new IllegalArgumentException();
        }
        this.values = values;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isBounded() {
        // Although technically the domain might be contigious, it is too expensive
        // to check.
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean contains(int value) {
        return Arrays.binarySearch(values, value) >= 0;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getLowBound() {
        return values[0];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getHighBound() {
        return values[values.length - 1];
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isEmpty() {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int size() {
        return values.length;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int[] getValues() {
        return values;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TIntIterator iterator() {
        return new ArrayIntIterator(values);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TIntIterator iterator(boolean increasing) {
        return increasing ? new ArrayIntIterator(values) : new ReverseArrayIntIterator(values);
    }

    /**
     * {@inheritDoc}
     */
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
                if (size() != size()) {
                    return false;
                }
                int otherLow = other.getLowBound();
                for (int i = 0; i < values.length; i++) {
                    if (values[i] != otherLow + i) {
                        return false;
                    }
                }
                return true;
            }
            return Arrays.equals(values, other.getValues());
        }
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return Arrays.hashCode(values);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append('{');
        for (int i = 0; i < values.length; i++) {
            if (i > 0) {
                result.append(", ");
            }
            result.append(values[i]);
        }
        result.append('}');
        return result.toString();
    }
}
