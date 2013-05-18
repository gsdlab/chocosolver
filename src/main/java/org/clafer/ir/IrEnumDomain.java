package org.clafer.ir;

import gnu.trove.iterator.TIntIterator;
import java.util.Arrays;
import org.clafer.collection.ArrayIntIterator;

/**
 *
 * @author jimmy
 */
public class IrEnumDomain extends IrDomain {

    private final int[] values;

    /**
     * @param values - sorted, unique, and immutable integers
     */
    public IrEnumDomain(int[] values) {
        if (values.length == 0) {
            throw new IllegalArgumentException();
        }
        this.values = values;
    }

    @Override
    public boolean isBounded() {
        return false;
    }

    @Override
    public boolean contains(int value) {
        return Arrays.binarySearch(values, value) >= 0;
    }

    @Override
    public int getLowerBound() {
        return values[0];
    }

    @Override
    public int getUpperBound() {
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
    public int[] getValues() {
        return values;
    }

    @Override
    public TIntIterator iterator() {
        return new ArrayIntIterator(values);
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
                if (size() != size()) {
                    return false;
                }
                int otherLow = other.getLowerBound();
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

    @Override
    public int hashCode() {
        return Arrays.hashCode(values);
    }

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
