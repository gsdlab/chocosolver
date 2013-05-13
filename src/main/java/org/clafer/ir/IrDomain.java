package org.clafer.ir;

import gnu.trove.TIntCollection;
import gnu.trove.iterator.TIntIterator;
import java.util.Arrays;
import org.clafer.collection.ArrayIntIterator;
import org.clafer.collection.BoundIntIterator;
import org.clafer.collection.EmptyIntIterator;
import org.clafer.collection.SingleIntIterator;

/**
 *
 * @author jimmy
 */
public abstract class IrDomain {

    private IrDomain() {
    }

    public abstract boolean isBounded();

    public abstract boolean contains(int value);

    public abstract int getLowerBound();

    public abstract int getUpperBound();

    public abstract boolean isEmpty();

    public abstract int size();

    public abstract int[] getValues();

    public abstract TIntIterator iterator();

    public static class IrEmptyDomain extends IrDomain {

        @Override
        public boolean isBounded() {
            return false;
        }

        @Override
        public boolean contains(int value) {
            return false;
        }

        @Override
        public int getLowerBound() {
            throw new IrException();
        }

        @Override
        public int getUpperBound() {
            throw new IrException();
        }

        @Override
        public boolean isEmpty() {
            return true;
        }

        @Override
        public int size() {
            return 0;
        }

        @Override
        public int[] getValues() {
            return new int[]{};
        }

        @Override
        public TIntIterator iterator() {
            return EmptyIntIterator.getIterator();
        }

        @Override
        public boolean equals(Object obj) {
            return obj instanceof IrEmptyDomain;
        }

        @Override
        public int hashCode() {
            return 0x12345678;
        }

        @Override
        public String toString() {
            return "{}";
        }
    }

    public static class IrEnumDomain extends IrDomain {

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
            if (obj instanceof IrEnumDomain) {
                IrEnumDomain other = (IrEnumDomain) obj;
                return Arrays.equals(values, other.values);
            } else if (obj instanceof IrBoundDomain) {
                IrBoundDomain other = (IrBoundDomain) obj;
                if (size() != size()) {
                    return false;
                }
                for (int i = 0; i < values.length; i++) {
                    if (values[i] != other.low + i) {
                        return false;
                    }
                }
                return true;
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

    public static class IrBoundDomain extends IrDomain {

        private final int low;
        private final int high;

        /**
         * @param low - Inclusive
         * @param high - Exclusive
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
            if (obj instanceof IrBoundDomain) {
                IrBoundDomain other = (IrBoundDomain) obj;
                return low == other.low && high == other.high;
            } else if (obj instanceof IrEnumDomain) {
                IrEnumDomain other = (IrEnumDomain) obj;
                if (size() != other.size()) {
                    return false;
                }
                for (int i = 0; i < other.values.length; i++) {
                    if (other.values[i] != low + i) {
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
}
