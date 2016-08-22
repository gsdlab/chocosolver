package org.clafer.domain;

import gnu.trove.TIntCollection;
import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import org.clafer.common.Util;
import org.clafer.ir.IrException;

/**
 *
 * @author jimmy
 */
public class Domain {

    protected final int[] bounds;

    /**
     * @param bounds sorted, unique, and immutable integers
     */
    public Domain(int... bounds) {
        if (bounds.length % 2 == 1) {
            throw new IllegalArgumentException();
        }
        assert isStrictSorted(bounds) : Arrays.toString(bounds);
        this.bounds = bounds;
    }

    private static boolean isStrictSorted(int[] bounds) {
        for (int i = 1; i < bounds.length; i++) {
            if (bounds[i - 1] >= bounds[i]) {
                return false;
            }
        }
        return true;
    }

    public static Domain boundDomain(int low, int high) {
        return new Domain(low, high + 1);
    }

    public static Domain constantDomain(int value) {
        return boundDomain(value, value);
    }

    public static Domain enumDomain(int... values) {
        if (values.length == 1) {
            return constantDomain(values[0]);
        }
        return enumDomain(new TIntHashSet(values));
    }

    public static Domain enumDomain(TIntSet values) {
        int[] bounds = new int[2 * values.size()];
        int regions = 0;
        int[] array = values.toArray();
        Arrays.sort(array);
        int i = 0;
        while (i < array.length) {
            int prev = i;
            do {
                i++;
            } while (i < array.length && array[i] == array[i - 1] + 1);
            bounds[regions++] = array[prev];
            bounds[regions++] = array[i - 1] + 1;
        }
        if (regions < bounds.length) {
            return new Domain(Arrays.copyOf(bounds, regions));
        }
        return new Domain(bounds);
    }

    public boolean isBounded() {
        return bounds.length <= 2;
    }

    public boolean contains(int value) {
        return Util.ordinal(value, bounds) % 2 == 1;
    }

    public boolean containsAll(TIntCollection values) {
        TIntIterator iter = values.iterator();
        while (iter.hasNext()) {
            if (!contains(iter.next())) {
                return false;
            }
        }
        return true;
    }

    public boolean containsAll(int[] values) {
        for (int value : values) {
            if (!contains(value)) {
                return false;
            }
        }
        return true;
    }

    public boolean containsAll(int low, int high) {
        int lowOrdinal = Util.ordinal(low, bounds);
        if (lowOrdinal % 2 == 0) {
            return false;
        }
        if (low == high) {
            return true;
        }
        int highOrdinal = Util.ordinal(high, bounds);
        return lowOrdinal == highOrdinal;
    }

    public boolean containsAny(TIntCollection values) {
        TIntIterator iter = values.iterator();
        while (iter.hasNext()) {
            if (contains(iter.next())) {
                return true;
            }
        }
        return false;
    }

    public boolean containsAny(int[] values) {
        for (int value : values) {
            if (contains(value)) {
                return true;
            }
        }
        return false;
    }

    public boolean containsAny(int low, int high) {
        int lowOrdinal = Util.ordinal(low, bounds);
        if (lowOrdinal % 2 == 1) {
            return true;
        }
        int highOrdinal = Util.ordinal(high, bounds);
        return lowOrdinal != highOrdinal;
    }

    public int getLowBound() {
        if (bounds.length == 0) {
            throw new IrException("Emtpy domain does not have a low bound.");
        }
        return bounds[0];
    }

    public int getHighBound() {
        if (bounds.length == 0) {
            throw new IrException("Emtpy domain does not have a low bound.");
        }
        return bounds[bounds.length - 1] - 1;
    }

    public boolean isEmpty() {
        return bounds.length == 0;
    }

    public boolean isConstant() {
        return bounds.length == 2 && bounds[0] + 1 == bounds[1];
    }

    public int size() {
        int size = 0;
        for (int i = 0; i < bounds.length; i += 2) {
            size += bounds[i + 1] - bounds[i];
        }
        return size;
    }

    public boolean isSubsetOf(Domain superset) {
        if (this == superset) {
            return true;
        }
        for (int i = 0; i < bounds.length; i += 2) {
            if (!superset.containsAll(bounds[i], bounds[i + 1] - 1)) {
                return false;
            }
        }
        return true;
    }

    public boolean intersects(Domain other) {
        if (this == other && !isEmpty()) {
            return true;
        }
        for (int i = 0; i < bounds.length; i += 2) {
            if (other.containsAny(bounds[i], bounds[i + 1] - 1)) {
                return true;
            }
        }
        return false;
    }

    private Domain insertOrRemove(boolean insertOrRemove, int value) {
        int ordinal = Util.ordinal(value, bounds);
        if (ordinal % 2 == (insertOrRemove ? 1 : 0)) {
            return this;
        }
        if (ordinal > 0 && bounds[ordinal - 1] == value) {
            if (ordinal < bounds.length && bounds[ordinal] == value + 1) {
                // Merge the two regions.
                int[] newBounds = new int[bounds.length - 2];
                System.arraycopy(bounds, 0, newBounds, 0, ordinal - 1);
                System.arraycopy(bounds, ordinal + 1, newBounds, ordinal - 1, bounds.length - ordinal - 1);
                return new Domain(newBounds);
            }
            int[] newBounds = bounds.clone();
            newBounds[ordinal - 1]++;
            return new Domain(newBounds);
        } else if (ordinal < bounds.length && bounds[ordinal] == value + 1) {
            int[] newBounds = bounds.clone();
            newBounds[ordinal]--;
            return new Domain(newBounds);
        } else {
            int[] newBounds = new int[bounds.length + 2];
            System.arraycopy(bounds, 0, newBounds, 0, ordinal);
            newBounds[ordinal] = value;
            newBounds[ordinal + 1] = value + 1;
            System.arraycopy(bounds, ordinal, newBounds, ordinal + 2, bounds.length - ordinal);
            return new Domain(newBounds);
        }
    }

    public Domain insert(int value) {
        return insertOrRemove(true, value);
    }

    public Domain remove(int value) {
        return insertOrRemove(false, value);
    }

    public Domain boundLow(int low) {
        if (isEmpty() || low <= getLowBound()) {
            return this;
        }
        int ordinal = Util.ordinal(low, bounds);
        if (ordinal % 2 == 0) {
            int[] newBounds = Arrays.copyOfRange(bounds, ordinal, bounds.length);
            return new Domain(newBounds);
        }
        int[] newBounds = Arrays.copyOfRange(bounds, ordinal - 1, bounds.length);
        newBounds[0] = low;
        return new Domain(newBounds);
    }

    public Domain boundHigh(int high) {
        if (isEmpty() || high >= getHighBound()) {
            return this;
        }
        int ordinal = Util.ordinal(high, bounds);
        if (ordinal % 2 == 0) {
            int[] newBounds = Arrays.copyOfRange(bounds, 0, ordinal);
            return new Domain(newBounds);
        }
        int[] newBounds = Arrays.copyOfRange(bounds, 0, ordinal + 1);
        newBounds[ordinal] = high + 1;
        return new Domain(newBounds);
    }

    public Domain boundBetween(int low, int high) {
        return boundLow(low).boundHigh(high);
    }

    public Domain minus() {
        int[] newBounds = new int[bounds.length];
        for (int i = 0; i < newBounds.length; i++) {
            newBounds[i] = -bounds[bounds.length - i - 1] + 1;
        }
        return new Domain(newBounds);
    }

    public Domain difference(Domain other) {
        if (isEmpty() || !intersects(other)) {
            return this;
        }
        int[] newBounds = new int[bounds.length + other.bounds.length];
        int length = 0;
        int i = 0;
        int j = 0;

        int from = bounds[i];
        int to = bounds[i + 1];

        while (true) {
            while (j < other.bounds.length
                    && from >= other.bounds[j + 1]) {
                j += 2;
            }
            if (j == other.bounds.length
                    || to <= other.bounds[j]) {
                newBounds[length++] = from;
                newBounds[length++] = to;
                from = to;
            } else if (from < other.bounds[j]
                    && to > other.bounds[j]) {
                newBounds[length++] = from;
                newBounds[length++] = other.bounds[j];
                from = other.bounds[j + 1];
            } else {
                from = other.bounds[j + 1];
            }
            if (from >= to) {
                i += 2;
                if (i >= bounds.length) {
                    break;
                }
                from = bounds[i];
                to = bounds[i + 1];
            }
        }
        if (length == newBounds.length) {
            return new Domain(newBounds);
        }
        return new Domain(Arrays.copyOf(newBounds, length));
    }

    public Domain intersection(Domain other) {
        if (isSubsetOf(other)) {
            return this;
        }
        if (other.isSubsetOf(this)) {
            return other;
        }
        int[] newBounds = new int[bounds.length + other.bounds.length];
        int length = 0;
        int i = 0;
        int j = 0;

        int from = bounds[i];
        int to = bounds[i + 1];

        while (true) {
            while (j < other.bounds.length
                    && from >= other.bounds[j + 1]) {
                j += 2;
            }
            if (j == other.bounds.length
                    || to <= other.bounds[j]) {
                from = to;
            } else if (from >= other.bounds[j]
                    && to <= other.bounds[j + 1]) {
                newBounds[length++] = from;
                newBounds[length++] = to;
                from = to;
            } else {
                newBounds[length++] = Math.max(from, other.bounds[j]);
                newBounds[length++] = Math.min(to, other.bounds[j + 1]);
                from = newBounds[length - 1];
            }
            if (from >= to) {
                i += 2;
                if (i >= bounds.length) {
                    break;
                }
                from = bounds[i];
                to = bounds[i + 1];
            }
        }
        if (length == newBounds.length) {
            return new Domain(newBounds);
        }
        return new Domain(Arrays.copyOf(newBounds, length));
    }

    private static int[] mergeRegions(int[] bounds) {
        int length = 0;
        int head = 0;
        while (head < bounds.length) {
            int from = bounds[head];
            int to = bounds[head + 1];
            while (head < bounds.length - 2
                    && to >= bounds[head + 2]) {
                head += 2;
                to = Math.max(to, bounds[head + 1]);
            }
            bounds[length++] = from;
            bounds[length++] = to;
            head += 2;
        }
        if (length == bounds.length) {
            return bounds;
        }
        return Arrays.copyOf(bounds, length);
    }

    public Domain union(Domain other) {
        if (isSubsetOf(other)) {
            return other;
        }
        if (other.isSubsetOf(this)) {
            return this;
        }
        int[] newBounds = new int[bounds.length + other.bounds.length];
        int length = 0;
        int j = 0;

        for (int i = 0; i < bounds.length; i += 2) {
            while (j < other.bounds.length
                    && bounds[i] >= other.bounds[j]) {
                newBounds[length++] = other.bounds[j];
                newBounds[length++] = other.bounds[j + 1];
                j += 2;
            }
            newBounds[length++] = bounds[i];
            newBounds[length++] = bounds[i + 1];
        }
        while (j < other.bounds.length) {
            newBounds[length++] = other.bounds[j];
            newBounds[length++] = other.bounds[j + 1];
            j += 2;
        }
        return new Domain(mergeRegions(newBounds));
    }

    public Domain offset(int c) {
        if (c == 0) {
            return this;
        }
        int[] newBounds = new int[bounds.length];
        for (int i = 0; i < newBounds.length; i++) {
            newBounds[i] = bounds[i] + c;
        }
        return new Domain(newBounds);
    }

    public Domain add(Domain other) {
        if (isEmpty()) {
            return this;
        }
        if (other.isEmpty()) {
            return other;
        }
        if (isConstant()) {
            return other.offset(getLowBound());
        }
        if (other.isConstant()) {
            return offset(other.getLowBound());
        }
        Domain sum = null;
        for (int i = 0; i < bounds.length; i += 2) {
            int[] newBounds = new int[other.bounds.length];
            for (int j = 0; j < other.bounds.length; j += 2) {
                newBounds[j] = bounds[i] + other.bounds[j];
                newBounds[j + 1] = bounds[i + 1] + other.bounds[j + 1] - 1;
            }
            Domain domain = new Domain(mergeRegions(newBounds));
            sum = sum == null ? domain : sum.union(domain);
        }
        return sum;
    }

    public int[] getValues() {
        int[] values = new int[size()];
        int size = 0;
        for (int region = 0; region < bounds.length; region += 2) {
            for (int i = bounds[region]; i < bounds[region + 1]; i++) {
                values[size++] = i;
            }
        }
        assert size == values.length;
        return values;
    }

    public TIntIterator iterator() {
        return new ForwardIterator();
    }

    public TIntIterator iterator(boolean increasing) {
        return increasing ? new ForwardIterator() : new ReverseIterator();
    }

    public void transferTo(TIntCollection collection) {
        for (int region = 0; region < bounds.length; region += 2) {
            for (int i = bounds[region]; i < bounds[region + 1]; i++) {
                collection.add(i);
            }
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Domain) {
            Domain other = (Domain) obj;
            return Arrays.equals(bounds, other.bounds);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(bounds);
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append('{');
        for (int i = 0; i < bounds.length; i += 2) {
            if (i > 0) {
                result.append(", ");
            }
            if (bounds[i + 1] > bounds[i] + 2) {
                result.append(bounds[i]);
                result.append("...");
                result.append(bounds[i + 1] - 1);
            } else {
                for (int j = bounds[i]; j < bounds[i + 1]; j++) {
                    if (j > bounds[i]) {
                        result.append(", ");
                    }
                    result.append(j);
                }
            }
        }
        result.append('}');
        return result.toString();
    }

    private class ForwardIterator implements TIntIterator {

        int region = 0;
        int i = 0;

        @Override
        public int next() {
            int next = bounds[region] + i;
            if (next == bounds[region + 1] - 1) {
                region += 2;
                i = 0;
            } else {
                i++;
            }
            return next;
        }

        @Override
        public boolean hasNext() {
            return region < bounds.length;
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    private class ReverseIterator implements TIntIterator {

        int region = bounds.length - 2;
        int i = 1;

        @Override
        public int next() {
            int next = bounds[region + 1] - i;
            if (next == bounds[region]) {
                region -= 2;
                i = 1;
            } else {
                i++;
            }
            return next;
        }

        @Override
        public boolean hasNext() {
            return region >= 0;
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }
    }
}
