package org.clafer.collection;

import gnu.trove.TIntCollection;
import gnu.trove.iterator.TIntIterator;
import gnu.trove.procedure.TIntProcedure;
import gnu.trove.set.TIntSet;
import java.util.Arrays;
import java.util.Collection;
import org.clafer.common.Util;

/**
 * For small, fixed-capacity sets. Only purpose of this class is better
 * performance than TIntHashSet for small cases, which happens to be most of
 * them.
 *
 * @author jimmy
 */
public class FixedCapacityIntSet implements TIntSet {

    private final int[] array;
    private int size = 0;

    public FixedCapacityIntSet(int capacity) {
        this.array = new int[capacity];
    }

    public FixedCapacityIntSet(int... values) {
        this(values.length);
        addAll(values);
    }

    public FixedCapacityIntSet(FixedCapacityIntSet set) {
        this(set.size);
        System.arraycopy(set.array, 0, array, 0, size);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getNoEntryValue() {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int size() {
        return size;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isEmpty() {
        return size == 0;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean contains(int entry) {
        for (int i = 0; i < size; i++) {
            if (array[i] == entry) {
                return true;
            }
        }
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TIntIterator iterator() {
        return new FixedCapacityIntSetIterator();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int[] toArray() {
        return Arrays.copyOf(array, size);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int[] toArray(int[] dest) {
        System.arraycopy(array, 0, dest, 0, Math.min(size, dest.length));
        return dest;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean add(int entry) {
        for (int i = 0; i < size; i++) {
            if (array[i] == entry) {
                return false;
            }
        }
        if (size == array.length) {
            throw new IllegalStateException();
        }
        array[size++] = entry;
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean remove(int entry) {
        for (int i = 0; i < size; i++) {
            if (array[i] == entry) {
                size--;
                array[i] = array[size];
                return true;
            }
        }
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean containsAll(Collection<?> collection) {
        for (Object element : collection) {
            if (element instanceof Integer) {
                if (!contains(((Integer) element).intValue())) {
                    return false;
                }
            } else {
                return false;
            }

        }
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean containsAll(TIntCollection collection) {
        TIntIterator iter = collection.iterator();
        while (iter.hasNext()) {
            if (!contains(iter.next())) {
                return false;
            }
        }
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean containsAll(int[] array) {
        for (int i : array) {
            if (!contains(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean addAll(Collection<? extends Integer> collection) {
        boolean changed = false;
        for (Integer element : collection) {
            changed |= add(element.intValue());
        }
        return changed;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean addAll(TIntCollection collection) {
        boolean changed = false;
        TIntIterator iter = collection.iterator();
        while (iter.hasNext()) {
            changed |= add(iter.next());
        }
        return changed;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean addAll(int[] array) {
        boolean changed = false;
        for (int i : array) {
            changed |= add(i);
        }
        return changed;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean retainAll(Collection<?> collection) {
        boolean changed = false;
        TIntIterator iter = iterator();
        while (iter.hasNext()) {
            if (!collection.contains(Integer.valueOf(iter.next()))) {
                iter.remove();
                changed = true;
            }
        }
        return changed;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean retainAll(TIntCollection collection) {
        boolean changed = false;
        TIntIterator iter = iterator();
        while (iter.hasNext()) {
            if (!collection.contains(iter.next())) {
                iter.remove();
                changed = true;
            }
        }
        return changed;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean retainAll(int[] array) {
        boolean changed = false;
        TIntIterator iter = iterator();
        while (iter.hasNext()) {
            if (!Util.in(iter.next(), array)) {
                iter.remove();
                changed = true;
            }
        }
        return changed;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean removeAll(Collection<?> collection) {
        boolean changed = false;
        for (Object element : collection) {
            if (element instanceof Integer) {
                changed |= remove(((Integer) element).intValue());
            }
        }
        return changed;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean removeAll(TIntCollection collection) {
        boolean changed = false;
        TIntIterator iter = collection.iterator();
        while (iter.hasNext()) {
            changed |= remove(iter.next());
        }
        return changed;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean removeAll(int[] array) {
        boolean changed = false;
        for (int i : array) {
            changed |= remove(i);
        }
        return changed;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void clear() {
        size = 0;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean forEach(TIntProcedure procedure) {
        for (int i = 0; i < size; i++) {
            if (!procedure.execute(array[i])) {
                return false;
            }
        }
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof TIntSet) {
            TIntSet other = (TIntSet) obj;
            return size() == other.size() && containsAll(other);
        }
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        int result = 0;
        for (int i = 0; i < size; i++) {
            // Needs to be commutative since the array might not be sorted yet
            // the arrays [1, 2, 3] and [2, 3, 1] need to have the same hash code.
            result = result + array[i];
        }
        return result;
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append('{');
        if (size > 0) {
            result.append(array[0]);
            for (int i = 1; i < size; i++) {
                result.append(", ").append(array[i]);
            }
        }
        result.append('}');
        return result.toString();
    }

    private class FixedCapacityIntSetIterator implements TIntIterator {

        private int index = 0;

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean hasNext() {
            return index < size;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public int next() {
            return array[index++];
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void remove() {
            for (int i = index - 1; i < size - 1; i++) {
                array[i] = array[i + 1];
            }
            index--;
            size--;
        }
    }
}
