package org.clafer.collection;

import gnu.trove.TIntCollection;
import gnu.trove.iterator.TIntIterator;
import gnu.trove.procedure.TIntProcedure;
import gnu.trove.set.TIntSet;
import java.util.Arrays;
import java.util.Collection;

/**
 * For small, fixed-capacity sets.
 * 
 * @author jimmy
 */
public class FixedIntSet implements TIntSet {

    private final int[] array;
    private int size = 0;

    public FixedIntSet(int capacity) {
        this.array = new int[capacity];
        FixedIntSet s = new FixedIntSet(capacity);
    }

    /** {@inheritDoc} */
    @Override
    public int getNoEntryValue() {
        throw new UnsupportedOperationException();
    }

    /** {@inheritDoc} */
    @Override
    public int size() {
        return size;
    }

    /** {@inheritDoc} */
    @Override
    public boolean isEmpty() {
        return size == 0;
    }

    /** {@inheritDoc} */
    @Override
    public boolean contains(int entry) {
        for (int i = 0; i < size; i++) {
            if (array[i] == entry) {
                return true;
            }
        }
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public TIntIterator iterator() {
        return new ArrayIntIterator(array, 0, size);
    }

    /** {@inheritDoc} */
    @Override
    public int[] toArray() {
        return Arrays.copyOf(array, size);
    }

    /** {@inheritDoc} */
    @Override
    public int[] toArray(int[] dest) {
        System.arraycopy(array, 0, dest, 0, Math.min(size, dest.length));
        return dest;
    }

    /** {@inheritDoc} */
    @Override
    public boolean add(int entry) {
        if (size == array.length) {
            throw new IllegalStateException();
        }
        for (int i = 0; i < size; i++) {
            if (array[i] == entry) {
                return false;
            }
        }
        array[size++] = entry;
        return true;
    }

    /** {@inheritDoc} */
    @Override
    public boolean remove(int entry) {
        for (int i = 0; i < size; i++) {
            if (array[i] == entry) {
                int temp = array[i];
                array[i] = array[size - 1];
                array[size - 1] = temp;
                return true;
            }
        }
        return false;
    }

    /** {@inheritDoc} */
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

    /** {@inheritDoc} */
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

    /** {@inheritDoc} */
    @Override
    public boolean containsAll(int[] array) {
        for (int i : array) {
            if (!contains(i)) {
                return false;
            }
        }
        return true;
    }

    /** {@inheritDoc} */
    @Override
    public boolean addAll(Collection<? extends Integer> collection) {
        boolean changed = false;
        for (Integer element : collection) {
            changed |= add(element.intValue());
        }
        return changed;
    }

    /** {@inheritDoc} */
    @Override
    public boolean addAll(TIntCollection collection) {
        boolean changed = false;
        TIntIterator iter = collection.iterator();
        while (iter.hasNext()) {
            changed |= add(iter.next());
        }
        return changed;
    }

    /** {@inheritDoc} */
    @Override
    public boolean addAll(int[] array) {
        boolean changed = false;
        for (int i : array) {
            changed |= add(i);
        }
        return changed;
    }

    /** {@inheritDoc} */
    @Override
    public boolean retainAll(Collection<?> collection) {
        throw new UnsupportedOperationException();
    }

    /** {@inheritDoc} */
    @Override
    public boolean retainAll(TIntCollection collection) {
        throw new UnsupportedOperationException();
    }

    /** {@inheritDoc} */
    @Override
    public boolean retainAll(int[] array) {
        throw new UnsupportedOperationException();
    }

    /** {@inheritDoc} */
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

    /** {@inheritDoc} */
    @Override
    public boolean removeAll(TIntCollection collection) {
        boolean changed = false;
        TIntIterator iter = collection.iterator();
        while (iter.hasNext()) {
            changed |= remove(iter.next());
        }
        return changed;
    }

    /** {@inheritDoc} */
    @Override
    public boolean removeAll(int[] array) {
        boolean changed = false;
        for (int i : array) {
            changed |= remove(i);
        }
        return changed;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        size = 0;
    }

    /** {@inheritDoc} */
    @Override
    public boolean forEach(TIntProcedure procedure) {
        for (int i = 0; i < size; i++) {
            if (!procedure.execute(array[i])) {
                return false;
            }
        }
        return true;
    }

    /** {@inheritDoc} */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof TIntSet) {
            TIntSet other = (TIntSet) obj;
            return size() == other.size() && containsAll(other);
        }
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public int hashCode() {
        int result = 1;
        for (int i = 0; i < size; i++) {
            result = 31 * result + array[i];
        }
        return result;
    }
}
