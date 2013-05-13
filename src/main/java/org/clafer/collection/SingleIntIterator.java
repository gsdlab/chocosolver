package org.clafer.collection;

import gnu.trove.iterator.TIntIterator;

/**
 *
 * @author jimmy
 */
public class SingleIntIterator implements TIntIterator {

    private final int value;
    private boolean start = true;

    public SingleIntIterator(int value) {
        this.value = value;
    }

    @Override
    public boolean hasNext() {
        return start;
    }

    @Override
    public int next() {
        start = false;
        return value;
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }
}
