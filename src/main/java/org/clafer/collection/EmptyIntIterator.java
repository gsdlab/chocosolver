package org.clafer.collection;

import java.util.NoSuchElementException;

/**
 *
 * @author jimmy
 */
public class EmptyIntIterator implements IntIterator {

    private static final EmptyIntIterator iterator = new EmptyIntIterator();

    private EmptyIntIterator() {
    }

    public static EmptyIntIterator getIterator() {
        return iterator;
    }

    @Override
    public boolean hasNext() {
        return false;
    }

    @Override
    public int next() {
        throw new NoSuchElementException();
    }
}
