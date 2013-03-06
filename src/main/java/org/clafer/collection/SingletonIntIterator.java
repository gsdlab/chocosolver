package org.clafer.collection;

import java.util.NoSuchElementException;

/**
 *
 * @author jimmy
 */
public class SingletonIntIterator implements IntIterator {
    
    private final int value;
    private boolean hasNext = true;
    
    public SingletonIntIterator(int value) {
        this.value = value;
    }
    
    @Override
    public boolean hasNext() {
        return hasNext;
    }
    
    @Override
    public int next() {
        if (hasNext) {
            hasNext = false;
            return value;
        }
        throw new NoSuchElementException();
    }
}
