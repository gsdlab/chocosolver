package org.clafer.collection;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/**
 *
 * @author jimmy
 */
class Null<E> extends FList<E> {

    public static final Null<?> Singleton = new Null();

    private Null() {
    }

    @Override
    public E getHead() {
        throw new NoSuchElementException("No head of empty list.");
    }

    @Override
    public FList<E> getTail() {
        throw new NoSuchElementException("No tail of empty list.");
    }

    @Override
    public boolean isEmpty() {
        return true;
    }

    @Override
    public List<E> toList() {
        return Collections.emptyList();
    }

    @Override
    public Iterator<E> iterator() {
        return Collections.emptyIterator();
    }

    @Override
    public boolean equals(Object obj) {
        return this == obj;
    }

    @Override
    public int hashCode() {
        return 71;
    }

    @Override
    public String toString() {
        return "[]";
    }
}
