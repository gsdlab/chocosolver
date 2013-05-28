package org.clafer.collection;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.clafer.common.Util;

/**
 * Persistent singly-linked list. Empty list is represented by {@code null}.
 * Useful for recursive algorithms.
 *
 * @param <E> the type of the elements
 * @author jimmy
 */
public class FList<E> implements Iterable<E> {

    private final E head;
    private final FList<E> tail;

    private FList(E head, FList<E> tail) {
        this.head = head;
        this.tail = tail;
    }

    /**
     * Returns the first element of the list.
     *
     * @return the head of the list
     */
    public E getHead() {
        return head;
    }

    /**
     * Returns the a sublist of this list without the first element. If this
     * list is of length 1, then tail is {@code null}
     *
     * @return the tail of the list
     */
    public FList<E> getTail() {
        return tail;
    }

    /**
     * Checks if the list is empty.
     *
     * @param list the list
     * @return {@code true} if and only if the list is empty, {@code false}
     * otherwise
     */
    public static <E> boolean isEmpty(FList<E> list) {
        return list == null;
    }

    /**
     * Returns an empty list. The empty list is represented by null.
     *
     * @return null
     */
    public static <E> FList<E> empty() {
        return null;
    }

    /**
     * A list containing a single element.
     *
     * @param item the element of the list
     * @return a list of size 1
     */
    public static <E> FList<E> single(E item) {
        return new FList<E>(item, null);
    }

    /**
     * Functional-programming cons. Nondestructive.
     *
     * @param head the beginning of the new list
     * @param tail the end of the new list
     * @return a copy of the original list with head appended at the start
     */
    public static <E> FList<E> cons(E head, FList<E> tail) {
        return new FList<E>(head, tail);
    }

    /**
     * Functional-programming snoc. Nondestructive.
     *
     * @param head the beginning of the new list
     * @param tail the end of the new list
     * @return a copy of the original list with tail appended at the end
     */
    public static <E> FList<E> snoc(FList<E> head, E tail) {
        if (isEmpty(head)) {
            return single(tail);
        }
        return cons(head.head, snoc(head.tail, tail));
    }

    /**
     * Converts this functional list to an imperative list.
     *
     * @return a copy of this list
     */
    public List<E> toList() {
        List<E> list = new ArrayList<E>();
        for (FList<E> current = this; current != null; current = current.tail) {
            list.add(current.head);
        }
        return list;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Iterator<E> iterator() {
        return new FListIterator<E>(this);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object obj) {
        if (obj instanceof FList) {
            FList<?> other = (FList<?>) obj;
            return head.equals(other.head) && Util.equals(tail, other.tail);
        }
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        int hash = 1;
        for (FList<E> current = this; current != null; current = current.tail) {
            hash = 31 * hash + Util.hashCode(current.head);
        }
        return hash;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append('[');
        result.append(head);
        for (FList<E> current = tail; current != null; current = current.tail) {
            result.append(", ");
            result.append(current.head);
        }
        return result.append(']').toString();
    }

    private static class FListIterator<E> implements Iterator<E> {

        private FList<E> current;

        FListIterator(FList<E> current) {
            this.current = current;
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public E next() {
            E head = current.getHead();
            current = current.getTail();
            return head;
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }
    }
}
