package org.clafer.collection;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import org.clafer.common.Check;

/**
 * Persistent singly-linked list. Useful for recursive algorithms.
 *
 * @param <E> the type of the elements
 * @author jimmy
 */
public abstract class FList<E> implements Iterable<E> {

    private static Null<Object> Null = new Null<Object>();

    private FList() {
    }

    /**
     * Returns the first element of the list.
     *
     * @return the head of the list
     */
    public abstract E getHead();

    /**
     * Returns the a sublist of this list without the first element.
     *
     * @return the tail of the list
     */
    public abstract FList<E> getTail();

    /**
     * Checks if the list is empty.
     *
     * @return {@code true} if and only if the list is empty, {@code false}
     * otherwise
     */
    public abstract boolean isEmpty();

    /**
     * Converts this functional list to an imperative list.
     *
     * @return a copy of this list
     */
    public abstract List<E> toList();

    /**
     * Returns an empty list. The empty list is represented by null.
     *
     * @param <E> the type of the elements
     * @return an empty list
     */
    public static <E> FList<E> empty() {
        @SuppressWarnings("unchecked")
        Null<E> empty = (Null<E>) Null;
        return empty;
    }

    /**
     * A list containing a single element.
     *
     * @param <E> the type of the elements
     * @param item the element of the list
     * @return a list of size 1
     */
    public static <E> FList<E> single(E item) {
        return cons(item, FList.<E>empty());
    }

    /**
     * Functional-programming cons. Nondestructive.
     *
     * @param <E> the type of the elements
     * @param head the beginning of the new list
     * @param tail the end of the new list
     * @return a copy of the original list with head appended at the start
     */
    public static <E> FList<E> cons(E head, FList<E> tail) {
        return new Cons<E>(head, tail);
    }

    /**
     * Functional-programming snoc. Nondestructive.
     *
     * @param <E> the type of the elements
     * @param head the beginning of the new list
     * @param tail the end of the new list
     * @return a copy of the original list with tail appended at the end
     */
    public static <E> FList<E> snoc(FList<E> head, E tail) {
        if (head.isEmpty()) {
            return single(tail);
        }
        return cons(head.getHead(), snoc(head.getTail(), tail));
    }

    private static class Cons<E> extends FList<E> {

        private final E head;
        private final FList<E> tail;

        private Cons(E head, FList<E> tail) {
            this.head = Check.notNull(head);
            this.tail = Check.notNull(tail);
        }

        @Override
        public E getHead() {
            return head;
        }

        @Override
        public FList<E> getTail() {
            return tail;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public List<E> toList() {
            List<E> list = new ArrayList<E>();
            for (FList<E> current = this; !current.isEmpty(); current = current.getTail()) {
                list.add(current.getHead());
            }
            return list;
        }

        @Override
        public Iterator<E> iterator() {
            return new FListIterator<E>(this);
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof Cons<?>) {
                Cons<?> other = (Cons<?>) obj;
                return head.equals(other.head) && tail.equals(other.tail);
            }
            return false;
        }

        @Override
        public int hashCode() {
            int hash = 1;
            for (FList<E> current = this; !current.isEmpty(); current = current.getTail()) {
                hash = 31 * hash + current.getHead().hashCode();
            }
            return hash;
        }

        @Override
        public String toString() {
            StringBuilder result = new StringBuilder();
            result.append('[');
            result.append(head);
            for (FList<E> current = tail; !current.isEmpty(); current = current.getTail()) {
                result.append(", ");
                result.append(current.getHead());
            }
            return result.append(']').toString();
        }
    }

    private static class Null<E> extends FList<E> {

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

    private static class FListIterator<E> implements Iterator<E> {

        private FList<E> current;

        FListIterator(FList<E> current) {
            this.current = current;
        }

        @Override
        public boolean hasNext() {
            return !current.isEmpty();
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
