package org.clafer.collection;

import java.util.List;

/**
 * Persistent singly-linked list. Useful for recursive algorithms.
 *
 * @param <E> the type of the elements
 * @author jimmy
 */
public abstract class FList<E> implements Iterable<E> {

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
        Null<E> empty = (Null<E>) Null.Singleton;
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
        return new Cons<>(head, tail);
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
}
