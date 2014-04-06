package org.clafer.common;

import java.util.Iterator;

/**
 * Various static utility functions for checking input.
 *
 * @author jimmy
 */
public class Check {

    private Check() {
    }

    /**
     * Check that the item is non-null.
     *
     * @param <T> the type of the object
     * @param message the message of the exception if thrown
     * @param obj the object to check
     * @return the original object
     * @throws NullPointerException if the object is null
     */
    public static <T> T notNull(String message, T obj) throws NullPointerException {
        if (obj == null) {
            throw new NullPointerException(message);
        }
        return obj;
    }

    /**
     * Check that the item is non-null.
     *
     * @param <T> the type of the object
     * @param obj the item to check
     * @return the original object
     * @throws NullPointerException if the object is null
     */
    public static <T> T notNull(T obj) throws NullPointerException {
        if (obj == null) {
            throw new NullPointerException();
        }
        return obj;
    }

    /**
     * @param <T> the type of the elements
     * @param array not null and cannot contain null
     * @return the original array
     * @throws NullPointerException if the array is null or contains a null
     * element
     */
    @SafeVarargs
    public static <T> T[] noNulls(T... array) throws NullPointerException {
        Check.notNull(array);
        for (T t : array) {
            Check.notNull(t);
        }
        return array;
    }

    /**
     * @param <T> the type of the elements
     * @param items not null and cannot contain null
     * @return the original items
     * @throws NullPointerException if the items is null or contains a null
     * element
     */
    public static <T extends Iterable<T>> T noNulls(T items) throws NullPointerException {
        Check.notNull(items);
        for (T t : items) {
            Check.notNull(t);
        }
        return items;
    }

    /**
     * @param <T> the type of the elements
     * @param array not null, cannot contain null, and is non-empty
     * @return the original array
     * @throws IllegalArgumentException if the array is empty
     * @throws NullPointerException if the array is null or contains a null
     * element
     */
    @SafeVarargs
    public static <T> T[] noNullsNotEmpty(T... array)
            throws IllegalArgumentException, NullPointerException {
        Check.notNull(array);
        if (array.length == 0) {
            throw new IllegalArgumentException();
        }
        for (T t : array) {
            Check.notNull(t);
        }
        return array;
    }

    /**
     * @param <T> the type of the elements
     * @param items not null, cannot contain null, and is non-empty
     * @return the original items
     * @throws IllegalArgumentException if the items is empty
     * @throws NullPointerException if the items is null or contains a null
     * element
     */
    public static <T extends Iterable<T>> T noNullsNotEmpty(T items)
            throws IllegalArgumentException, NullPointerException {
        Check.notNull(items);
        Iterator<T> iter = items.iterator();
        if (!iter.hasNext()) {
            throw new IllegalArgumentException();
        }
        do {
            Check.notNull(iter.next());
        } while (iter.hasNext());
        return items;
    }
}
