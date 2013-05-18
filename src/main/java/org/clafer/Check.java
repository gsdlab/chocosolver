package org.clafer;

import java.util.ArrayList;
import java.util.Iterator;

/**
 * Various static utility functions for checking input.
 * 
 * @author jimmy
 */
public class Check {

    /**
     * @param array not null and cannot contain null
     * @return the original array
     * @throws NullPointerException if the array is null or contains a null element
     */
    public static <T> T[] noNulls(T... array) throws NullPointerException {
        Check.notNull(array);
        for (T t : array) {
            Check.notNull(t);
        }
        return array;
    }

    /**
     * @param items not null and cannot contain null
     * @return the original items
     * @throws NullPointerException if the items is null or contains a null element
     */
    public static <T extends Iterable<T>> T noNulls(T items) throws NullPointerException {
        Check.notNull(items);
        for (T t : items) {
            Check.notNull(t);
        }
        return items;
    }

    /**
     * @param arrays not null, cannot contain null, and none of its elements can
     *          contain null
     * @return the original array
     * @throws NullPointerException if the array is null, contains a null element,
     *          or one of its elements contains a null
     */
    public static <T> T[][] noNulls(T[]... arrays) throws NullPointerException {
        Check.notNull(arrays);
        for (T[] t : arrays) {
            Check.noNulls(t);
        }
        return arrays;
    }

    /**
     * @param array not null, cannot contain null, and is non-empty
     * @return the original array
     * @throws IllegalArgumentException if the array is empty
     * @throws NullPointerException if the array is null or contains a null element
     */
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
     * @param items not null, cannot contain null, and is non-empty
     * @return the original items
     * @throws IllegalArgumentException if the items is empty
     * @throws NullPointerException if the items is null or contains a null element
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

    /**
     * @param arrays not null, cannot contain null, none of its elements can
     *          contain null, and is non-empty
     * @return the original array
     * @throws IllegalArgumentException if the arrays is empty
     * @throws NullPointerException if the array is null, contains a null element,
     *          or one of its elements contains a null
     */
    public static <T> T[][] noNullsNotEmpty(T[]... arrays)
            throws IllegalArgumentException, NullPointerException {
        Check.notNull(arrays);
        if (arrays.length == 0) {
            throw new IllegalArgumentException();
        }
        for (T[] t : arrays) {
            Check.noNulls(t);
        }
        return arrays;
    }

    /**
     * Check that the item is non-null.
     * 
     * @param message the message ofthe exception if thrown
     * @param item the item to check
     * @return the original item
     * @throws NullPointerException if the item is null
     */
    public static <T> T notNull(String message, T item) throws NullPointerException {
        if (item == null) {
            throw new NullPointerException(message);
        }
        return item;
    }

    /**
     * Check that the item is non-null.
     * 
     * @param item the item to check
     * @return the original item
     * @throws NullPointerException if the item is null
     */
    public static <T> T notNull(T item) throws NullPointerException {
        if (item == null) {
            throw new NullPointerException();
        }
        return item;
    }
}
