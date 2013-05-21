package org.clafer.common;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.list.array.TIntArrayList;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Reader;
import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import solver.Solver;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.VF;
import util.iterators.IntIterator;

/**
 * Various static utility functions.
 *
 * @author jimmy
 */
public class Util {

    private Util() {
    }

    /**
     * Check equality in a null-friendly fashion.
     *
     * @param a an object
     * @param b an object to compare with a
     * @return true if and only if a equals b, false otherwise
     */
    public static boolean equals(Object a, Object b) {
        return (a == b) || (a != null && a.equals(b));
    }

    /**
     * Compute a hash code in a null-friendly fashion.
     *
     * @param o the object to compute the hash for
     * @return the hash code of the object or 0 if null
     */
    public static int hashCode(Object o) {
        return o != null ? o.hashCode() : 0;
    }

    /**
     * @param low the lowest integer in the range
     * @param high the highest integer in the range
     * @return an array of all the values between low (inclusive) and high
     * (inclusive) in order
     */
    public static int[] range(int low, int high) {
        if (low > high) {
            throw new IllegalArgumentException();
        }
        int[] range = new int[high - low + 1];
        for (int i = 0; i < range.length; i++) {
            range[i] = i + low;
        }
        return range;
    }

    /**
     * @param from the lowest integer in the range
     * @param to the integer after the highest integer in the range
     * @return an array of all the values between from (inclusive) and to
     * (exclusive) in order
     */
    public static int[] fromTo(int from, int to) {
        if (from > to) {
            throw new IllegalArgumentException();
        }
        int[] range = new int[to - from];
        for (int i = 0; i < range.length; i++) {
            range[i] = i + from;
        }
        return range;
    }

    /**
     * Returns the position of all the {@code true} elements in the array. The
     * positions are returned in sorted order.
     *
     * @param array the array
     * @return the position of all the {@code true} elements
     */
    public static int[] trues(boolean[] array) {
        return boolIndices(array, true);
    }

    /**
     * Returns the position of all the {@code false} elements in the array. The
     * positions are returned in sorted order.
     *
     * @param array the array
     * @return the position of all the {@code false} elements
     */
    public static int[] falses(boolean[] array) {
        return boolIndices(array, false);
    }

    private static int[] boolIndices(boolean[] bs, boolean val) {
        int count = 0;
        for (boolean b : bs) {
            if (b == val) {
                count++;
            }
        }
        int[] vals = new int[count];
        count = 0;
        for (int i = 0; i < bs.length && count < vals.length; i++) {
            if (bs[i] == val) {
                vals[count++] = i;
            }
        }
        assert count == vals.length;
        return vals;
    }

    /**
     * Reverse part of an array in place.
     *
     * @param array the array to reverse
     * @param to reverse from index 0 to here
     */
    public static void reverse(int[] array, int to) {
        for (int j = 0; j < to / 2; j++) {
            int temp = array[j];
            array[j] = array[to - j - 1];
            array[to - j - 1] = temp;
        }
    }

    /**
     * Reverse an array in place.
     *
     * @param array the array to reverse
     */
    public static void reverse(int[] array) {
        reverse(array, array.length);
    }

    /**
     * Reverse part of an array in place.
     *
     * @param <T> the type of the elements
     * @param array the array to reverse
     * @param to reverse from index 0 to here
     */
    public static <T> void reverse(T[] array, int to) {
        for (int j = 0; j < to / 2; j++) {
            T temp = array[j];
            array[j] = array[to - j - 1];
            array[to - j - 1] = temp;
        }
    }

    /**
     * Reverse an array in place.
     *
     * @param <T> the type of the elements
     * @param array the array to reverse
     */
    public static <T> void reverse(T[] array) {
        reverse(array, array.length);
    }

    /**
     * Sort a list using the comparator. Nondestructive.
     *
     * @param <T> the type of the elements
     * @param list the list of items to be sorted
     * @param comparator induces the order of the sort
     * @return a new sorted list
     */
    public static <T> List<T> sorted(List<T> list, Comparator<? super T> comparator) {
        List<T> sorted = new ArrayList<T>(list);
        Collections.sort(sorted, comparator);
        return sorted;
    }

    /**
     * Check if the array contains the item at least once.
     *
     * @param item check if this item exists in the array
     * @param array the array that may contain the item
     * @return true if and only if item s in array, false otherwise
     */
    public static boolean in(int item, int[] array) {
        for (int a : array) {
            if (a == item) {
                return true;
            }
        }
        return false;
    }

    /**
     * Check if the array contains the item at least once.
     *
     * @param <T> the type of the elements
     * @param item check if this item exists in the array
     * @param array the array that may contain the item
     * @return true if and only if item s in array, false otherwise
     */
    public static <T> boolean in(T item, T[] array) {
        for (T a : array) {
            if (a.equals(item)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Functional-programming cons. Nondestructive.
     *
     * @param <T> the type of the elements
     * @param head the beginning of the new list
     * @param tail the end of the new list
     * @return a copy of the original list with head appended at the start
     */
    public static <T> List<T> cons(T head, List<? extends T> tail) {
        List<T> r = new ArrayList<T>(tail.size() + 1);
        r.add(head);
        r.addAll(tail);
        return r;
    }

    /**
     * Functional-programming snoc. Nondestructive.
     *
     * @param <T> the type of the elements
     * @param head the beginning of the new list
     * @param tail the end of the new list
     * @return a copy of the original list with tail appended at the end
     */
    public static <T> List<T> snoc(List<? extends T> head, T tail) {
        List<T> r = new ArrayList<T>(head.size() + 1);
        r.addAll(head);
        r.add(tail);
        return r;
    }

    /**
     * Append the item at the start of the array. Nondestructive.
     *
     * @param <T> the type of the elements
     * @param item the beginning of the new array
     * @param array the end of the new array
     * @return a copy of the original array with item appended at the start
     */
    public static <T> T[] cons(T item, T[] array) {
        T[] r = Arrays.copyOf(array, array.length + 1);
        for (int i = r.length - 1; i > 0; i--) {
            r[i] = r[i - 1];
        }
        r[0] = item;
        return r;
    }

    /**
     * Append the item at the end of the array. Nondestructive.
     *
     * @param <T> the type of the elements
     * @param array the beginning of the new array
     * @param item the end of the new array
     * @return a copy of the original array with item appended at the end
     */
    public static <T> T[] snoc(T[] array, T item) {
        T[] r = Arrays.copyOf(array, array.length + 1);
        r[array.length] = item;
        return r;
    }

    /**
     * Append the item at the start of the array. Nondestructive.
     *
     * @param item the beginning of the new array
     * @param array the end of the new array
     * @return a copy of the original array with item appended at the start
     */
    public static int[] cons(int item, int[] array) {
        int[] r = new int[array.length + 1];
        System.arraycopy(array, 0, r, 1, array.length);
        r[0] = item;
        return r;
    }

    /**
     * Append the item at the end of the array. Nondestructive.
     *
     * @param array the beginning of the new array
     * @param item the end of the new array
     * @return a copy of the original array with item appended at the end
     */
    public static int[] snoc(int[] array, int item) {
        int[] r = Arrays.copyOf(array, array.length + 1);
        r[array.length] = item;
        return r;
    }

    /**
     * Concatenates all the arrays in the given order into one array. Must be
     * supplied at least one array. Nondestructive.
     *
     * @param <T> the type of the elements
     * @param arrays the array of arrays
     * @return the concatenation of all the arrays
     */
    public static <T> T[] concat(T[]... arrays) {
        if (arrays.length == 0) {
            throw new IllegalArgumentException();
        }
        int length = 0;
        for (T[] array : arrays) {
            length += array.length;
        }
        int offset = 0;
        @SuppressWarnings("unchecked")
        T[] concat = (T[]) Array.newInstance(arrays.getClass().getComponentType().getComponentType(), length);
        for (T[] array : arrays) {
            System.arraycopy(array, 0, concat, offset, array.length);
            offset += array.length;
        }
        return concat;
    }

    /**
     * Compute the greatest common divisor between two numbers usingFermat's
     * algorithm.
     *
     * @param a a non-negative integer
     * @param b a non-negative integer
     * @return the greatest common divisor
     */
    public static int gcd(int a, int b) {
        if (a <= 0) {
            throw new IllegalArgumentException();
        }
        if (b <= 0) {
            throw new IllegalArgumentException();
        }
        do {
            int r = a % b;
            a = b;
            b = r;
        } while (b != 0);
        return a;
    }

    /**
     * Shift in place every element towards the start of the array, wrapping
     * around to the end. For example:
     * <pre>
     * int[] array = new int[]{0,1,2,3,4};
     * shiftLeft(new int[]{0,1,2,3,4}, 2);
     * assertArrayEquals(new int[]{2,3,4,0,1}, array);
     * </pre>
     *
     * @param array the array to shift
     * @param shift the number of indices to shift each element
     */
    public static void shiftLeft(final int[] array, final int shift) {
        if (array.length < shift) {
            throw new IllegalArgumentException();
        }
        if (shift < 0) {
            throw new IllegalArgumentException();
        }
        if (shift > 0) {
            int gcd = gcd(array.length, shift);
            int div = array.length / gcd;
            for (int i = 0; i < gcd; i++) {
                int index = i;
                int place = i + shift;
                int temp = array[i];

                for (int j = 0; j < div - 1; j++) {
                    array[index] = array[place];
                    index = place;
                    place = (index + shift) % array.length;
                }

                array[index] = temp;
            }
        }
    }

    /**
     * @param array an array of integers
     * @return the sum of the integers in the array
     */
    public static int sum(int... array) {
        int sum = 0;
        for (int a : array) {
            sum += a;
        }
        return sum;
    }

    /**
     * @param iter an iterator of integers
     * @return the sum of the integers in the iterator
     */
    public static int sum(TIntIterator iter) {
        int sum = 0;
        while (iter.hasNext()) {
            sum += iter.next();
        }
        return sum;
    }

    /**
     * @param array an array of integers
     * @return the minimum integer in the array
     */
    public static int min(int... array) {
        if (array.length == 0) {
            throw new IllegalArgumentException();
        }
        int min = array[0];
        for (int i = 1; i < array.length; i++) {
            min = Math.min(min, array[i]);
        }
        return min;
    }

    /**
     * @param iter an iterator of integers
     * @return the minimum integer in the iterator
     */
    public static int min(TIntIterator iter) {
        if (!iter.hasNext()) {
            throw new IllegalArgumentException();
        }
        int min = iter.next();
        while (iter.hasNext()) {
            min = Math.min(min, iter.next());
        }
        return min;
    }

    /**
     * @param array an array of integers
     * @return the maximum integer in the array
     */
    public static int max(int... array) {
        if (array.length == 0) {
            throw new IllegalArgumentException();
        }
        int max = array[0];
        for (int i = 1; i < array.length; i++) {
            max = Math.max(max, array[i]);
        }
        return max;
    }

    /**
     * @param iter an iterator of integers
     * @return the maximum integer in the iterator
     */
    public static int max(TIntIterator iter) {
        if (!iter.hasNext()) {
            throw new IllegalArgumentException();
        }
        int max = iter.next();
        while (iter.hasNext()) {
            max = Math.max(max, iter.next());
        }
        return max;
    }

    /**
     * Enumerate the iterator and return the values discovered. The iterator is
     * exhausted on return.
     *
     * @param iter an iterator
     * @return the values found in the iterator
     */
    public static int[] iterate(IntIterator iter) {
        TIntArrayList i = new TIntArrayList();
        while (iter.hasNext()) {
            i.add(iter.next());
        }
        return i.toArray();
    }

    /**
     * Returns a deep copy of the object.
     *
     * @param <T> the type of the object
     * @param obj an object
     * @return a copy of the object
     */
    public static <T extends Serializable> T copy(T obj) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream out;
        try {
            out = new ObjectOutputStream(baos);
            out.writeObject(obj);
            out.close();
            byte[] buf = baos.toByteArray();

            ByteArrayInputStream bin = new ByteArrayInputStream(buf);
            ObjectInputStream in = new ObjectInputStream(bin);
            @SuppressWarnings("unchecked")
            T copy = (T) in.readObject();
            return copy;
        } catch (IOException e) {
            // ByteArrayOutputStream should not throw IOException.
            throw new Error(e);
        } catch (ClassNotFoundException e) {
            // Should not throw ClassNotFoundException since the class is already loaded.
            throw new Error(e);
        }
    }

    /**
     * Concatenate the string representation of the items with a comma
     * separating each item.
     *
     * @param <T> the type of the elements
     * @param items the items to display
     * @return the items string form separated by commas
     */
    public static <T> String commaSeparate(T... items) {
        return intercalate(", ", items);
    }

    /**
     * Concatenate the string representation of the items with a comma
     * separating each item.
     *
     * @param items the items to display
     * @return the items string form separated by commas
     */
    public static String commaSeparate(Iterable<?> items) {
        return intercalate(", ", items);
    }

    /**
     * Concatenate the string representation of the items with a separator
     * separating each item.
     *
     * @param <T> the type of the elements
     * @param separator the string to separate each item
     * @param items the items to display
     * @return the items string form separated by the separator
     */
    public static <T> String intercalate(String separator, T... items) {
        StringBuilder result = new StringBuilder();
        if (items.length > 0) {
            result.append(items[0]);
            for (int i = 1; i < items.length; i++) {
                result.append(separator).append(items[i]);
            }
        }
        return result.toString();
    }

    /**
     * Concatenate the string representation of the items with a separator
     * separating each item.
     *
     * @param separator the string to separate each item
     * @param items the items to display
     * @return the items string form separated by the separatpr
     */
    public static String intercalate(String separator, Iterable<?> items) {
        StringBuilder result = new StringBuilder();
        Iterator<?> iter = items.iterator();
        if (iter.hasNext()) {
            result.append(iter.next());
            while (iter.hasNext()) {
                result.append(separator).append(iter.next());
            }
        }
        return result.toString();
    }

    /**
     * Read the entire contents of a file into a string.
     *
     * @param in the file to read
     * @return the contents of the file
     * @throws IOException an I/O error occured while reading the file
     */
    public static String readAll(File in) throws IOException {
        Reader reader = new FileReader(in);
        try {
            return readAll(reader);
        } finally {
            reader.close();
        }
    }

    /**
     * Read the entire contents of a stream into a string. The stream is left
     * opened, but exhausted.
     *
     * @param in the stream to read
     * @return the contents of the stream
     * @throws IOException an I/O error occured while reading the stream
     */
    public static String readAll(InputStream in) throws IOException {
        return readAll(new InputStreamReader(in));
    }

    /**
     * Read the entire contents of a reader into a string. The reader is left
     * opened, but exhausted.
     *
     * @param in the reader to read
     * @return the contents of the reader
     * @throws IOException an I/O error occured while reading the reader
     */
    public static String readAll(Reader in) throws IOException {
        StringBuilder result = new StringBuilder();
        char[] buffer = new char[1024];
        int l;
        while ((l = in.read(buffer)) != -1) {
            result.append(buffer, 0, l);
        }
        return result.toString();
    }
}
