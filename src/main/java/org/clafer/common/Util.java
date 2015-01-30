package org.clafer.common;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.list.array.TIntArrayList;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import org.chocosolver.util.iterators.IntIterator;

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

    public static int permutation(int n, int r) {
        if (n < 0) {
            throw new IllegalArgumentException();
        }
        int permutation = 1;
        for (int i = 0; i < r; i++) {
            permutation *= n - i;
        }
        return permutation;
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
     * Randomly shuffle an array in place.
     *
     * @param <T> the type of the elements
     * @param array the array to shuffle
     * @param rand the random number generator
     */
    public static <T> void shuffle(T[] array, Random rand) {
        for (int i = 0; i < array.length; i++) {
            int index = rand.nextInt(array.length);
            // Simple swap
            T temp = array[index];
            array[index] = array[i];
            array[i] = temp;
        }
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
     * Check if the array contains the item at least once.
     *
     * @param item check if this item exists in the array
     * @param array the array that may contain the item
     * @return {@code true} if and only if item is in array, {@code false}
     * otherwise
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
     * @return {@code true} if and only if item is in array, {@code false}
     * otherwise
     */
    public static <T> boolean in(T item, T[] array) {
        for (T a : array) {
            if (equals(item, a)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Check if every element in the array is unique.
     *
     * @param array the array
     * @return {@code true} if and only if the elements in the array never
     * repeat, {@code false} otherwise
     */
    public static boolean isUnique(int[] array) {
        for (int i = 0; i < array.length; i++) {
            for (int j = i + 1; j < array.length; j++) {
                if (array[i] == array[j]) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Check if every element in the array is unique.
     *
     * @param <T> the type of the elements
     * @param array the array
     * @return {@code true} if and only if the elements in the array never
     * repeat, {@code false} otherwise
     */
    public static <T> boolean isUnique(T[] array) {
        for (int i = 0; i < array.length; i++) {
            for (int j = i + 1; j < array.length; j++) {
                if (equals(array[i], array[j])) {
                    return false;
                }
            }
        }
        return true;
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
        List<T> r = new ArrayList<>(tail.size() + 1);
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
        List<T> r = new ArrayList<>(head.size() + 1);
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
    public static <T> T[] concat(T[][] arrays) {
        switch (arrays.length) {
            case 0:
                @SuppressWarnings("unchecked") T[] empty = (T[]) Array.newInstance(arrays.getClass().getComponentType().getComponentType(), 0);
                return empty;
            case 1:
                return arrays[0];
            default:
                int length = 0;
                for (T[] array : arrays) {
                    length += array.length;
                }
                int offset = 0;
                @SuppressWarnings("unchecked") T[] concat = (T[]) Array.newInstance(arrays.getClass().getComponentType().getComponentType(), length);
                for (T[] array : arrays) {
                    System.arraycopy(array, 0, concat, offset, array.length);
                    offset += array.length;
                }
                return concat;
        }
    }

    /**
     * Repeat the item.
     *
     * @param <T> the type of the item
     * @param item the item
     * @param times the number of times to repeat
     * @return an array containing only the item
     */
    public static <T> T[] replicate(T item, int times) {
        @SuppressWarnings("unchecked")
        T[] array = (T[]) Array.newInstance(item.getClass(), times);
        for (int i = 0; i < array.length; i++) {
            array[i] = item;
        }
        return array;
    }

    /**
     * Returns all permutations of picking a fixed number of distinct elements
     * in the array.
     *
     * @param <T> the type of the elements
     * @param array the array
     * @param choose the number of elements to pick
     * @return the permutations
     */
    public static <T> T[][] permutations(T[] array, int choose) {
        if (choose > array.length) {
            throw new IllegalArgumentException();
        }
        if (choose == 0) {
            @SuppressWarnings("unchecked")
            T[][] permutations = (T[][]) Array.newInstance(array.getClass(), 1);
            @SuppressWarnings("unchecked")
            T[] permutation = (T[]) Array.newInstance(array.getClass().getComponentType(), 0);
            permutations[0] = permutation;
            return permutations;
        }

        @SuppressWarnings("unchecked")
        T[][] permutations = (T[][]) Array.newInstance(
                array.getClass().getComponentType(),
                permutation(array.length, choose), choose);

        int[] indices = new int[choose];
        indices[indices.length - 1]--;
        for (T[] permutation : permutations) {
            do {
                int j = indices.length - 1;
                indices[j]++;
                while (indices[j] >= array.length) {
                    indices[j] = 0;
                    j--;
                    indices[j]++;
                }
            } while (!isUnique(indices));
            for (int k = 0; k < indices.length; k++) {
                permutation[k] = array[indices[k]];
            }
        }
        return permutations;
    }

    /**
     * Returns all possibilities of picking a fixed number of elements in the
     * array.
     *
     * @param <T> the type of the elements
     * @param array the array
     * @param choose the number of elements to pick
     * @return the sequence
     */
    public static <T> T[][] sequence(T[] array, int choose) {
        if (choose <= 0) {
            throw new IllegalArgumentException();
        }
        return sequence(replicate(array, choose));
    }

    /**
     * Returns all possibilities of picking one element in each array. Similar
     * to Haskell's {@code sequence} for lists, except for edge cases.
     *
     * @param <T> the type of the elements
     * @param arrays the arrays
     * @return the sequence
     */
    public static <T> T[][] sequence(T[][] arrays) {
        if (arrays.length == 0) {
            @SuppressWarnings("unchecked")
            T[][] sequence = (T[][]) Array.newInstance(arrays.getClass().getComponentType(), 1);
            @SuppressWarnings("unchecked")
            T[] permutation = (T[]) Array.newInstance(arrays.getClass().getComponentType().getComponentType(), 0);
            sequence[0] = permutation;
            return sequence;
        }

        int product = 1;
        for (T[] array : arrays) {
            product *= array.length;
        }
        @SuppressWarnings("unchecked")
        T[][] sequence = (T[][]) Array.newInstance(
                arrays.getClass().getComponentType().getComponentType(),
                product, arrays.length);

        int[] indices = new int[arrays.length];
        indices[indices.length - 1]--;
        for (T[] array : sequence) {
            int j = indices.length - 1;
            indices[j]++;
            while (indices[j] >= arrays[j].length) {
                indices[j] = 0;
                j--;
                indices[j]++;
            }
            for (int k = 0; k < indices.length; k++) {
                array[k] = arrays[k][indices[k]];
            }
        }
        return sequence;
    }

    /**
     * Returns a sublist from index 0 to the index of the item (inclusive).
     * Equivalent to the Haskell code {@code takeWhile (== item) list}
     *
     * @param <T> the element type
     * @param item the item to find
     * @param list the list of items
     * @return a prefix of the {@code list} ending at the first {@code item} if
     * found, otherwise the entire list
     */
    public static <T> List<T> takeUntil(T item, List<T> list) {
        int index = 0;
        for (T t : list) {
            index++;
            if (item.equals(t)) {
                return list.subList(0, index);
            }
        }
        return list;
    }

    /**
     * Returns a sublist from the index of the item (inclusive) to the end of
     * the list. Equivalent to the Haskell code {@code dropWhile (/= item) list}
     *
     * @param <T> the element type
     * @param item the item to find
     * @param list the list of items
     * @return a suffix of the {@code list} starting at the first {@code item}
     * if found, otherwise the entire list
     */
    public static <T> List<T> dropUntil(T item, List<T> list) {
        int index = 0;
        for (T t : list) {
            if (item.equals(t)) {
                return list.subList(index, list.size());
            }
            index++;
        }
        return Collections.emptyList();
    }

    /**
     * Checks if the list starts with specific elements.
     *
     * @param <T> the element type
     * @param string the list
     * @param prefix the starting elements of the list
     * @return {@code true} if and only if {@code string} starts with
     * {@code prefix}, {@code false} otherwise
     */
    public static <T> boolean startsWith(List<T> string, List<T> prefix) {
        final int stringSize = string.size();
        final int prefixSize = prefix.size();
        if (prefixSize > stringSize) {
            return false;
        }
        return prefix.equals(string.subList(0, prefixSize));
    }

    /**
     * Checks if the list ends with specific elements.
     *
     * @param <T> the element type
     * @param string the list
     * @param suffix the ending elements of the list
     * @return {@code true} if and only if {@code string} ends with
     * {@code suffix}, {@code false} otherwise
     */
    public static <T> boolean endsWith(List<T> string, List<T> suffix) {
        final int stringSize = string.size();
        final int suffixSize = suffix.size();
        if (suffixSize > stringSize) {
            return false;
        }
        return suffix.equals(string.subList(stringSize - suffixSize, stringSize));
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
     * @param a
     * @param b
     * @return the greatest common divisor of {@code a} and {@code b}
     */
    public static int gcd(int a, int b) {
        return b == 0 ? a : gcd(b, a % b);
    }

    /**
     * @param a
     * @param b
     * @return the greatest common divisor of {@code a} and {@code b}
     */
    public static long gcd(long a, long b) {
        return b == 0 ? a : gcd(b, a % b);
    }

    /**
     * @param a
     * @param b
     * @return the least common multiple of {@code a} and {@code b}
     */
    public static int lcm(int a, int b) {
        return a * (b / gcd(a, b));
    }

    /**
     * @param a
     * @param b
     * @return the least common multiple of {@code a} and {@code b}
     */
    public static long lcm(long a, long b) {
        return a * (b / gcd(a, b));
    }

    public static int divFloor(int a, int b) {
        if (b < 0) {
            return divFloor(-a, -b);
        } else if (a < 0) {
            return (a - b + 1) / b;
        }
        return (a / b);
    }

    public static long divFloor(long a, long b) {
        if (b < 0) {
            return divFloor(-a, -b);
        } else if (a < 0) {
            return (a - b + 1) / b;
        }
        return (a / b);
    }

    public static int divCeil(int a, int b) {
        if (b < 0) {
            return divCeil(-a, -b);
        } else if (a >= 0) {
            return ((a + b - 1) / b);
        }
        return a / b;
    }

    public static long divCeil(long a, long b) {
        if (b < 0) {
            return divCeil(-a, -b);
        } else if (a >= 0) {
            return ((a + b - 1) / b);
        }
        return a / b;
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
     * @param <K> input type
     * @param <V> return type
     * @param map a bijection
     * @return the inverse of map
     */
    public static <K, V> Map<V, K> inverse(Map<K, V> map) {
        Map<V, K> inverse = new HashMap<>(map.size());
        for (Entry<K, V> entry : map.entrySet()) {
            if (inverse.put(entry.getValue(), entry.getKey()) != null) {
                throw new IllegalArgumentException(map + " is not a bijection.");
            }
        }
        return inverse;
    }

    /**
     * Concatenate the string representation of the items with a comma
     * separating each item.
     *
     * @param <T> the type of the elements
     * @param items the items to display
     * @return the items string form separated by commas
     */
    public static <T> String commaSeparate(T[] items) {
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
    public static <T> String intercalate(String separator, T[] items) {
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
     * @return the items string form separated by the separator
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

    @SafeVarargs
    public static <T> T[] cast(Object[] array, T... dest) {
        @SuppressWarnings("unchecked")
        T[] to = dest.length == array.length
                ? dest
                : (T[]) Array.newInstance(dest.getClass().getComponentType(), array.length);
        System.arraycopy(array, 0, to, 0, to.length);
        return to;
    }
}
