package org.clafer;

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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import util.iterators.IntIterator;

/**
 *
 * @author jimmy
 */
public class Util {

    public static boolean equals(Object a, Object b) {
        return (a == b) || (a != null && a.equals(b));
    }

    public static int hashCode(Object o) {
        return o != null ? o.hashCode() : 0;
    }

    /**
     * @param low - inclusive
     * @param high - inclusive
     * @return 
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
     * @param from - inclusive
     * @param to - exclusive
     * @return 
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
//
//    /**
//     * @return - The set difference
//     */
////    public static int[] difference(int[] s1, int[] s2) {
////        TIntArrayList diff = new TIntArrayList();
////        for (int s : s1) {
////            if (!in(s, s2)) {
////                diff.add(s);
////            }
////        }
////        return diff.toNativeArray();
////    }

    /**
     * @return - The position of all the trues
     */
    public static int[] trues(boolean[] bs) {
        return boolIndices(bs, true);
    }

    public static int[] falses(boolean[] bs) {
        return boolIndices(bs, false);
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

    // http://stackoverflow.com/questions/8095045/java-array-order-reversing
    public static void reverse(int[] a, int to) {
        for (int j = 0; j < to / 2; j++) {
            int temp = a[j];
            a[j] = a[to - j - 1];
            a[to - j - 1] = temp;
        }
    }

    public static void reverse(int[] a) {
        reverse(a, a.length);
    }

    public static <T> void reverse(T[] a, int to) {
        for (int j = 0; j < to / 2; j++) {
            T temp = a[j];
            a[j] = a[to - j - 1];
            a[to - j - 1] = temp;
        }
    }

    public static <T> void reverse(T[] a) {
        reverse(a, a.length);
    }

    public static <T> List<T> sorted(List<T> list, Comparator<? super T> c) {
        List<T> sorted = new ArrayList<T>(list);
        Collections.sort(sorted, c);
        return sorted;
    }

//    public static IntegerVariable[][] transpose(IntegerVariable[][] a) {
//        Check.noNulls(a);
//        if (a.length == 0) {
//            return new IntegerVariable[][]{};
//        }
//        int wide = a[0].length;
//        for (IntegerVariable[] b : a) {
//            Check.noNulls(b);
//            if (b.length != wide) {
//                throw new IllegalArgumentException();
//            }
//        }
//        IntegerVariable[][] z = new IntegerVariable[wide][a.length];
//        for (int i = 0; i < a.length; i++) {
//            for (int j = 0; j < wide; j++) {
//                z[j][i] = a[i][j];
//            }
//        }
//        return z;
//    }
//
//    public static int[] domainSizes(IntDomainVar... vars) {
//        int[] sizes = new int[vars.length];
//        for (int i = 0; i < vars.length; i++) {
//            sizes[i] = vars[i].getDomainSize();
//        }
//        return sizes;
//    }
//
//    public static int maximum(int[] is) {
//        if (is.length == 0) {
//            throw new IllegalArgumentException();
//        }
//        int max = is[0];
//        for (int i = 1; i < is.length; i++) {
//            max = Math.max(max, is[i]);
//        }
//        return max;
//    }
//
    public static boolean in(int item, int[] array) {
        for (int a : array) {
            if (a == item) {
                return true;
            }
        }
        return false;
    }

    public static <T> boolean in(T item, T[] array) {
        for (T a : array) {
            if (a.equals(item)) {
                return true;
            }
        }
        return false;
    }

    public static <T> List<T> cons(T head, List<? extends T> tail) {
        List<T> r = new ArrayList<T>(tail.size() + 1);
        r.add(head);
        r.addAll(tail);
        return r;
    }

    public static <T> List<T> cons(List<? extends T> head, T tail) {
        List<T> r = new ArrayList<T>(head.size() + 1);
        r.addAll(head);
        r.add(tail);
        return r;
    }

    public static <T> T[] cons(T item, T[] list) {
        T[] r = Arrays.copyOf(list, list.length + 1);
        for (int i = r.length - 1; i > 0; i--) {
            r[i] = r[i - 1];
        }
        r[0] = item;
        return r;
    }

    public static <T> T[] cons(T[] list, T item) {
        T[] r = Arrays.copyOf(list, list.length + 1);
        r[list.length] = item;
        return r;
    }

    public static int[] cons(int item, int[] list) {
        int[] r = Arrays.copyOf(list, list.length + 1);
        for (int i = r.length - 1; i > 0; i--) {
            r[i] = r[i - 1];
        }
        r[0] = item;
        return r;
    }

    public static int[] cons(int[] list, int item) {
        int[] r = Arrays.copyOf(list, list.length + 1);
        r[list.length] = item;
        return r;
    }

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

    public static int[] shiftLeft(final int[] array, final int shift) {
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
        return array;
    }

    public static int sum(int... array) {
        int sum = 0;
        for (int a : array) {
            sum += a;
        }
        return sum;
    }

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

    public static int[] iterate(IntIterator it) {
        TIntArrayList i = new TIntArrayList();
        while (it.hasNext()) {
            i.add(it.next());
        }
        return i.toArray();
    }

    public static <T extends Serializable> T copy(T t) {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream out;
        try {
            out = new ObjectOutputStream(baos);
            out.writeObject(t);
            out.close();
            byte[] buf = baos.toByteArray();

            ByteArrayInputStream bin = new ByteArrayInputStream(buf);
            ObjectInputStream in = new ObjectInputStream(bin);
            return (T) in.readObject();
        } catch (IOException e) {
            throw new Error(e);
        } catch (ClassNotFoundException e) {
            throw new Error(e);
        }
    }

    public static String readAll(File in) throws IOException {
        Reader reader = new FileReader(in);
        try {
            return readAll(reader);
        } finally {
            reader.close();
        }
    }

    public static String readAll(InputStream in) throws IOException {
        return readAll(new InputStreamReader(in));
    }

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
