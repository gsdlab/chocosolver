package org.clafer;

import java.util.Arrays;

/**
 *
 * @author jimmy
 */
public class Util {

    public static <T> T[] combine(T[] first, T[] second) {
        T[] result = Arrays.copyOf(first, first.length + second.length);
        System.arraycopy(second, 0, result, first.length, second.length);
        return result;
    }
}
