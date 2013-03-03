package org.clafer;

/**
 *
 * @author jimmy
 */
public class Check {

    public static <T> T[] noNulls(T... ts) {
        Check.notNull(ts);
        for(T t : ts) {
            Check.notNull(t);
        }
        return ts;
    }

    public static <T> T notNull(T t) {
        if (t == null) {
            throw new NullPointerException();
        }
        return t;
    }
}
