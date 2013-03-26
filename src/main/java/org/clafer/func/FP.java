package org.clafer.func;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 *
 * @author jimmy
 */
public class FP {

    public static <T> Func<T[], Integer> length() {
        return new Func<T[], Integer>() {

            @Override
            public Integer apply(T[] param) {
                return param.length;
            }
        };
    }

    public static int sum(List<Integer> is) {
        int sum = 0;
        for (Integer i : is) {
            sum += i.intValue();
        }
        return sum;
    }

    public static <A, B, C> Func<A, C> compose(final Func<B, C> f1, final Func<A, B> f2) {
        return new Func<A, C>() {

            @Override
            public C apply(A param) {
                return f1.apply(f2.apply(param));
            }
        };
    }

    public static boolean all(List<Boolean> bools) {
        for (Boolean bool : bools) {
            if (bool.equals(Boolean.FALSE)) {
                return false;
            }
        }
        return true;
    }

    public static <T> boolean same(Iterable<T> ts) {
        Iterator<T> iter = ts.iterator();
        if (iter.hasNext()) {
            T t = iter.next();
            while (iter.hasNext()) {
                if (!t.equals(iter.next())) {
                    return false;
                }
            }
        }
        return true;
    }

    public static <A, B> List<B> mapped(List<? extends A> as, Func<A, B> f) {
        List<B> bs = new ArrayList<B>(as.size());
        for (A a : as) {
            bs.add(f.apply(a));
        }
        return bs;
    }

    public static <A, B> B[] mapped(A[] as, Func<A, B> f) {
        if (as.length == 0) {
            throw new IllegalArgumentException();
        }
        B b = f.apply(as[0]);
        B[] bs = (B[]) Array.newInstance(b.getClass(), as.length);
        bs[0] = b;
        for (int i = 1; i < bs.length; i++) {
            bs[i] = f.apply(as[i]);
        }
        return bs;
    }
}
