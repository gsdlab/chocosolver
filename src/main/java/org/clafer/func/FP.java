package org.clafer.func;

import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import org.clafer.tree.ConcreteClafer;

/**
 *
 * @author jimmy
 */
public class FP {

    public static final Func<SetVariable, IntegerVariable> getCard =
            new Func<SetVariable, IntegerVariable>() {

                @Override
                public IntegerVariable apply(SetVariable param) {
                    return param.getCard();
                }
            };
    public static final Func<ConcreteClafer, SetVariable[]> getChildSet =
            new Func<ConcreteClafer, SetVariable[]>() {

                @Override
                public SetVariable[] apply(ConcreteClafer param) {
                    return param.getChildSet();
                }
            };

    public static <T> Func<T[], Integer> length() {
        return new Func<T[], Integer>() {

            @Override
            public Integer apply(T[] param) {
                return param.length;
            }
        };
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

    public static <A, B> List<B> mapped(List<A> as, Func<A, B> f) {
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

    public static void main(String[] args) {
        System.out.println(
                Arrays.toString(
                mapped(
                new String[]{"abc", "d", "ef"},
                new Func<String, Integer>() {

                    @Override
                    public Integer apply(String param) {
                        return param.length();
                    }
                })));
    }
}
