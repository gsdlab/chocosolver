package org.clafer.func;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class MinFunc implements Func<Integer, Integer> {

    private final Func<Integer, Integer> e1, e2;

    public MinFunc(Func<Integer, Integer> e1, Func<Integer, Integer> e2) {
        this.e1 = Check.notNull(e1);
        this.e2 = Check.notNull(e2);
    }

    public MinFunc(Func<Integer, Integer> e1, int e2) {
        this(e1, new ConstFunc<Integer, Integer>(e2));
    }

    @Override
    public Integer apply(Integer arg) {
        return Math.min(e1.apply(arg), e2.apply(arg));
    }
}
