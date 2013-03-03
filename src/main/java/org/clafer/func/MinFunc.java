package org.clafer.func;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class MinFunc implements IntFunc {

    private final IntFunc e1, e2;

    public MinFunc(IntFunc e1, IntFunc e2) {
        this.e1 = Check.notNull(e1);
        this.e2 = Check.notNull(e2);
    }

    public MinFunc(IntFunc e1, int e2) {
        this(e1, new ConstFunc(e2));
    }

    @Override
    public int apply(int arg) {
        return Math.min(e1.apply(arg), e2.apply(arg));
    }
}
