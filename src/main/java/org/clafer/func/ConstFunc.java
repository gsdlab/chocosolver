package org.clafer.func;

/**
 *
 * @author jimmy
 */
public class ConstFunc<A, B> implements Func<A, B> {

    private final B value;

    public ConstFunc(B value) {
        this.value = value;
    }

    @Override
    public B apply(A arg) {
        return value;
    }
}
