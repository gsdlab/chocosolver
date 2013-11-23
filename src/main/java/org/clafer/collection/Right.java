package org.clafer.collection;

import org.clafer.common.Check;

/**
 * @param <A> the type of left
 * @param <B> the type of right
 * @author jimmy
 */
public class Right<A, B> extends Either<A, B> {

    private final B value;

    Right(B value) {
        this.value = Check.notNull(value);
    }

    public B getValue() {
        return value;
    }

    @Override
    public boolean isLeft() {
        return false;
    }

    @Override
    public A getLeft() {
        throw new UnsupportedOperationException("Right.getLeft");
    }

    @Override
    public boolean isRight() {
        return true;
    }

    @Override
    public B getRight() {
        return value;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Right<?, ?>) {
            Right<?, ?> other = (Right<?, ?>) obj;
            return value.equals(other.value);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 7 * value.hashCode();
    }

    @Override
    public String toString() {
        return "Right " + value;
    }
}
