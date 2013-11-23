package org.clafer.collection;

import org.clafer.common.Check;

/**
 * @param <A> the type of left
 * @param <B> the type of right
 * @author jimmy
 */
public class Left<A, B> extends Either<A, B> {

    private final A value;

    Left(A value) {
        this.value = Check.notNull(value);
    }

    public A getValue() {
        return value;
    }

    @Override
    public boolean isLeft() {
        return true;
    }

    @Override
    public A getLeft() {
        return value;
    }

    @Override
    public boolean isRight() {
        return false;
    }

    @Override
    public B getRight() {
        throw new UnsupportedOperationException("Left.getRight");
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Left<?, ?>) {
            Left<?, ?> other = (Left<?, ?>) obj;
            return value.equals(other.value);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 11 * value.hashCode();
    }

    @Override
    public String toString() {
        return "Left" + value;
    }
}
