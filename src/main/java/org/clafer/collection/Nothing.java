package org.clafer.collection;

/**
 *
 * @param <T> the type of maybe
 * @author jimmy
 */
public class Nothing<T> extends Maybe<T> {

    Nothing() {
    }

    @Override
    public boolean isNothing() {
        return true;
    }

    @Override
    public boolean isJust() {
        return false;
    }

    @Override
    public T fromMaybe(T t) {
        return t;
    }

    @Override
    public boolean equals(Object obj) {
        return this == obj;
    }

    @Override
    public int hashCode() {
        return 731;
    }

    @Override
    public String toString() {
        return "Nothing";
    }
}
