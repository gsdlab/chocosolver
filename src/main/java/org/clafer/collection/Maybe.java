package org.clafer.collection;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 * A option type. Does not allow nulls.
 *
 * @param <T> the type of just
 * @author jimmy
 */
public class Maybe<T> {

    private static final Maybe<?> Nothing = new Maybe<>(false, null);
    private final boolean isJust;
    private final T t;

    private Maybe(boolean isJust, T t) {
        this.isJust = isJust;
        this.t = t;
    }

    public boolean isNothing() {
        return !isJust();
    }

    public boolean isJust() {
        return isJust;
    }

    public T fromJust() {
        assert isJust();
        return t;
    }

    public static <T> Maybe<T> nothing() {
        @SuppressWarnings("unchecked")
        Maybe<T> maybe = (Maybe<T>) Nothing;
        return maybe;
    }

    public static <T> Maybe<T> just(T t) {
        return new Maybe<>(true, Check.notNull(t));
    }

    public static <T> List<T> filterJust(List<Maybe<? extends T>> maybes) {
        List<T> justs = new ArrayList<>();
        for (Maybe<? extends T> maybe : maybes) {
            if (maybe.isJust()) {
                justs.add(maybe.fromJust());
            }
        }
        return justs;
    }

    public static <T> T[] filterJust(Maybe<? extends T>[] maybes, T... array) {
        List<T> justs = new ArrayList<>();
        for (Maybe<? extends T> maybe : maybes) {
            if (maybe.isJust()) {
                justs.add(maybe.fromJust());
            }
        }
        @SuppressWarnings("unchecked")
        T[] to = array.length == justs.size()
                ? array
                : (T[]) Array.newInstance(array.getClass().getComponentType(), justs.size());
        return justs.toArray(to);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Maybe<?>) {
            Maybe<?> other = (Maybe<?>) obj;
            return isNothing() == other.isNothing() || fromJust().equals(other.fromJust());
        }
        return false;
    }

    @Override
    public int hashCode() {
        return (isJust ? 1231 : 1237) ^ Util.hashCode(t);
    }

    @Override
    public String toString() {
        return isJust ? "Just " + t : "Nothing";
    }
}
