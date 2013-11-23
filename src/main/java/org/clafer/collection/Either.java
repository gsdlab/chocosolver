package org.clafer.collection;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;
import org.clafer.common.Check;

/**
 * A sum type. Does not allow nulls.
 *
 * @param <A> the type of left
 * @param <B> the type of right
 * @author jimmy
 */
public abstract class Either<A, B> {

    public abstract boolean isLeft();

    public abstract A getLeft();

    public abstract boolean isRight();

    public abstract B getRight();

    public static <A, B> Either<A, B> left(A left) {
        return new Left<>(Check.notNull(left));
    }

    public static <A, B> Either<A, B> right(B right) {
        return new Right<>(Check.notNull(right));
    }

    public static <A, B> List<A> filterLeft(List<Either<A, B>> eithers) {
        List<A> lefts = new ArrayList<>();
        for (Either<A, B> either : eithers) {
            if (either.isLeft()) {
                lefts.add(either.getLeft());
            }
        }
        return lefts;
    }

    @SafeVarargs
    public static <A, B> A[] filterLeft(Either<A, B>[] eithers, A... array) {
        List<A> lefts = new ArrayList<>();
        for (Either<A, B> either : eithers) {
            if (either.isLeft()) {
                lefts.add(either.getLeft());
            }
        }
        @SuppressWarnings("unchecked")
        A[] to = array.length == lefts.size()
                ? array
                : (A[]) Array.newInstance(array.getClass().getComponentType(), lefts.size());
        return lefts.toArray(to);
    }

    public static <A, B> List<B> filterRight(List<Either<A, B>> eithers) {
        List<B> rights = new ArrayList<>();
        for (Either<A, B> either : eithers) {
            if (either.isRight()) {
                rights.add(either.getRight());
            }
        }
        return rights;
    }

    @SafeVarargs
    public static <A, B> B[] filterRight(Either<A, B>[] eithers, B... array) {
        List<B> rights = new ArrayList<>();
        for (Either<A, B> either : eithers) {
            if (either.isRight()) {
                rights.add(either.getRight());
            }
        }
        @SuppressWarnings("unchecked")
        B[] to = array.length == rights.size()
                ? array
                : (B[]) Array.newInstance(array.getClass().getComponentType(), rights.size());
        return rights.toArray(to);
    }
}
