package org.clafer.collection;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;
import org.clafer.common.Check;

/**
 * A 2-tuple.
 *
 *
 * @param <A> the type of fst
 * @param <B> the type of snd
 * @author jimmy
 */
public class Pair<A, B> {

    private final A fst;
    private final B snd;

    /**
     * Construct a tuple.
     *
     * @param fst
     * @param snd
     */
    public Pair(A fst, B snd) {
        this.fst = Check.notNull(fst);
        this.snd = Check.notNull(snd);
    }

    public Pair(Pair<? extends A, ? extends B> pair) {
        this(pair.getFst(), pair.getSnd());
    }

    public A getFst() {
        return fst;
    }

    public B getSnd() {
        return snd;
    }

    /**
     * Returns the first element of the tuples in the same order. Equivalent to
     * the Haskell code {@code map fst}.
     *
     * @param <A> the type of first element in the pairs
     * @param <B> the type of second element in the pairs
     * @param pairs the tuples
     * @return the first element of the tuples
     */
    public static <A, B> List<A> mapFst(List<Pair<A, B>> pairs) {
        List<A> fsts = new ArrayList<A>(pairs.size());
        for (Pair<A, B> pair : pairs) {
            fsts.add(pair.getFst());
        }
        return fsts;
    }

    /**
     * Returns the first element of the tuples in the same order. Equivalent to
     * the Haskell code {@code map fst}.
     *
     * @param <A> the type of first element in the pairs
     * @param <B> the type of second element in the pairs
     * @param pairs the tuples
     * @param array the array to write to if the size is correct
     * @return the first element of the tuples
     */
    public static <A, B> A[] mapFst(Pair<A, B>[] pairs, A... array) {
        @SuppressWarnings("unchecked")
        A[] to = array.length == pairs.length
                ? array
                : (A[]) Array.newInstance(array.getClass().getComponentType(), pairs.length);
        for (int i = 0; i < to.length; i++) {
            to[i] = pairs[i].getFst();
        }
        return to;
    }

    /**
     * Returns the second element of the tuples in the same order. Equivalent to
     * the Haskell code {@code map snd}.
     *
     * @param <A> the type of first element in the pairs
     * @param <B> the type of second element in the pairs
     * @param pairs the tuples
     * @return the second element of the tuples
     */
    public static <A, B> List<B> mapSnd(List<Pair<A, B>> pairs) {
        List<B> snds = new ArrayList<B>(pairs.size());
        for (Pair<A, B> pair : pairs) {
            snds.add(pair.getSnd());
        }
        return snds;
    }

    /**
     * Returns the second element of the tuples in the same order. Equivalent to
     * the Haskell code {@code map snd}.
     *
     * @param <A> the type of first element in the pairs
     * @param <B> the type of second element in the pairs
     * @param pairs the tuples
     * @param array the array to write to if the size is correct
     * @return the second element of the tuples
     */
    public static <A, B> B[] mapSnd(Pair<A, B>[] pairs, B... array) {
        @SuppressWarnings("unchecked")
        B[] to = array.length == pairs.length
                ? array
                : (B[]) Array.newInstance(array.getClass().getComponentType(), pairs.length);
        for (int i = 0; i < to.length; i++) {
            to[i] = pairs[i].getSnd();
        }
        return to;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Pair<?, ?>) {
            Pair<?, ?> other = (Pair<?, ?>) obj;
            return fst.equals(other.fst) && snd.equals(other.snd);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return fst.hashCode() ^ snd.hashCode();
    }

    @Override
    public String toString() {
        return "(" + fst + ", " + snd + ")";
    }
}
