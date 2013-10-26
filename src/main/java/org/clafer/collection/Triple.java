package org.clafer.collection;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;
import org.clafer.common.Check;
import org.clafer.common.Util;

/**
 * A 3-tuple.
 *
 *
 * @param <A> the type of fst
 * @param <B> the type of snd
 * @param <C> the type of thd
 * @author jimmy
 */
public class Triple<A, B, C> {

    private final A fst;
    private final B snd;
    private final C thd;

    public Triple(A fst, B snd, C thd) {
        this.fst = Check.notNull(fst);
        this.snd = Check.notNull(snd);
        this.thd = Check.notNull(thd);
    }

    public Triple(Pair<? extends A, ? extends B> pair, C thd) {
        this(pair.getFst(), pair.getSnd(), thd);
    }

    public Triple(A fst, Pair<? extends B, ? extends C> pair) {
        this(fst, pair.getFst(), pair.getSnd());
    }

    public Triple(Triple<? extends A, ? extends B, ? extends C> triple) {
        this(triple.getFst(), triple.getSnd(), triple.getThd());
    }

    public A getFst() {
        return fst;
    }

    public B getSnd() {
        return snd;
    }

    public C getThd() {
        return thd;
    }

    public Pair<A, B> getFstSnd() {
        return new Pair<A, B>(fst, snd);
    }

    public Pair<B, C> getSndThd() {
        return new Pair<B, C>(snd, thd);
    }

    /**
     * Returns the first element of the tuples in the same order. Equivalent to
     * the Haskell code {@code map fst}.
     *
     * @param <A> the type of first element in the pairs
     * @param <B> the type of second element in the pairs
     * @param <C> the type of third element in the pairs
     * @param triples the tuples
     * @return the first element of the tuples
     */
    public static <A, B, C> List<A> mapFst(List<Triple<A, B, C>> triples) {
        List<A> fsts = new ArrayList<A>(triples.size());
        for (Triple<A, B, C> triple : triples) {
            fsts.add(triple.getFst());
        }
        return fsts;
    }

    /**
     * Returns the first element of the tuples in the same order. Equivalent to
     * the Haskell code {@code map fst}.
     *
     * @param <A> the type of first element in the pairs
     * @param <B> the type of second element in the pairs
     * @param <C> the type of third element in the pairs
     * @param triples the tuples
     * @param array the array to write to if the size is correct
     * @return the first element of the tuples
     */
    public static <A, B, C> A[] mapFst(Triple<A, B, C>[] triples, A... array) {
        @SuppressWarnings("unchecked")
        A[] to = array.length == triples.length
                ? array
                : (A[]) Array.newInstance(array.getClass().getComponentType(), triples.length);
        for (int i = 0; i < to.length; i++) {
            to[i] = triples[i].getFst();
        }
        return to;
    }

    /**
     * Returns the second element of the tuples in the same order. Equivalent to
     * the Haskell code {@code map snd}.
     *
     * @param <A> the type of first element in the pairs
     * @param <B> the type of second element in the pairs
     * @param <C> the type of third element in the pairs
     * @param triples the tuples
     * @return the second element of the tuples
     */
    public static <A, B, C> List<B> mapSnd(List<Triple<A, B, C>> triples) {
        List<B> snds = new ArrayList<B>(triples.size());
        for (Triple<A, B, C> triple : triples) {
            snds.add(triple.getSnd());
        }
        return snds;
    }

    /**
     * Returns the second element of the tuples in the same order. Equivalent to
     * the Haskell code {@code map snd}.
     *
     * @param <A> the type of first element in the pairs
     * @param <B> the type of second element in the pairs
     * @param <C> the type of third element in the pairs
     * @param triples the tuples
     * @param array the array to write to if the size is correct
     * @return the second element of the tuples
     */
    public static <A, B, C> B[] mapSnd(Triple<A, B, C>[] triples, B... array) {
        @SuppressWarnings("unchecked")
        B[] to = array.length == triples.length
                ? array
                : (B[]) Array.newInstance(array.getClass().getComponentType(), triples.length);
        for (int i = 0; i < to.length; i++) {
            to[i] = triples[i].getSnd();
        }
        return to;
    }

    /**
     * Returns the third element of the tuples in the same order. Equivalent to
     * the Haskell code {@code map thd}.
     *
     * @param <A> the type of first element in the pairs
     * @param <B> the type of second element in the pairs
     * @param <C> the type of third element in the pairs
     * @param triples the tuples
     * @return the third element of the tuples
     */
    public static <A, B, C> List<C> mapThd(List<Triple<A, B, C>> triples) {
        List<C> thds = new ArrayList<C>(triples.size());
        for (Triple<A, B, C> triple : triples) {
            thds.add(triple.getThd());
        }
        return thds;
    }

    /**
     * Returns the third element of the tuples in the same order. Equivalent to
     * the Haskell code {@code map thd}.
     *
     * @param <A> the type of first element in the pairs
     * @param <B> the type of second element in the pairs
     * @param <C> the type of third element in the pairs
     * @param triples the tuples
     * @param array the array to write to if the size is correct
     * @return the third element of the tuples
     */
    public static <A, B, C> C[] mapThd(Triple<A, B, C>[] triples, C... array) {
        @SuppressWarnings("unchecked")
        C[] to = array.length == triples.length
                ? array
                : (C[]) Array.newInstance(array.getClass().getComponentType(), triples.length);
        for (int i = 0; i < to.length; i++) {
            to[i] = triples[i].getThd();
        }
        return to;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Triple<?, ?, ?>) {
            Triple<?, ?, ?> other = (Triple<?, ?, ?>) obj;
            return Util.equals(fst, other.fst) && Util.equals(snd, other.snd) && Util.equals(thd, other.thd);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Util.hashCode(fst) ^ Util.hashCode(snd) ^ Util.hashCode(thd);
    }

    @Override
    public String toString() {
        return "(" + fst + ", " + snd + ", " + thd + ")";
    }
}