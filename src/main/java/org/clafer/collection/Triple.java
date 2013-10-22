package org.clafer.collection;

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

    public static <A, B, C> List<A> mapFst(List<Triple<A, B, C>> triples) {
        List<A> fsts = new ArrayList<A>(triples.size());
        for (Triple<A, B, C> triple : triples) {
            fsts.add(triple.getFst());
        }
        return fsts;
    }

    public static <A, B, C> List<B> mapSnd(List<Triple<A, B, C>> triples) {
        List<B> snds = new ArrayList<B>(triples.size());
        for (Triple<A, B, C> triple : triples) {
            snds.add(triple.getSnd());
        }
        return snds;
    }

    public static <A, B, C> List<C> mapThd(List<Triple<A, B, C>> triples) {
        List<C> snds = new ArrayList<C>(triples.size());
        for (Triple<A, B, C> triple : triples) {
            snds.add(triple.getThd());
        }
        return snds;
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