package org.clafer.collection;

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
        this.fst = fst;
        this.snd = snd;
        this.thd = thd;
    }

    public Triple(Pair<A, B> pair, C thd) {
        this.fst = pair.getFst();
        this.snd = pair.getSnd();
        this.thd = thd;
    }

    public Triple(A fst, Pair<B, C> pair) {
        this.fst = fst;
        this.snd = pair.getFst();
        this.thd = pair.getSnd();
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