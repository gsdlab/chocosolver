package org.clafer.collection;

import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class Pair<A, B> {

    private final A fst;
    private final B snd;

    public Pair(A fst, B snd) {
        this.fst = Check.notNull(fst);
        this.snd = Check.notNull(snd);
    }

    public A getFst() {
        return fst;
    }

    public B getSnd() {
        return snd;
    }

    @Override
    public int hashCode() {
        return fst.hashCode() ^ snd.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Pair<?, ?>) {
            Pair<?, ?> other = (Pair<?, ?>) obj;

            return fst.equals(other.fst) && snd.equals(other.snd);
        }
        return false;
    }
}
