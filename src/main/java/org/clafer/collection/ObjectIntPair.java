package org.clafer.collection;

import java.util.Comparator;
import org.clafer.Check;

/**
 *
 * @author jimmy
 */
public class ObjectIntPair<A> {

//    private final A fst;
//    private final int snd;
//
//    public ObjectIntPair(A fst, int snd) {
//        this.fst = Check.notNull(fst);
//        this.snd = snd;
//    }
//
//    public A getFst() {
//        return fst;
//    }
//
//    public int getSnd() {
//        return snd;
//    }
//
//    @Override
//    public int hashCode() {
//        return fst.hashCode() ^ snd;
//    }
//
//    @Override
//    public boolean equals(Object obj) {
//        if (obj instanceof ObjectIntPair< ?>) {
//            ObjectIntPair<?> other = (ObjectIntPair<?>) obj;
//
//            return fst.equals(other.fst) && snd == other.snd;
//        }
//        return false;
//    }
//
//    @Override
//    public String toString() {
//        return "(" + fst + ", " + snd + ")";
//    }
//    public static final Comparator<ObjectIntPair<Comparable>> fstComparator = new Comparator<ObjectIntPair<Comparable>>() {
//
//        @Override
//        public int compare(ObjectIntPair<Comparable> o1, ObjectIntPair<Comparable> o2) {
//            return o1.getFst().compareTo(o2.getFst());
//        }
//    };
//    public static final Comparator<ObjectIntPair> sndComparator = new Comparator<ObjectIntPair>() {
//
//        @Override
//        public int compare(ObjectIntPair o1, ObjectIntPair o2) {
//            int x = o1.getSnd();
//            int y = o2.getSnd();
//            return (x < y) ? -1 : ((x == y) ? 0 : 1);
//        }
//    };
}
