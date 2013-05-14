package org.clafer.collection;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.clafer.Util;

/**
 * Persistent singly-linked list. Empty list is represented by null.
 * 
 * @author jimmy
 */
public class FList<E> implements Iterable<E> {

    private final E head;
    private final FList<E> tail;

    public FList(E head, FList<E> tail) {
        this.head = head;
        this.tail = tail;
    }

    public static <E> boolean isEmpty(FList<E> list) {
        return list == null;
    }

    public static <E> FList<E> empty() {
        return null;
    }

    public static <E> FList<E> single(E item) {
        return new FList<E>(item, null);
    }

    public static <E> FList<E> cons(E head, FList<E> tail) {
        return new FList<E>(head, tail);
    }

    public static <E> FList<E> snoc(FList<E> head, E tail) {
        if (isEmpty(head)) {
            return single(tail);
        }
        return cons(head.head, snoc(head.tail, tail));
    }

    public static <E> boolean equals(FList<E> l1, FList<E> l2) {
        if (isEmpty(l1)) {
            return isEmpty(l2);
        }
        if (isEmpty(l2)) {
            return isEmpty(l1);
        }
        if (isEmpty(l1.tail)) {
            return isEmpty(l2.tail);
        }
        if (isEmpty(l2.tail)) {
            return false;
        }
        return Util.equals(l1.head, l2.head) && equals(l1.tail, l2.tail);
    }

    public E getHead() {
        return head;
    }

    public FList<E> getTail() {
        return tail;
    }

    public List<E> toList() {
        List<E> list = new ArrayList<E>();
        for (FList<E> current = this; current != null; current = current.tail) {
            list.add(current.head);
        }
        return list;
    }

    @Override
    public Iterator<E> iterator() {
        return new FListIterator<E>(this);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof FList) {
            FList<E> other = (FList) obj;
            return equals(this, other);
        }
        return false;
    }

    @Override
    public int hashCode() {
        int hash = 1;
        for (FList<E> current = this; current != null; current = current.tail) {
            hash = 31 * hash + (current.head == null ? 0 : current.head.hashCode());
        }
        return hash;
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append('[');
        result.append(head);
        for (FList<E> current = tail; current != null; current = current.tail) {
            result.append(", ");
            result.append(current.head);
        }
        return result.append(']').toString();
    }

    private static class FListIterator<E> implements Iterator<E> {

        private FList<E> current;

        FListIterator(FList<E> current) {
            this.current = current;
        }

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public E next() {
            E head = current.getHead();
            current = current.getTail();
            return head;
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }
    }
}
