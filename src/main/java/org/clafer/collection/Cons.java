package org.clafer.collection;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.clafer.common.Check;

/**
 *
 * @author jimmy
 */
public class Cons<E> extends FList<E> {

    private final E head;
    private final FList<E> tail;

    Cons(E head, FList<E> tail) {
        this.head = Check.notNull(head);
        this.tail = Check.notNull(tail);
    }

    @Override
    public E getHead() {
        return head;
    }

    @Override
    public FList<E> getTail() {
        return tail;
    }

    @Override
    public boolean isEmpty() {
        return false;
    }

    @Override
    public List<E> toList() {
        List<E> list = new ArrayList<>();
        for (FList<E> current = this; !current.isEmpty(); current = current.getTail()) {
            list.add(current.getHead());
        }
        return list;
    }

    @Override
    public Iterator<E> iterator() {
        return new FListIterator<>(this);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Cons<?>) {
            Cons<?> other = (Cons<?>) obj;
            return head.equals(other.head) && tail.equals(other.tail);
        }
        return false;
    }

    @Override
    public int hashCode() {
        int hash = 1;
        for (FList<E> current = this; !current.isEmpty(); current = current.getTail()) {
            hash = 31 * hash + current.getHead().hashCode();
        }
        return hash;
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        result.append('[');
        result.append(head);
        for (FList<E> current = tail; !current.isEmpty(); current = current.getTail()) {
            result.append(", ");
            result.append(current.getHead());
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
            return !current.isEmpty();
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
