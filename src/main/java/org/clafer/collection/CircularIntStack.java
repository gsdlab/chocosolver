package org.clafer.collection;

import gnu.trove.TIntCollection;
import gnu.trove.iterator.TIntIterator;
import gnu.trove.queue.TIntQueue;
import gnu.trove.stack.TIntStack;

/**
 * Fixed size circular buffer.
 *
 * @author jimmy
 */
public class CircularIntStack {

    private final int[] array;
    private int index = -1;
    private int size = 0;

    public CircularIntStack(int capacity) {
        this.array = new int[capacity];
    }

    public boolean isEmpty() {
        return size == 0;
    }

    public int size() {
        return size;
    }

    public boolean addAll(TIntCollection collection) {
        TIntIterator iter = collection.iterator();
        while (iter.hasNext()) {
            push(iter.next());
        }
        return true;
    }

    public void push(int e) {
        TIntQueue a;
        int capacity = array.length;
        if (size == capacity) {
            throw new IllegalStateException();
        }
        index++;
        if (index == capacity) {
            index = 0;
        }
        array[index] = e;

        size++;
    }

    public int peek() {
        if (isEmpty()) {
            throw new IllegalStateException();
        }
        return array[index];
    }

    public int pop() {
        if (isEmpty()) {
            throw new IllegalStateException();
        }
        int e = array[index];
        index--;
        if (index == -1) {
            index = array.length - 1;
        }
        size--;
        return e;
    }
}
