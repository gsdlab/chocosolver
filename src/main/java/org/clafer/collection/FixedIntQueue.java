package org.clafer.collection;

import java.util.Arrays;
import org.clafer.Util;

/**
 * Fixed upper bound on size for space efficiency reasons for implementing SumSet.
 * 
 * For example, if size's upper bound is 10, then after adding 100 ints, the
 * heap contains the 10 smallest/largest ints. Essentially a simple online
 * selection algorithm.
 * 
 * Every add scans the entire queue. Can have better asymptotic performance using
 * a double ended priority queue (ie. min-max heap). Capacity is bounded by cardinality
 * of the set in SumSet which is likely small. Scanning is likely more efficient.
 * 
 * @author jimmy
 */
public class FixedIntQueue {

    private final int[] queue;
    private int size = 0;
    // True = smallest ints
    // False = largest ints
    private boolean smallest;

    private FixedIntQueue(int capacity, boolean smallest) {
        if (capacity <= 0) {
            throw new IllegalArgumentException();
        }
        this.queue = new int[capacity];
        this.smallest = smallest;
    }

    private boolean compare(int a, int b) {
        if (smallest) {
            return a < b;
        }
        return a > b;
    }

    public void add(int val) {
        if (size < queue.length) {
            queue[size++] = val;
        } else {
            // Heap is filled. Doesn't allow capacity==0.
            int critical = queue[0];
            int criticalIndex = 0;

            for (int i = 1; i < queue.length; i++) {
                if (compare(critical, queue[i])) {
                    critical = queue[i];
                    criticalIndex = i;
                }
            }

            if (compare(val, critical)) {
                queue[criticalIndex] = val;
            }
        }
    }

    /**
     * @return Iterate over the queue in order.
     * 
     * Smallest - ascending order
     * Largest - descending order
     */
    public IntIterator toIterator() {
        Arrays.sort(queue, 0, size);
        if (!smallest) {
            Util.reverse(queue, size);
        }
        return new IntArrayIterator(queue, 0, size);
    }

    @Override
    public String toString() {
        IntIterator it = toIterator();
        if (it.hasNext()) {
            StringBuilder result = new StringBuilder();
            result.append('[');
            result.append(it.next());
            while (it.hasNext()) {
                result.append(", ");
                result.append(it.next());
            }
            return result.append(']').toString();
        } else {
            return "[]";
        }
    }

    public static FixedIntQueue smallest(int capacity) {
        return new FixedIntQueue(capacity, true);
    }

    public static FixedIntQueue largest(int capacity) {
        return new FixedIntQueue(capacity, false);
    }
}
