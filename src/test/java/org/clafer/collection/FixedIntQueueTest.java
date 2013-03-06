package org.clafer.collection;

import gnu.trove.TIntArrayList;
import java.util.Collections;
import java.util.PriorityQueue;
import java.util.Random;
import org.clafer.Util;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author jimmy
 */
public class FixedIntQueueTest {

    private int[] array(int... is) {
        return is;
    }

    private int[] iterate(FixedIntQueue q) {
        return Util.iterate(q.toIterator());
    }

    private int[] iterate(PriorityQueue<Integer> q, int top) {
        q = new PriorityQueue<Integer>(q);
        TIntArrayList a = new TIntArrayList();
        while (a.size() < top && !q.isEmpty()) {
            a.add(q.poll());
        }
        return a.toNativeArray();
    }

    @Test
    public void randomMinTest() {
        Random rand = new Random();

        for (int repeat = 0; repeat < 10; repeat++) {
            final int size = rand.nextInt(10) + 1;

            FixedIntQueue q = FixedIntQueue.smallest(size);
            PriorityQueue<Integer> minQ = new PriorityQueue<Integer>(size);

            for (int i = 0; i < size * 2; i++) {
                int item = rand.nextInt(10);

                q.add(item);
                minQ.add(item);

                assertArrayEquals(
                        iterate(minQ, Math.min(i + 1, size)),
                        iterate(q));
            }
        }
    }

    @Test
    public void randomMaxTest() {
        Random rand = new Random();

        for (int repeat = 0; repeat < 10; repeat++) {
            final int size = rand.nextInt(10) + 1;

            FixedIntQueue q = FixedIntQueue.largest(size);
            PriorityQueue<Integer> maxQ = new PriorityQueue<Integer>(size, Collections.reverseOrder());

            for (int i = 0; i < size * 2; i++) {
                int item = rand.nextInt(10);

                q.add(item);
                maxQ.add(item);

                assertArrayEquals(
                        iterate(maxQ, Math.min(i + 1, size)),
                        iterate(q));
            }
        }
    }

    @Test
    public void testMin() {
        FixedIntQueue q = FixedIntQueue.smallest(4);

        assertArrayEquals(array(), iterate(q));
        q.add(4);
        assertArrayEquals(array(4), iterate(q));
        q.add(3);
        assertArrayEquals(array(3, 4), iterate(q));
        q.add(2);
        assertArrayEquals(array(2, 3, 4), iterate(q));
        q.add(4);
        assertArrayEquals(array(2, 3, 4, 4), iterate(q));
        q.add(5);
        assertArrayEquals(array(2, 3, 4, 4), iterate(q));
        q.add(1);
        assertArrayEquals(array(1, 2, 3, 4), iterate(q));
    }

    @Test
    public void testMax() {
        FixedIntQueue q = FixedIntQueue.largest(4);

        assertArrayEquals(array(), iterate(q));
        q.add(4);
        assertArrayEquals(array(4), iterate(q));
        q.add(3);
        assertArrayEquals(array(4, 3), iterate(q));
        q.add(2);
        assertArrayEquals(array(4, 3, 2), iterate(q));
        q.add(4);
        assertArrayEquals(array(4, 4, 3, 2), iterate(q));
        q.add(5);
        assertArrayEquals(array(5, 4, 4, 3), iterate(q));
        q.add(1);
        assertArrayEquals(array(5, 4, 4, 3), iterate(q));
    }
}
