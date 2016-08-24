package org.clafer.domain;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import java.util.PrimitiveIterator;
import java.util.Random;
import java.util.stream.IntStream;
import org.clafer.common.Util;
import org.clafer.test.RepeatRule;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Rule;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class DomainFuzzTest {

    @Rule
    public final RepeatRule repeatRule = new RepeatRule(100);

    private final Random random = new Random();
    private final int n = 11;

    int randInt() {
        return random.nextInt(n) - (n / 2);
    }

    int randPositiveInt() {
        return random.nextInt(n);
    }

    TIntSet randSet() {
        TIntSet set = new TIntHashSet();
        int c = randPositiveInt();
        for (int i = 0; i < c; i++) {
            set.add(randInt());
        }
        return set;
    }

    @Test
    public void testContains() {
        TIntSet set = randSet();
        Domain domain = Domain.enumDomain(set);

        for (int i = 0; i < n; i++) {
            assertEquals(set.contains(i), domain.contains(i));
        }
    }

    @Test
    public void testContainsAll() {
        TIntSet set = randSet();
        Domain domain = Domain.enumDomain(set);

        for (int i = 0; i < n; i++) {
            int a = randInt();
            int b = randInt();
            int low = a < b ? a : b;
            int high = a < b ? b : a;

            boolean containsAll = true;
            for (int j = low; j <= high && containsAll; j++) {
                containsAll &= set.contains(j);
            }
            assertEquals(
                    IntStream.rangeClosed(low, high).allMatch(set::contains),
                    domain.containsAll(low, high));
        }
    }

    @Test
    public void testContainsAny() {
        TIntSet set = randSet();
        Domain domain = Domain.enumDomain(set);

        for (int i = 0; i < n; i++) {
            int a = randInt();
            int b = randInt();
            int low = a < b ? a : b;
            int high = a < b ? b : a;

            assertEquals(
                    IntStream.rangeClosed(low, high).anyMatch(set::contains),
                    domain.containsAny(low, high));
        }
    }

    @Test
    public void testGetLowBound() {
        TIntSet set = randSet();
        Domain domain = Domain.enumDomain(set);

        if (!set.isEmpty()) {
            assertFalse(domain.isEmpty());
            assertEquals(Util.min(set.iterator()), domain.getLowBound());
        }
    }

    @Test
    public void testGetHighBound() {
        TIntSet set = randSet();
        Domain domain = Domain.enumDomain(set);

        if (!set.isEmpty()) {
            assertFalse(domain.isEmpty());
            assertEquals(Util.max(set.iterator()), domain.getHighBound());
        }
    }

    @Test
    public void testIsEmpty() {
        TIntSet set = randSet();
        Domain domain = Domain.enumDomain(set);

        assertEquals(set.isEmpty(), domain.isEmpty());
    }

    @Test
    public void testSize() {
        TIntSet set = randSet();
        Domain domain = Domain.enumDomain(set);

        assertEquals(set.size(), domain.size());
    }

    @Test
    public void testIsSubsetOf() {
        TIntSet subSet = randSet();
        TIntSet supSet = randSet();
        Domain subDomain = Domain.enumDomain(subSet);
        Domain supDomain = Domain.enumDomain(supSet);

        assertEquals(supSet.containsAll(subSet), subDomain.isSubsetOf(supDomain));
    }

    @Test
    public void testIntersects() {
        TIntSet a = randSet();
        TIntSet b = randSet();
        Domain aDomain = Domain.enumDomain(a);
        Domain bDomain = Domain.enumDomain(b);

        a.retainAll(b);
        assertEquals(!a.isEmpty(), aDomain.intersects(bDomain));
    }

    @Test
    public void testInsert() {
        TIntSet set = randSet();
        Domain domain = Domain.enumDomain(set);
        int insert = randInt();

        set.add(insert);
        assertEquals(Domain.enumDomain(set), domain.insert(insert));
    }

    @Test
    public void testRemove() {
        TIntSet set = randSet();
        Domain domain = Domain.enumDomain(set);
        int insert = randInt();

        set.remove(insert);
        assertEquals(Domain.enumDomain(set), domain.remove(insert));
    }

    @Test
    public void testBoundLow() {
        TIntSet set = randSet();
        Domain domain = Domain.enumDomain(set);
        int low = randInt();

        TIntIterator iter = set.iterator();
        while (iter.hasNext()) {
            if (iter.next() < low) {
                iter.remove();
            }
        }
        assertEquals(Domain.enumDomain(set), domain.boundLow(low));
    }

    @Test
    public void testBoundHigh() {
        TIntSet set = randSet();
        Domain domain = Domain.enumDomain(set);
        int high = randInt();

        TIntIterator iter = set.iterator();
        while (iter.hasNext()) {
            if (iter.next() > high) {
                iter.remove();
            }
        }
        assertEquals(Domain.enumDomain(set), domain.boundHigh(high));
    }

    @Test
    public void testBoundBetween() {
        TIntSet set = randSet();
        Domain domain = Domain.enumDomain(set);
        int high = randInt();

        TIntIterator iter = set.iterator();
        while (iter.hasNext()) {
            if (iter.next() > high) {
                iter.remove();
            }
        }
        assertEquals(Domain.enumDomain(set), domain.boundHigh(high));
    }

    @Test
    public void testMinus() {
        TIntSet set = randSet();
        Domain domain = Domain.enumDomain(set);

        TIntSet minus = new TIntHashSet();
        set.forEach(x -> minus.add(-x));
        assertEquals(Domain.enumDomain(minus), domain.minus());
    }

    @Test
    public void testDifference() {
        TIntSet minuend = randSet();
        TIntSet subtrahend = randSet();
        Domain minuendDomain = Domain.enumDomain(minuend);
        Domain subtrahendDomain = Domain.enumDomain(subtrahend);

        minuend.removeAll(subtrahend);
        assertEquals(
                Domain.enumDomain(minuend),
                minuendDomain.difference(subtrahendDomain));
    }

    @Test
    public void testIntersection() {
        TIntSet a = randSet();
        TIntSet b = randSet();
        Domain aDomain = Domain.enumDomain(a);
        Domain bDomain = Domain.enumDomain(b);

        a.retainAll(b);
        assertEquals(
                Domain.enumDomain(a),
                aDomain.intersection(bDomain));
    }

    @Test
    public void testUnion() {
        TIntSet a = randSet();
        TIntSet b = randSet();
        Domain aDomain = Domain.enumDomain(a);
        Domain bDomain = Domain.enumDomain(b);

        a.addAll(b);
        assertEquals(
                Domain.enumDomain(a),
                aDomain.union(bDomain));
    }

    @Test
    public void testOffset() {
        TIntSet set = randSet();
        Domain domain = Domain.enumDomain(set);
        int c = randInt();

        TIntSet offset = new TIntHashSet();
        set.forEach(x -> offset.add(x + c));
        assertEquals(Domain.enumDomain(offset), domain.offset(c));
    }

    @Test
    public void testAdd() {
        TIntSet a = randSet();
        TIntSet b = randSet();
        Domain aDomain = Domain.enumDomain(a);
        Domain bDomain = Domain.enumDomain(b);

        TIntSet sum = new TIntHashSet();
        a.forEach(x -> b.forEach(y -> sum.add(x + y) || true));
        assertEquals(aDomain + " + " + bDomain, Domain.enumDomain(sum), aDomain.add(bDomain));
    }

    @Test
    public void testIterator() {
        TIntSet set = randSet();
        Domain domain = Domain.enumDomain(set);

        TIntSet iterated = new TIntHashSet();
        PrimitiveIterator.OfInt iter = domain.iterator();
        int prev = Integer.MIN_VALUE;
        while (iter.hasNext()) {
            int cur = iter.next();
            assertTrue(prev < cur);
            iterated.add(cur);
            prev = cur;
        }
        assertEquals(Domain.enumDomain(iterated), domain);
    }

    @Test
    public void testReverseIterator() {
        TIntSet set = randSet();
        Domain domain = Domain.enumDomain(set);

        TIntSet iterated = new TIntHashSet();
        PrimitiveIterator.OfInt iter = domain.iterator(false);
        int prev = Integer.MAX_VALUE;
        while (iter.hasNext()) {
            int cur = iter.next();
            assertTrue(domain.toString(), prev > cur);
            iterated.add(cur);
            prev = cur;
        }
        assertEquals(Domain.enumDomain(iterated), domain);
    }

    @Test
    public void testEquals() {
        TIntSet a = randSet();
        TIntSet b = randSet();
        Domain aDomain = Domain.enumDomain(a);
        Domain bDomain = Domain.enumDomain(b);

        assertEquals(
                a.equals(b),
                aDomain.equals(bDomain));
    }

    @Test
    public void testHashCode() {
        TIntSet set = randSet();

        assertEquals(
                Domain.enumDomain(set).hashCode(),
                Domain.enumDomain(set).hashCode());
    }
}
