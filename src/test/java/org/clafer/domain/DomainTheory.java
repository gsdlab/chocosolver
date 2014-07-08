package org.clafer.domain;

import static org.junit.Assert.*;
import static org.junit.Assume.assumeTrue;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(Theories.class)
public class DomainTheory {

    private void assertImplies(boolean a, boolean b) {
        assertTrue(!a || b);
    }

    @DataPoints
    public static final int[] ints
            = new int[]{-2, -1, 0, 1, 2};

    @DataPoints
    public static final Domain[] domains
            = new Domain[]{
                new EmptyDomain(),
                BoolDomain.TrueDomain,
                BoolDomain.FalseDomain,
                BoolDomain.TrueFalseDomain,
                new EnumDomain(-2, -1, 0, 1, 2),
                new EnumDomain(-2, -1, 0, 1),
                new EnumDomain(-1, 0, 1, 2),
                new EnumDomain(-2, 0, 1, 2),
                new EnumDomain(0, 1, 2),
                new EnumDomain(-2, 2),
                new EnumDomain(-1, 1),
                new EnumDomain(2),
                new EnumDomain(-1),
                new BoundDomain(-2, 2),
                new BoundDomain(-1, 2),
                new BoundDomain(-2, 0),
                new BoundDomain(-1, 1),
                new BoundDomain(-1, -1),
                new BoundDomain(2, 2),
                new BoundDomain(1, 2)
            };

    @Theory
    public void equalsReflexiveProperty(Domain d) {
        assertTrue(d.equals(d));
    }

    @Theory
    public void equalsSymmetricProperty(Domain d1, Domain d2) {
        assertEquals(d1.equals(d2), d2.equals(d1));
    }

    @Theory
    public void equalsTransitiveProperty(Domain d1, Domain d2, Domain d3) {
        assertImplies(d1.equals(d2) && d2.equals(d3), d1.equals(d3));
    }

    @Theory
    public void isSubsetOfAxiom(Domain d1, Domain d2) {
        if (d1.isSubsetOf(d2)) {
            for (int i : d1.getValues()) {
                assertTrue(d2.contains(i));
            }
        } else {
            for (int i : d1.getValues()) {
                if (!d2.contains(i)) {
                    return;
                }
            }
            fail(d1 + " is a subset of " + d2);
        }
    }

    @Theory
    public void intersectsAxiom(Domain d1, Domain d2) {
        assertEquals(d1.intersects(d2), !d1.intersection(d2).isEmpty());
    }

    @Theory
    public void insertAxiom(Domain d, int v) {
        Domain insert = d.insert(v);
        for (int i : insert.getValues()) {
            assertTrue(d.contains(i) || v == i);
        }
        for (int i : d.getValues()) {
            assertTrue(insert.contains(i));
        }
        assertTrue(insert.contains(v));
    }

    @Theory
    public void insertIdempotentProperty(Domain d, int v) {
        Domain insert = d.insert(v);
        assertSame(insert, insert.insert(v));
    }

    @Theory
    public void removeAxiom(Domain d, int v) {
        Domain remove = d.remove(v);
        for (int i : remove.getValues()) {
            assertTrue(d.contains(i));
        }
        assertFalse(remove.contains(v));
        for (int i : d.getValues()) {
            assertTrue(remove.contains(i) || v == i);
        }
    }

    @Theory
    public void removeIdempotentProperty(Domain d, int v) {
        Domain remove = d.remove(v);
        assertSame(remove, remove.remove(v));
    }

    @Theory
    public void boundLowAxiom(Domain d, int low) {
        Domain boundLow = d.boundLow(low);
        for (int i : boundLow.getValues()) {
            assertTrue(d.contains(i));
        }
        for (int i : d.getValues()) {
            assertEquals(i >= low, boundLow.contains(i));
        }
    }

    @Theory
    public void boundLowIdempotent(Domain d, int low) {
        Domain boundLow = d.boundLow(low);
        assertSame(boundLow, boundLow.boundLow(low));
    }

    @Theory
    public void boundHighAxiom(Domain d, int high) {
        Domain boundHigh = d.boundHigh(high);
        for (int i : boundHigh.getValues()) {
            assertTrue(d.contains(i));
        }
        for (int i : d.getValues()) {
            assertEquals(i <= high, boundHigh.contains(i));
        }
    }

    @Theory
    public void boundHighIdempotent(Domain d, int high) {
        Domain boundHigh = d.boundHigh(high);
        assertSame(boundHigh, boundHigh.boundHigh(high));
    }

    @Theory
    public void boundBetweenAxiom(Domain d, int low, int high) {
        assumeTrue(low <= high);
        Domain boundBetween = d.boundBetween(low, high);
        for (int i : boundBetween.getValues()) {
            assertTrue(d.contains(i));
        }
        for (int i : d.getValues()) {
            assertEquals(d + " : " + low + " : " + high + " : " + boundBetween, low <= i && i <= high, boundBetween.contains(i));
        }
    }

    @Theory
    public void boundBetweenIdempotent(Domain d, int low, int high) {
        assumeTrue(low <= high);
        Domain boundBetween = d.boundBetween(low, high);
        assertSame(boundBetween, boundBetween.boundBetween(low, high));
    }

    @Theory
    public void minusAxiom(Domain d) {
        Domain minus = d.minus();
        assertEquals(d.size(), minus.size());
        for (int i : d.getValues()) {
            assertTrue(minus.contains(-i));
        }
    }

    @Theory
    public void minusInverseMinus(Domain d) {
        assertEquals(d, d.minus().minus());
    }

    @Theory
    public void differenceAxiom(Domain d1, Domain d2) {
        Domain difference = d1.difference(d2);
        for (int i : difference.getValues()) {
            assertTrue(d1.contains(i) && !d2.contains(i));
        }
        for (int i : d1.getValues()) {
            assertEquals(difference.contains(i), !d2.contains(i));
        }
    }

    @Theory
    public void differenceIdempotentProperty(Domain d1, Domain d2) {
        Domain difference = d1.difference(d2);
        assertSame(difference, difference.difference(d2));
    }

    @Theory
    public void intersectionAxiom(Domain d1, Domain d2) {
        Domain intersection = d1.intersection(d2);
        for (int i : intersection.getValues()) {
            assertTrue(d1.contains(i));
            assertTrue(d2.contains(i));
        }
        for (int i : d1.getValues()) {
            assertEquals(d2.contains(i), intersection.contains(i));
        }
        for (int i : d2.getValues()) {
            assertEquals(d1.contains(i), intersection.contains(i));
        }
    }

    @Theory
    public void intersectionCommutativeProperty(Domain d1, Domain d2) {
        assertEquals(d1.intersection(d2), d2.intersection(d1));
    }

    @Theory
    public void intersectionIdempotentProperty(Domain d1, Domain d2) {
        Domain intersection = d1.intersection(d2);
        assertSame(intersection, intersection.intersection(d2));
    }

    @Theory
    public void unionAxiom(Domain d1, Domain d2) {
        Domain union = d1.union(d2);
        for (int i : union.getValues()) {
            assertTrue(d1.contains(i) || d2.contains(i));
        }
        for (int i : d1.getValues()) {
            assertTrue(union.contains(i));
        }
        for (int i : d1.getValues()) {
            assertTrue(union.contains(i));
        }
    }

    @Theory
    public void unionCommutativeProperty(Domain d1, Domain d2) {
        assertEquals(d1.union(d2), d2.union(d1));
    }

    @Theory
    public void unionIdempotentProperty(Domain d1, Domain d2) {
        Domain union = d1.union(d2);
        assertSame(union, union.union(d2));
    }

    @Theory
    public void offsetAxiom(Domain d, int c) {
        Domain offset = d.offset(c);
        assertEquals(d.size(), offset.size());
        for (int i : d.getValues()) {
            assertTrue(offset.contains(i + c));
        }
    }

    @Theory
    public void offsetInverseOffset(Domain d, int c) {
        assertEquals(d, d.offset(c).offset(-c));
    }

    @Theory
    public void isSubsetOfIffIntersectionIsIdentity(Domain d1, Domain d2) {
        assertEquals(d1.isSubsetOf(d2), d1.intersection(d2).equals(d1));
    }
}
