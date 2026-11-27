package org.clafer.math;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 * @author Michał Antkiewicz
 */
public class RationalTest {

    @Test
    public void testArithmetic() {
        Rational half = new Rational(1, 2);
        Rational third = new Rational(1, 3);
        assertEquals(new Rational(5, 6), half.add(third));
        assertEquals(new Rational(1, 6), half.sub(third));
        assertEquals(new Rational(1, 6), half.mul(third));
        assertEquals(new Rational(3, 2), half.div(third));
        assertTrue(third.isLessThan(half));
        assertEquals(half, half.max(third));
    }

    /**
     * Overflowing a long throws instead of silently computing a wrong value, or
     * a zero denominator.
     */
    @Test(expected = ArithmeticException.class)
    public void testAddOverflow() {
        new Rational(Long.MAX_VALUE).add(1);
    }

    @Test(expected = ArithmeticException.class)
    public void testDenominatorOverflow() {
        new Rational(1, 1L << 40).sub(new Rational(1, (1L << 40) + 1));
    }

    @Test(expected = ArithmeticException.class)
    public void testCompareOverflow() {
        new Rational(Long.MAX_VALUE, 3).compareTo(new Rational(Long.MAX_VALUE, 5));
    }
}
