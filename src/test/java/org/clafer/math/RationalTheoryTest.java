package org.clafer.math;

import static org.junit.Assert.assertTrue;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(Theories.class)
public class RationalTheoryTest {

    @DataPoints
    public static final Rational[] rationals = new Rational[]{
        new Rational(-1), new Rational(0), new Rational(1),
        new Rational(1, 2), new Rational(3, 2), new Rational(4, 3)
    };

    @Theory
    public void testCeilGreaterEqualFloor(Rational r) {
        assertTrue(r.ceil() >= r.floor());
    }
}
