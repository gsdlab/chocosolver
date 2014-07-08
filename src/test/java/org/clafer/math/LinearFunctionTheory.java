package org.clafer.math;

import static org.junit.Assert.*;
import static org.junit.Assume.assumeFalse;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(Theories.class)
public class LinearFunctionTheory {

    @DataPoints
    public static final LinearFunction[] functions = new LinearFunction[]{
        new LinearFunction(0),
        new LinearFunction(1),
        new LinearFunction(-1),
        new LinearFunction(new long[]{1}, new Variable[]{new Variable("a", -1, 1)}, 0),
        new LinearFunction(new long[]{-1}, new Variable[]{new Variable("a", -1, 1)}, 0),
        new LinearFunction(new long[]{1, -1}, new Variable[]{new Variable("a", -1, 1), new Variable("b", -2, 2)}, 1)
    };

    @DataPoints
    public static final Rational[] rationals = new Rational[]{
        Rational.Zero,
        Rational.One,
        new Rational(-1, 1),
        new Rational(5, 2),
        new Rational(-3, 2),
        new Rational(-4, 3),
        new Rational(5, 1),
        new Rational(2, 3),
        new Rational(2, 5),
        new Rational(-1, 3)
    };

    @DataPoints
    public static final long[] longs = new long[]{-15, -14, -2, -1, 0, 1, 2, 14, 15};

    @Theory
    public void addIdentity(LinearFunction f) {
        assertEquals(f, f.add(new LinearFunction(Rational.Zero)));
        assertEquals(f, f.add(Rational.Zero));
        assertEquals(f, f.add(0));
    }

    @Theory
    public void addAssociative(LinearFunction f1, LinearFunction f2, LinearFunction f3) {
        assertEquals(f1.add(f2).add(f3), f1.add(f2.add(f3)));
    }

    @Theory
    public void addCommutative(LinearFunction f1, LinearFunction f2) {
        assertEquals(f1.add(f2), f2.add(f1));
    }

    @Theory
    public void subIdentity(LinearFunction f) {
        assertEquals(f, f.sub(new LinearFunction(Rational.Zero)));
        assertEquals(f, f.sub(Rational.Zero));
        assertEquals(f, f.sub(0));
    }

    @Theory
    public void addInverseSub(LinearFunction f1, LinearFunction f2) {
        assertEquals(f1, f1.add(f2).sub(f2));
        assertEquals(f1, f1.sub(f2).add(f2));
    }

    @Theory
    public void addInverseSubRational(LinearFunction f, Rational r) {
        assertEquals(f, f.add(r).sub(r));
        assertEquals(f, f.sub(r).add(r));
    }

    @Theory
    public void addInverseSubLong(LinearFunction f, long l) {
        assertEquals(f, f.add(l).sub(l));
        assertEquals(f, f.sub(l).add(l));
    }

    @Theory
    public void mulIdentity(LinearFunction f) {
        assertEquals(f, f.mul(Rational.One));
        assertEquals(f, f.mul(1));
    }

    @Theory
    public void divIdentity(LinearFunction f) {
        assertEquals(f, f.div(Rational.One));
        assertEquals(f, f.div(1));
    }

    @Theory
    public void mulInverseDivRational(LinearFunction f, Rational r) {
        assumeFalse(r.isZero());
        assertEquals(f, f.mul(r).div(r));
        assertEquals(f, f.div(r).mul(r));
    }

    @Theory
    public void mulInverseDivLong(LinearFunction f, long l) {
        assumeFalse(l == 0);
        assertEquals(f, f.mul(l).div(l));
        assertEquals(f, f.div(l).mul(l));
    }

    @Theory
    public void lowHighBound(LinearFunction f) {
        assertTrue(f.getLowBound().compareTo(f.getHighBound()) <= 0);
    }
}
