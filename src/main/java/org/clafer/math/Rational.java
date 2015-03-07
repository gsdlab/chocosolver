package org.clafer.math;

import org.clafer.common.Util;

/**
 * A rational number.
 *
 * @author jimmy
 */
public class Rational implements Comparable<Rational> {

    public static Rational Zero = new Rational(0);
    public static Rational One = new Rational(1);

    private final long numerator, denominator;

    public Rational(long whole) {
        this.numerator = whole;
        this.denominator = 1;
    }

    public Rational(long numerator, long denominator) {
        if (denominator == 0) {
            throw new IllegalArgumentException();
        }
        long n, d;
        if (denominator > 0) {
            n = numerator;
            d = denominator;
        } else {
            n = -numerator;
            d = -denominator;
        }
        long gcd = Util.gcd(Math.abs(n), d);
        assert gcd > 0;
        assert n % gcd == 0;
        assert d % gcd == 0;
        this.numerator = n / gcd;
        this.denominator = d / gcd;
    }

    private Rational(long numerator, long denominator, boolean force) {
        this.numerator = numerator;
        this.denominator = denominator;
    }

    public long getNumerator() {
        return numerator;
    }

    public long getDenominator() {
        return denominator;
    }

    public boolean isZero() {
        return numerator == 0;
    }

    public boolean isOne() {
        return numerator == denominator;
    }

    public boolean isWhole() {
        return denominator == 1;
    }

    public boolean isPositive() {
        return numerator > 0;
    }

    public boolean isNegative() {
        return numerator < 0;
    }

    public long ceil() {
        return Util.divCeil(numerator, denominator);
    }

    public long floor() {
        return Util.divFloor(numerator, denominator);
    }

    public Rational minus() {
        return new Rational(-numerator, denominator, true);
    }

    public Rational abs() {
        return isNegative() ? minus() : this;
    }

    public Rational add(long addend) {
        if (addend == 0) {
            return this;
        }
        return new Rational(
                numerator + denominator * addend,
                denominator
        );
    }

    public Rational add(Rational addend) {
        if (addend.isZero()) {
            return this;
        }
        return new Rational(
                numerator * addend.denominator + denominator * addend.numerator,
                denominator * addend.denominator
        );
    }

    public Rational sub(long subtrahend) {
        if (subtrahend == 0) {
            return this;
        }
        return new Rational(
                numerator - denominator * subtrahend,
                denominator
        );
    }

    public Rational sub(Rational subtrahend) {
        if (subtrahend.isZero()) {
            return this;
        }
        return new Rational(
                numerator * subtrahend.denominator - denominator * subtrahend.numerator,
                denominator * subtrahend.denominator
        );
    }

    public Rational mul(long multiplier) {
        if (multiplier == 1) {
            return this;
        }
        return new Rational(
                numerator * multiplier,
                denominator);
    }

    public Rational mul(Rational multiplier) {
        if (multiplier.isOne()) {
            return this;
        }
        return new Rational(
                numerator * multiplier.numerator,
                denominator * multiplier.denominator);
    }

    public Rational div(long divisor) {
        if (divisor == 0) {
            throw new ArithmeticException("/ by zero");
        } else if (divisor == 1) {
            return this;
        }
        return div(new Rational(divisor));
    }

    public Rational div(Rational divisor) {
        if (divisor.isZero()) {
            throw new ArithmeticException("/ by zero");
        } else if (divisor.isOne()) {
            return this;
        }
        return new Rational(
                numerator * divisor.denominator,
                denominator * divisor.numerator);
    }

    public Rational max(Rational m) {
        return numerator * m.denominator > m.numerator * denominator ? this : m;
    }

    public Rational min(Rational m) {
        return numerator * m.denominator < m.numerator * denominator ? this : m;
    }

    @Override
    public int compareTo(Rational o) {
        return Long.compare(numerator * o.denominator, o.numerator * denominator);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Rational) {
            Rational other = (Rational) obj;
            return numerator == other.numerator && denominator == other.denominator;
        }
        return false;
    }

    @Override
    public int hashCode() {
        long hash = numerator * 63 + denominator;
        return (int) (hash ^ (hash >>> 32));
    }

    @Override
    public String toString() {
        if (numerator == 0) {
            return "0";
        }
        if (denominator == 1) {
            return Long.toString(numerator);
        }
        return numerator + "/" + denominator;
    }
}
