package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.ICause;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.common.Util;

/**
 * Bound arithmetic in {@code long} for the decimal propagators.
 *
 * @author Michał Antkiewicz
 */
class DecimalBounds {

    private DecimalBounds() {
    }

    /**
     * @return {@code a * b}, saturated to the range of {@code long}
     */
    static long mul(long a, long b) {
        long high = Math.multiplyHigh(a, b);
        long low = a * b;
        if ((high == 0 && low >= 0) || (high == -1 && low < 0)) {
            return low;
        }
        return (a < 0) == (b < 0) ? Long.MAX_VALUE : Long.MIN_VALUE;
    }

    static boolean updateBounds(IntVar var, long low, long high, ICause cause) throws ContradictionException {
        if (low > high) {
            throw var.getModel().getSolver().getContradictionException().set(cause, var, "empty bounds");
        }
        return var.updateBounds(
                (int) Math.max(Integer.MIN_VALUE, low),
                (int) Math.min(Integer.MAX_VALUE, high),
                cause);
    }

    /**
     * The smallest {@code p} such that {@code divRound(p, one) >= z}.
     */
    static long lowestRounding(long z, long one) {
        long p = mul(z, one) - one / 2 - 1;
        while (Util.divRound(p, one) < z) {
            p++;
        }
        return p;
    }

    /**
     * The largest {@code p} such that {@code divRound(p, one) <= z}.
     */
    static long highestRounding(long z, long one) {
        long p = mul(z, one) + one / 2 + 1;
        while (Util.divRound(p, one) > z) {
            p--;
        }
        return p;
    }

    /**
     * The bounds of {@code v} such that {@code v * w} is in
     * {@code [low, high]} for some {@code w} in {@code [wLow, wHigh]}, a range
     * without zero. Returns {@code {lowest, highest}}.
     */
    static long[] quotientBounds(long low, long high, long wLow, long wHigh) {
        assert wLow > 0 || wHigh < 0;
        long[] lows = {
            Util.divCeil(low, wLow), Util.divCeil(low, wHigh),
            Util.divCeil(high, wLow), Util.divCeil(high, wHigh)};
        long[] highs = {
            Util.divFloor(low, wLow), Util.divFloor(low, wHigh),
            Util.divFloor(high, wLow), Util.divFloor(high, wHigh)};
        return new long[]{min(lows), max(highs)};
    }

    /**
     * Restricts {@code v} such that {@code v * w} is in {@code [low, high]}.
     *
     * @return whether a domain changed
     */
    static boolean narrowFactor(IntVar v, IntVar w, long low, long high, ICause cause) throws ContradictionException {
        if (low <= 0 && 0 <= high && w.contains(0)) {
            // w = 0 satisfies the product for any v.
            return false;
        }
        boolean changed = false;
        if (w.getLB() == 0) {
            changed |= w.updateLowerBound(1, cause);
        }
        if (w.getUB() == 0) {
            changed |= w.updateUpperBound(-1, cause);
        }
        long vLow = Long.MAX_VALUE;
        long vHigh = Long.MIN_VALUE;
        if (w.getLB() < 0) {
            long[] bounds = quotientBounds(low, high, w.getLB(), Math.min(w.getUB(), -1));
            vLow = Math.min(vLow, bounds[0]);
            vHigh = Math.max(vHigh, bounds[1]);
        }
        if (w.getUB() > 0) {
            long[] bounds = quotientBounds(low, high, Math.max(w.getLB(), 1), w.getUB());
            vLow = Math.min(vLow, bounds[0]);
            vHigh = Math.max(vHigh, bounds[1]);
        }
        return updateBounds(v, vLow, vHigh, cause) | changed;
    }

    static long min(long... values) {
        long min = values[0];
        for (int i = 1; i < values.length; i++) {
            min = Math.min(min, values[i]);
        }
        return min;
    }

    static long max(long... values) {
        long max = values[0];
        for (int i = 1; i < values.length; i++) {
            max = Math.max(max, values[i]);
        }
        return max;
    }
}
