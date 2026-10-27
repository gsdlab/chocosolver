package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.util.ESat;
import static org.clafer.choco.constraint.propagator.DecimalBounds.max;
import static org.clafer.choco.constraint.propagator.DecimalBounds.min;
import static org.clafer.choco.constraint.propagator.DecimalBounds.mul;
import org.clafer.common.Util;

/**
 * Division of decimals scaled by {@code one}:
 * {@code quotient = divRound(dividend * one, divisor)}, rounded half away from
 * zero, and {@code divisor != 0}. Bound consistent on the quotient; the
 * dividend is narrowed by the bounds of the quotient.
 *
 * @author Michał Antkiewicz
 */
public class PropDecimalDiv extends Propagator<IntVar> {

    private final IntVar dividend, divisor, quotient;
    private final long one;

    public PropDecimalDiv(IntVar dividend, IntVar divisor, IntVar quotient, int one) {
        super(new IntVar[]{dividend, divisor, quotient}, PropagatorPriority.TERNARY, false);
        if (one <= 0) {
            throw new IllegalArgumentException("one must be positive");
        }
        this.dividend = dividend;
        this.divisor = divisor;
        this.quotient = quotient;
        this.one = one;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return IntEventType.boundAndInst();
    }

    /**
     * @return the bounds of the rounded quotient over the part of the divisor
     * in {@code [low, high]}, a range without zero
     */
    private long[] quotientBounds(long low, long high) {
        long dividendLow = mul(dividend.getLB(), one);
        long dividendHigh = mul(dividend.getUB(), one);
        long ll = Util.divRound(dividendLow, low);
        long lh = Util.divRound(dividendLow, high);
        long hl = Util.divRound(dividendHigh, low);
        long hh = Util.divRound(dividendHigh, high);
        return new long[]{min(ll, lh, hl, hh), max(ll, lh, hl, hh)};
    }

    /**
     * @return the bounds of the rounded quotient
     */
    private long[] quotientBounds() {
        long low = Long.MAX_VALUE;
        long high = Long.MIN_VALUE;
        if (divisor.getLB() < 0) {
            long[] bounds = quotientBounds(divisor.getLB(), Math.min(divisor.getUB(), -1));
            low = Math.min(low, bounds[0]);
            high = Math.max(high, bounds[1]);
        }
        if (divisor.getUB() > 0) {
            long[] bounds = quotientBounds(Math.max(divisor.getLB(), 1), divisor.getUB());
            low = Math.min(low, bounds[0]);
            high = Math.max(high, bounds[1]);
        }
        return new long[]{low, high};
    }

    /**
     * {@code divRound(dividend * one, divisor)} is in {@code [zl, zh]} only if
     * {@code dividend * one / divisor} is in {@code [zl - 1/2, zh + 1/2]}, ie.
     * {@code 2 * dividend * one} is between {@code divisor * (2 * zl - 1)} and
     * {@code divisor * (2 * zh + 1)}.
     *
     * @return the bounds of the dividend over the part of the divisor in
     * {@code [low, high]}, a range without zero
     */
    private long[] dividendBounds(long low, long high) {
        long zl = 2L * quotient.getLB() - 1;
        long zh = 2L * quotient.getUB() + 1;
        long ll = mul(low, zl);
        long lh = mul(low, zh);
        long hl = mul(high, zl);
        long hh = mul(high, zh);
        long twoOne = 2 * one;
        return new long[]{
            Util.divCeil(min(ll, lh, hl, hh), twoOne),
            Util.divFloor(max(ll, lh, hl, hh), twoOne)};
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        boolean changed;
        do {
            changed = false;
            if (divisor.getLB() == 0) {
                changed |= divisor.updateLowerBound(1, this);
            }
            if (divisor.getUB() == 0) {
                changed |= divisor.updateUpperBound(-1, this);
            }
            long[] bounds = quotientBounds();
            changed |= DecimalBounds.updateBounds(quotient, bounds[0], bounds[1], this);
            long low = Long.MAX_VALUE;
            long high = Long.MIN_VALUE;
            if (divisor.getLB() < 0) {
                long[] part = dividendBounds(divisor.getLB(), Math.min(divisor.getUB(), -1));
                low = Math.min(low, part[0]);
                high = Math.max(high, part[1]);
            }
            if (divisor.getUB() > 0) {
                long[] part = dividendBounds(Math.max(divisor.getLB(), 1), divisor.getUB());
                low = Math.min(low, part[0]);
                high = Math.max(high, part[1]);
            }
            changed |= DecimalBounds.updateBounds(dividend, low, high, this);
        } while (changed);
    }

    @Override
    public ESat isEntailed() {
        if (divisor.isInstantiatedTo(0)) {
            return ESat.FALSE;
        }
        if (isCompletelyInstantiated()) {
            return ESat.eval(quotient.getValue()
                    == Util.divRound((long) dividend.getValue() * one, divisor.getValue()));
        }
        long[] bounds = quotientBounds();
        if (bounds[1] < quotient.getLB() || bounds[0] > quotient.getUB()) {
            return ESat.FALSE;
        }
        return ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return quotient + " = round(" + dividend + " * " + one + " / " + divisor + ")";
    }
}
