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
 * Multiplication of decimals scaled by {@code one}:
 * {@code product = divRound(multiplicand * multiplier, one)}, rounded half
 * away from zero. The intermediate product is computed in {@code long}, so it
 * may exceed the range of integer variables. Bound consistent on the product,
 * and the factors are narrowed by the bounds of the product.
 *
 * @author Michał Antkiewicz
 */
public class PropDecimalMul extends Propagator<IntVar> {

    private final IntVar multiplicand, multiplier, product;
    private final long one;

    public PropDecimalMul(IntVar multiplicand, IntVar multiplier, IntVar product, int one) {
        super(new IntVar[]{multiplicand, multiplier, product}, PropagatorPriority.TERNARY, false);
        if (one <= 0) {
            throw new IllegalArgumentException("one must be positive");
        }
        this.multiplicand = multiplicand;
        this.multiplier = multiplier;
        this.product = product;
        this.one = one;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return IntEventType.boundAndInst();
    }

    private long[] exactProductBounds() {
        long ll = mul(multiplicand.getLB(), multiplier.getLB());
        long lh = mul(multiplicand.getLB(), multiplier.getUB());
        long hl = mul(multiplicand.getUB(), multiplier.getLB());
        long hh = mul(multiplicand.getUB(), multiplier.getUB());
        return new long[]{min(ll, lh, hl, hh), max(ll, lh, hl, hh)};
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        boolean changed;
        do {
            long[] exact = exactProductBounds();
            changed = DecimalBounds.updateBounds(product,
                    Util.divRound(exact[0], one), Util.divRound(exact[1], one), this);
            long low = DecimalBounds.lowestRounding(product.getLB(), one);
            long high = DecimalBounds.highestRounding(product.getUB(), one);
            changed |= DecimalBounds.narrowFactor(multiplicand, multiplier, low, high, this);
            changed |= DecimalBounds.narrowFactor(multiplier, multiplicand, low, high, this);
        } while (changed);
    }

    @Override
    public ESat isEntailed() {
        if (isCompletelyInstantiated()) {
            return ESat.eval(product.getValue()
                    == Util.divRound((long) multiplicand.getValue() * multiplier.getValue(), one));
        }
        long[] exact = exactProductBounds();
        if (Util.divRound(exact[1], one) < product.getLB() || Util.divRound(exact[0], one) > product.getUB()) {
            return ESat.FALSE;
        }
        return ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return product + " = round(" + multiplicand + " * " + multiplier + " / " + one + ")";
    }
}
