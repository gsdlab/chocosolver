package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.IntVar;
import util.ESat;

/**
 * strings[i] &lt; strings[j] iff ints[i] &lt;= ints[j] strings[i] = strings[j]
 * iff ints[i] = ints[j]
 *
 * 0 &lt; ints[i] &lt; |{ints}|
 *
 * @author jimmy
 */
public class PropLexChainChannel extends Propagator<IntVar> {

    private final IntVar[][] strings;
    private final IntVar[] ints;

    public PropLexChainChannel(IntVar[][] strings, IntVar[] ints) {
        super(buildArray(strings, ints), PropagatorPriority.QUADRATIC, false);
        this.strings = strings;
        this.ints = ints;
    }

    public static IntVar[] buildArray(IntVar[][] strings, IntVar[] ints) {
        if (strings.length != ints.length) {
            throw new IllegalArgumentException();
        }
        IntVar[] array = new IntVar[strings.length * strings[0].length + ints.length];
        System.arraycopy(ints, 0, array, 0, ints.length);
        int i = ints.length;
        for (IntVar[] string : strings) {
            if (string.length != strings[0].length) {
                throw new IllegalArgumentException();
            }
            System.arraycopy(string, 0, array, i, string.length);
            i += string.length;
        }
        return array;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return EventType.INT_ALL_MASK();
    }

    private static CharOrdering compareChar(IntVar a, IntVar b) {
        if (a.instantiated() && b.instantiated() && a.getValue() == b.getValue()) {
            return CharOrdering.EQ;
        }
        if (a.getLB() > b.getUB()) {
            return CharOrdering.GT;
        }
        if (a.getUB() < b.getLB()) {
            return CharOrdering.LT;
        }
        return CharOrdering.UNKNOWN;
    }

    private static CharOrdering compareString(IntVar[] a, IntVar[] b) throws ContradictionException {
        for (int k = 0; k < a.length; k++) {
            CharOrdering ord = compareChar(a[k], b[k]);
            if (!CharOrdering.EQ.equals(ord)) {
                return ord;
            }
        }
        // Only saw Ordering.EQ in the comparisons.
        return CharOrdering.EQ;
    }

    private static Ordering compare(IntVar a, IntVar b) {
        if (a.instantiated() && b.instantiated() && a.getValue() == b.getValue()) {
            return Ordering.EQ;
        }
        if (a.getLB() > b.getUB()) {
            return Ordering.GT;
        }
        if (a.getLB() >= b.getUB()) {
            return Ordering.GE;
        }
        if (a.getUB() < b.getLB()) {
            return Ordering.LT;
        }
        if (a.getUB() <= b.getLB()) {
            return Ordering.LE;
        }
        return Ordering.UNKNOWN;
    }

    private boolean lessThan(IntVar a, IntVar b) throws ContradictionException {
        // Don't use short-circuit or
        return a.updateUpperBound(b.getUB() - 1, aCause)
                | b.updateLowerBound(a.getLB() + 1, aCause);
    }

    private boolean lessThanEqual(IntVar a, IntVar b) throws ContradictionException {
        // Don't use short-circuit or
        return a.updateUpperBound(b.getUB(), aCause)
                | b.updateLowerBound(a.getLB(), aCause);
    }

    private boolean greaterThan(IntVar a, IntVar b) throws ContradictionException {
        // Don't use short-circuit or
        return a.updateLowerBound(b.getLB() + 1, aCause)
                | b.updateUpperBound(a.getUB() - 1, aCause);
    }

    private boolean greaterThanEqual(IntVar a, IntVar b) throws ContradictionException {
        // Don't use short-circuit or
        return a.updateLowerBound(b.getLB(), aCause)
                | b.updateUpperBound(a.getUB(), aCause);
    }

    private boolean equal(IntVar a, IntVar b) throws ContradictionException {
        // Don't use short-circuit or
        return a.updateLowerBound(b.getLB(), aCause)
                | a.updateUpperBound(b.getUB(), aCause)
                | b.updateLowerBound(a.getLB(), aCause)
                | b.updateUpperBound(a.getUB(), aCause);
    }

    private boolean propagateStrings() throws ContradictionException {
        boolean repeat;
        boolean changed = false;
        do {
            repeat = false;
            int eqs = 0;
            for (int i = 0; i < strings.length; i++) {
                boolean equivalenceClass = false;
                for (int j = i + 1; j < strings.length; j++) {
                    switch (compareString(strings[i], strings[j])) {
                        case LT:
                            repeat |= lessThan(ints[i], ints[j]);
                            break;
                        case GT:
                            repeat |= greaterThan(ints[i], ints[j]);
                            break;
                        case EQ:
                            repeat |= equal(ints[i], ints[j]);
                            equivalenceClass = true;
                            break;
                    }
                }
                if (equivalenceClass) {
                    eqs++;
                }
            }
            for (int i = 0; i < ints.length; i++) {
                repeat |= ints[i].updateUpperBound(ints.length - 1 - eqs, aCause);
            }
            changed |= repeat;
        } while (repeat);
        return changed;
    }

    private boolean propagateInts() throws ContradictionException {
        boolean repeat;
        boolean changed = false;
        do {
            repeat = false;
            for (int i = 0; i < ints.length; i++) {
                for (int j = i + 1; j < ints.length; j++) {
                    switch (compare(ints[i], ints[j])) {
                        case LT:
                            for (int x = 0; x < strings[i].length; x++) {
                                CharOrdering ord = compareChar(strings[i][x], strings[j][x]);
                                if (CharOrdering.UNKNOWN.equals(ord)) {
                                    repeat |= lessThanEqual(strings[i][x], strings[j][x]);
                                    break;
                                }
                                if (!CharOrdering.EQ.equals(ord)) {
                                    repeat |= lessThan(strings[i][x], strings[j][x]);
                                    break;
                                }
                            }
                            break;
                        case LE:
                            for (int x = 0; x < strings[i].length; x++) {
                                CharOrdering ord = compareChar(strings[i][x], strings[j][x]);
                                if (!CharOrdering.EQ.equals(ord)) {
                                    repeat |= lessThanEqual(strings[i][x], strings[j][x]);
                                    break;
                                }
                            }
                            break;
                        case GT:
                            for (int x = 0; x < strings[i].length; x++) {
                                CharOrdering ord = compareChar(strings[i][x], strings[j][x]);
                                if (CharOrdering.UNKNOWN.equals(ord)) {
                                    repeat |= greaterThanEqual(strings[i][x], strings[j][x]);
                                    break;
                                }
                                if (!CharOrdering.EQ.equals(ord)) {
                                    repeat |= greaterThan(strings[i][x], strings[j][x]);
                                    break;
                                }
                            }
                            break;
                        case GE:
                            for (int x = 0; x < strings[i].length; x++) {
                                CharOrdering ord = compareChar(strings[i][x], strings[j][x]);
                                if (!CharOrdering.EQ.equals(ord)) {
                                    repeat |= greaterThanEqual(strings[i][x], strings[j][x]);
                                    break;
                                }
                            }
                            break;
                        case EQ:
                            for (int y = 0; y < strings[i].length; y++) {
                                repeat |= equal(strings[i][y], strings[j][y]);
                            }
                            break;
                    }
                }
            }
            changed |= repeat;
        } while (repeat);
        return changed;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        do {
            propagateStrings();
        } while (propagateInts());
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        forcePropagate(EventType.FULL_PROPAGATION);
    }

    @Override
    public ESat isEntailed() {
        for (int i = 0; i < strings.length; i++) {
            for (int j = i + 1; j < strings.length; j++) {
                CharOrdering intOrdering = compareChar(ints[i], ints[j]);
                CharOrdering ord = CharOrdering.EQ;
                for (int k = 0; CharOrdering.EQ.equals(ord) && k < strings[i].length; k++) {
                    ord = compareChar(strings[i][k], strings[j][k]);
                }
                if (!intOrdering.equals(ord)
                        && !CharOrdering.UNKNOWN.equals(intOrdering)
                        && !CharOrdering.UNKNOWN.equals(ord)) {
                    return ESat.FALSE;
                }
            }
        }
        return isCompletelyInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "lexChainChannel(" + Arrays.deepToString(strings) + ", " + Arrays.toString(ints) + ")";
    }

    private static enum Ordering {

        LT,
        LE,
        GT,
        GE,
        EQ,
        UNKNOWN;
    }

    private static enum CharOrdering {

        LT,
        GT,
        EQ,
        UNKNOWN;
    }
}
