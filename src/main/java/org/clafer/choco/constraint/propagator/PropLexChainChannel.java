package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import solver.Solver;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.IntVar;
import solver.variables.VF;
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

    private static Ordering compareString(IntVar[] a, IntVar[] b) {
        return compareString(a, b, 0);
    }

    private static Ordering compareString(IntVar[] a, IntVar[] b, int index) {
        if (index == a.length) {
            return Ordering.EQ;
        }
        Ordering ord = compare(a[index], b[index]);
        switch (ord) {
            case EQ:
                return compareString(a, b, index + 1);
            case LE:
                switch (compareString(a, b, index + 1)) {
                    case LT:
                        return Ordering.LT;
                    case LE:
                    case EQ:
                        return Ordering.LE;
                    default:
                        return Ordering.UNKNOWN;
                }
            case GE:
                switch (compareString(a, b, index + 1)) {
                    case GT:
                        return Ordering.GT;
                    case GE:
                    case EQ:
                        return Ordering.GE;
                    default:
                        return Ordering.UNKNOWN;
                }
            default:
                return ord;
        }
    }

    private static Ordering compare(IntVar a, IntVar b) {
        if (a.instantiated() && b.instantiated() && a.getValue() == b.getValue()) {
            return Ordering.EQ;
        }
        int aLb = a.getLB();
        int aUb = a.getUB();
        int bLb = b.getLB();
        int bUb = b.getUB();
        if (aLb > bUb) {
            return Ordering.GT;
        }
        if (aLb >= bUb) {
            return Ordering.GE;
        }
        if (aUb < bLb) {
            return Ordering.LT;
        }
        if (aUb <= bLb) {
            return Ordering.LE;
        }
        return Ordering.UNKNOWN;
    }

    private boolean lessThanString(IntVar[] a, IntVar[] b) throws ContradictionException {
        boolean changed = false;
        for (int x = 0; x < a.length - 1; x++) {
            switch (compare(a[x], b[x])) {
                case LT:
                case LE:
                case UNKNOWN:
                    return changed;
                case GT:
                case GE:
                    changed |= equal(a[x], b[x]);
                    break;
            }
        }
        // All equal except for the last character.
        return lessThan(a[a.length - 1], b[a.length - 1]) || changed;
    }

    private boolean lessThan(IntVar a, IntVar b) throws ContradictionException {
        // Don't use short-circuit or
        return a.updateUpperBound(b.getUB() - 1, aCause)
                | b.updateLowerBound(a.getLB() + 1, aCause);
    }

    private boolean lessThanEqualString(IntVar[] a, IntVar[] b) throws ContradictionException {
        for (int x = 0; x < a.length; x++) {
            Ordering charOrd = compare(a[x], b[x]);
            if (!Ordering.EQ.equals(charOrd)) {
                return lessThanEqual(a[x], b[x]);
            }
        }
        // The two strings are equal.
        return false;
    }

    private boolean lessThanEqual(IntVar a, IntVar b) throws ContradictionException {
        // Don't use short-circuit or
        return a.updateUpperBound(b.getUB(), aCause)
                | b.updateLowerBound(a.getLB(), aCause);
    }

    private boolean greaterThanString(IntVar[] a, IntVar[] b) throws ContradictionException {
        boolean changed = false;
        for (int x = 0; x < a.length - 1; x++) {
            switch (compare(a[x], b[x])) {
                case GT:
                case GE:
                case UNKNOWN:
                    return changed;
                case LT:
                case LE:
                    changed |= equal(a[x], b[x]);
                    break;
            }
        }
        // All equal except for the last character.
        return greaterThan(a[a.length - 1], b[a.length - 1]) || changed;
    }

    private boolean greaterThan(IntVar a, IntVar b) throws ContradictionException {
        // Don't use short-circuit or
        return a.updateLowerBound(b.getLB() + 1, aCause)
                | b.updateUpperBound(a.getUB() - 1, aCause);
    }

    private boolean greaterThanEqualString(IntVar[] a, IntVar[] b) throws ContradictionException {
        for (int x = 0; x < a.length; x++) {
            Ordering charOrd = compare(a[x], b[x]);
            if (!Ordering.EQ.equals(charOrd)) {
                return greaterThanEqual(a[x], b[x]);
            }
        }
        // The two strings are equal.
        return false;
    }

    private boolean greaterThanEqual(IntVar a, IntVar b) throws ContradictionException {
        // Don't use short-circuit or
        return a.updateLowerBound(b.getLB(), aCause)
                | b.updateUpperBound(a.getUB(), aCause);
    }

    private boolean equalString(IntVar[] a, IntVar[] b) throws ContradictionException {
        boolean changed = false;
        for (int i = 0; i < a.length; i++) {
            changed |= equal(a[i], b[i]);
        }
        return changed;
    }

    private boolean equal(IntVar a, IntVar b) throws ContradictionException {
        // Don't use short-circuit or
        return a.updateLowerBound(b.getLB(), aCause)
                | a.updateUpperBound(b.getUB(), aCause)
                | b.updateLowerBound(a.getLB(), aCause)
                | b.updateUpperBound(a.getUB(), aCause);
    }

    // Idempotent.
    private boolean propagateStrings() throws ContradictionException {
        Update operations = new Update(strings.length);
        int eqs = 0;
        for (int i = 0; i < strings.length; i++) {
            boolean equivalenceClass = false;
            for (int j = i + 1; j < strings.length; j++) {
                Ordering ord = compareString(strings[i], strings[j]);
                switch (ord) {
                    case EQ:
                        equivalenceClass = true;
                    // fallthrough
                    case LT:
                    case LE:
                    case GT:
                    case GE:
                        operations.add(i, j, ord);
                        break;
                }
            }
            if (equivalenceClass) {
                eqs++;
            }
        }
        boolean changed = false;
        for (int i = 0; i < ints.length; i++) {
            changed |= ints[i].updateUpperBound(ints.length - 1 - eqs, aCause);
        }
        boolean repeat;
        do {
            repeat = operations.doIntUpdates();
            changed |= repeat;
        } while (repeat);
        return changed;
    }

    // Idempotent.
    private boolean propagateInts() throws ContradictionException {
        Update operations = new Update(strings.length);
        for (int i = 0; i < ints.length; i++) {
            for (int j = i + 1; j < ints.length; j++) {
                Ordering ord = compare(ints[i], ints[j]);
                if (!Ordering.UNKNOWN.equals(ord)) {
                    operations.add(i, j, ord);
                }
            }
        }
        boolean changed = false;
        boolean repeat;
        do {
            repeat = operations.doStringUpdates();
            changed |= repeat;
        } while (repeat);
        return changed;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        propagateStrings();
        while (propagateInts() && propagateStrings());
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        forcePropagate(EventType.FULL_PROPAGATION);
    }

    @Override
    public ESat isEntailed() {
        for (int i = 0; i < strings.length; i++) {
            for (int j = i + 1; j < strings.length; j++) {
                Ordering intOrdering = compare(ints[i], ints[j]);
                Ordering ord = Ordering.EQ;
                for (int k = 0; Ordering.EQ.equals(ord) && k < strings[i].length; k++) {
                    ord = compare(strings[i][k], strings[j][k]);
                }
                if (!intOrdering.equals(ord)
                        && !Ordering.UNKNOWN.equals(intOrdering)
                        && !Ordering.UNKNOWN.equals(ord)) {
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

    private class Update {

        private int[] updates;
        private int size = 0;

        Update(int capacity) {
            updates = new int[capacity * 3];
        }

        void add(int x, int y, Ordering ord) {
            int offset = size * 3;
            if (offset >= updates.length) {
                updates = Arrays.copyOf(updates, updates.length * 2);
            }
            updates[offset] = x;
            updates[offset + 1] = y;
            updates[offset + 2] = ord.ordinal();
            size++;
        }

        boolean doStringUpdates() throws ContradictionException {
            boolean changed = false;
            int cap = size * 3;
            for (int i = 0; i < cap; i += 3) {
                int x = updates[i];
                int y = updates[i + 1];
                switch (updates[i + 2]) {
                    case 0: // LT
                        changed |= lessThanString(strings[x], strings[y]);
                        break;
                    case 1: // LE
                        changed |= lessThanEqualString(strings[x], strings[y]);
                        break;
                    case 2: // GT
                        changed |= greaterThanString(strings[x], strings[y]);
                        break;
                    case 3: // GE
                        changed |= greaterThanEqualString(strings[x], strings[y]);
                        break;
                    case 4: // EQ
                        changed |= equalString(strings[x], strings[y]);
                        break;
                    default:
                        throw new IllegalStateException();
                }
            }
            return changed;
        }

        boolean doIntUpdates() throws ContradictionException {
            boolean changed = false;
            int cap = size * 3;
            for (int i = 0; i < cap; i += 3) {
                int x = updates[i];
                int y = updates[i + 1];
                switch (updates[i + 2]) {
                    case 0: // LT
                        changed |= lessThan(ints[x], ints[y]);
                        break;
                    case 1: // LE
                        changed |= lessThanEqual(ints[x], ints[y]);
                        break;
                    case 2: // GT
                        changed |= greaterThan(ints[x], ints[y]);
                        break;
                    case 3: // GE
                        changed |= greaterThanEqual(ints[x], ints[y]);
                        break;
                    case 4: // EQ
                        changed |= equal(ints[x], ints[y]);
                        break;
                    default:
                        throw new IllegalStateException();
                }
            }
            return changed;
        }
    }
}
