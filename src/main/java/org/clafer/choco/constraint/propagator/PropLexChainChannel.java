package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
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
        super(buildArray(strings, ints), PropagatorPriority.CUBIC, false);
        this.strings = strings;
        this.ints = ints;
    }

    private static IntVar[] buildArray(IntVar[][] strings, IntVar[] ints) {
        if (strings.length != ints.length) {
            throw new IllegalArgumentException();
        }
        IntVar[] array = new IntVar[strings.length * strings[0].length + ints.length];
        System.arraycopy(ints, 0, array, 0, ints.length);
        int i = ints.length;
        for (IntVar[] string : strings) {
            System.arraycopy(string, 0, array, i, string.length);
            i += string.length;
        }
        assert i == array.length;
        return array;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return EventType.BOUND.mask + EventType.INSTANTIATE.mask;
    }

    private static Ordering compareString(IntVar[] a, IntVar[] b) {
        return compareString(a, b, 0);
    }

    private static Ordering compareString(IntVar[] a, IntVar[] b, int index) {
        if (index == a.length) {
            return a.length == b.length ? Ordering.EQ : Ordering.LT;
        }
        if (index == b.length) {
            assert a.length != b.length;
            return Ordering.GT;
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

    private boolean lessThanString(IntVar[] a, IntVar[] b) throws ContradictionException {
        return lessThanString(a, b, 0);
    }

    private boolean lessThanString(IntVar[] a, IntVar[] b, int index) throws ContradictionException {
        if (index == a.length) {
            if (index == b.length) {
                contradiction(a[0], "a = b");
            }
            return false;
        }
        if (index == b.length) {
            assert a.length != b.length;
            contradiction(a[index], "a > b");
        }
        switch (compare(a[index], b[index])) {
            case EQ:
                return lessThanString(a, b, index + 1);
            case LT:
                return false;
            case GT:
                contradiction(a[index], "a > b");
                return true;
            case LE:
            case GE:
            case UNKNOWN:
                switch (compareString(a, b, index + 1)) {
                    case EQ:
                    case GT:
                    case GE:
                        return lessThan(a[index], b[index]);
                    case LT:
                    case LE:
                    case UNKNOWN:
                        return lessThanEqual(a[index], b[index]);
                    default:
                        throw new IllegalStateException();
                }
            default:
                throw new IllegalStateException();
        }
    }

    private boolean lessThan(IntVar a, IntVar b) throws ContradictionException {
        // Don't use short-circuit or
        return a.updateUpperBound(b.getUB() - 1, aCause)
                | b.updateLowerBound(a.getLB() + 1, aCause);
    }

    private boolean lessThanEqualString(IntVar[] a, IntVar[] b) throws ContradictionException {
        return lessThanEqualString(a, b, 0);
    }

    private boolean lessThanEqualString(IntVar[] a, IntVar[] b, int index) throws ContradictionException {
        if (index == a.length) {
            return false;
        }
        if (index == b.length) {
            assert a.length != b.length;
            contradiction(a[index], "a > b");
        }
        switch (compare(a[index], b[index])) {
            case EQ:
                return lessThanEqualString(a, b, index + 1);
            case LT:
                return false;
            case GT:
                contradiction(a[index], "a > b");
                return true;
            case LE:
            case GE:
            case UNKNOWN:
                switch (compareString(a, b, index + 1)) {
                    case EQ:
                    case LT:
                    case LE:
                    case GE:
                    case UNKNOWN:
                        return lessThanEqual(a[index], b[index]);
                    case GT:
                        return lessThan(a[index], b[index]);
                    default:
                        throw new IllegalStateException();
                }
            default:
                throw new IllegalStateException();
        }
    }

    private boolean lessThanEqual(IntVar a, IntVar b) throws ContradictionException {
        // Don't use short-circuit or
        return a.updateUpperBound(b.getUB(), aCause)
                | b.updateLowerBound(a.getLB(), aCause);
    }

    private boolean propagateSmallest(boolean[] notSmallest, boolean notSmaller[], boolean[] lessThanEqual, int smallest) throws ContradictionException {
        boolean changed = false;
        boolean stop = true;
        boolean lessThanEqualStop = false;
        boolean[] notNextSmallest = new boolean[strings.length];
        for (int i = 0; i < notSmallest.length; i++) {
            if (!notSmallest[i]) {
                notSmaller[i] = false;
                stop = false;
                lessThanEqualStop |= lessThanEqual[i];
                changed |= ints[i].instantiateTo(smallest, aCause);
            }
            notNextSmallest[i] = !notSmaller[i];
        }
        if (stop || lessThanEqualStop) {
            return changed;
        }
        for (int i = 0; i < strings.length; i++) {
            if (notSmaller[i]) {
                for (int j = i + 1; j < strings.length; j++) {
                    if (notSmaller[j]) {
                        Ordering ord = compareString(strings[i], strings[j]);
                        switch (ord) {
                            case LE:
                                lessThanEqual[i] = true;
                            // fallthrough
                            case LT:
                                notNextSmallest[j] = true;
                                break;
                            case GE:
                                lessThanEqual[j] = true;
                            // fallthrough
                            case GT:
                                notNextSmallest[i] = true;
                                break;
                            case UNKNOWN:
                                notNextSmallest[i] = true;
                                notNextSmallest[j] = true;
                                break;
                        }
                    }
                }
            }
        }
        changed |= propagateSmallest(notNextSmallest, notSmaller, lessThanEqual, smallest + 1);
        return changed;
    }

    // Idempotent.
    private boolean propagateStrings() throws ContradictionException {
        Update operations = new Update(strings.length);
        int eqs = 0;
        boolean[] notSmallest = new boolean[strings.length];
        boolean[] lessThanEqual = new boolean[strings.length];
        for (int i = 0; i < strings.length; i++) {
            boolean equivalenceClass = false;
            for (int j = i + 1; j < strings.length; j++) {
                Ordering ord = compareString(strings[i], strings[j]);
                switch (ord) {
                    case EQ:
                        equivalenceClass = true;
                        operations.add(i, j, ord);
                        break;
                    case LE:
                        lessThanEqual[i] = true;
                    // fallthrough
                    case LT:
                        notSmallest[j] = true;
                        operations.add(i, j, ord);
                        break;
                    case GE:
                        lessThanEqual[j] = true;
                    // fallthrough
                    case GT:
                        notSmallest[i] = true;
                        operations.add(i, j, ord);
                        break;
                    case UNKNOWN:
                        notSmallest[i] = true;
                        notSmallest[j] = true;
                        break;
                }
            }
            if (equivalenceClass) {
                eqs++;
            }
        }
        boolean changed = propagateSmallest(notSmallest, notSmallest, lessThanEqual, 0);
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
        boolean[] values = new boolean[ints.length];
        for (int i = 0; i < strings.length; i++) {
            for (int j = i + 1; j < strings.length; j++) {
                Ordering intOrdering = compare(ints[i], ints[j]);
                Ordering ord = compareString(strings[i], strings[j]);
                if (intOrdering.contradicts(ord)) {
                    return ESat.FALSE;
                }
            }
            if (ints[i].instantiated()) {
                int value = ints[i].getValue();
                if (value < 0 || value >= values.length) {
                    return ESat.FALSE;
                }
                values[value] = true;
            }
        }
        if (isCompletelyInstantiated()) {
            int i = 0;
            while (i < values.length && values[i]) {
                i++;
            }
            for (; i < values.length; i++) {
                if (values[i]) {
                    return ESat.FALSE;
                }
            }
            return ESat.TRUE;
        }
        return ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "lexChainChannel(" + Arrays.deepToString(strings) + ", " + Arrays.toString(ints) + ")";
    }

    private static enum Ordering {

        EQ,
        LT,
        LE,
        GT,
        GE,
        UNKNOWN;

        boolean contradicts(Ordering ord) {
            switch (this) {
                case EQ:
                    return LT.equals(ord) || GT.equals(ord);
                case LT:
                    return EQ.equals(ord) || GE.equals(ord) || GT.equals(ord);
                case LE:
                    return GT.equals(ord);
                case GT:
                    return EQ.equals(ord) || LE.equals(ord) || LT.equals(ord);
                case GE:
                    return LT.equals(ord);
                case UNKNOWN:
                    return false;
                default:
                    throw new IllegalStateException();
            }
        }
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
                    case 0: // EQ
                        changed |= equalString(strings[x], strings[y]);
                        break;
                    case 1: // LT
                        changed |= lessThanString(strings[x], strings[y]);
                        break;
                    case 2: // LE
                        changed |= lessThanEqualString(strings[x], strings[y]);
                        break;
                    case 3: // GT
                        changed |= lessThanString(strings[y], strings[x]);
                        break;
                    case 4: // GE
                        changed |= lessThanEqualString(strings[y], strings[x]);
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
                    case 0: // EQ
                        changed |= equal(ints[x], ints[y]);
                        break;
                    case 1: // LT
                        changed |= lessThan(ints[x], ints[y]);
                        break;
                    case 2: // LE
                        changed |= lessThanEqual(ints[x], ints[y]);
                        break;
                    case 3: // GT
                        changed |= lessThan(ints[y], ints[x]);
                        break;
                    case 4: // GE
                        changed |= lessThanEqual(ints[y], ints[x]);
                        break;
                    default:
                        throw new IllegalStateException();
                }
            }
            return changed;
        }
    }
}
