package org.clafer.choco.constraint.propagator;

import gnu.trove.set.TIntSet;
import org.chocosolver.solver.ICause;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.IIntDeltaMonitor;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.util.objects.setDataStructures.ISet;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;

/**
 * Various static utility functions for writing Choco propagators.
 *
 * @author jimmy
 */
public class PropUtil {

    private PropUtil() {
    }

    /**
     * Monitor the deltas for all the variables.
     *
     * @param vars the variables
     * @param propagator the propagator
     * @return the variables delta monitors
     */
    public static IIntDeltaMonitor[] monitorDeltas(IntVar[] vars, ICause propagator) {
        IIntDeltaMonitor[] deltas = new IIntDeltaMonitor[vars.length];
        for (int i = 0; i < vars.length; i++) {
            deltas[i] = vars[i].monitorDelta(propagator);
        }
        return deltas;
    }

    /**
     * Monitor the deltas for all the variables.
     *
     * @param vars the variables
     * @param propagator the propagator
     * @return the variables delta monitors
     */
    public static ISetDeltaMonitor[] monitorDeltas(SetVar[] vars, ICause propagator) {
        ISetDeltaMonitor[] deltas = new ISetDeltaMonitor[vars.length];
        for (int i = 0; i < vars.length; i++) {
            deltas[i] = vars[i].monitorDelta(propagator);
        }
        return deltas;
    }

    public static boolean equals(ISet a, ISet b) {
        if (a.size() != b.size()) {
            return false;
        }
        ISetIterator aIter = a.iterator();
        ISetIterator bIter = b.iterator();
        while (aIter.hasNext()) {
            assert bIter.hasNext();
            if (aIter.nextInt() != bIter.nextInt()) {
                return false;
            }
        }
        assert !bIter.hasNext();
        return true;
    }

    /**
     * Enumerate the domain of a integer variable.
     *
     * @param ivar the integer variable
     * @return {@code dom(int)}
     */
    public static int[] iterateDom(IntVar ivar) {
        int[] iterate = new int[ivar.getDomainSize()];
        int count = 0;
        int ub = ivar.getUB();
        for (int i = ivar.getLB(); i <= ub; i = ivar.nextValue(i)) {
            iterate[count++] = i;
        }
        assert count == iterate.length;
        return iterate;
    }

    /**
     * Enumerate the envelope of a set variable.
     *
     * @param set the set variable
     * @return {@code env(set)}
     */
    public static int[] iterateEnv(SetVar set) {
        int[] iterate = new int[set.getUB().size()];
        int count = 0;
        ISetIterator iter = set.getUB().iterator();
        while (iter.hasNext()) {
            iterate[count++] = iter.nextInt();
        }
        assert count == iterate.length;
        return iterate;
    }

    /**
     * Enumerate the kernel of a set variable.
     *
     * @param set the set variable
     * @return {@code ker(set)}
     */
    public static int[] iterateKer(SetVar set) {
        int[] iterate = new int[set.getLB().size()];
        int count = 0;
        ISetIterator iter = set.getLB().iterator();
        while (iter.hasNext()) {
            iterate[count++] = iter.nextInt();
        }
        assert count == iterate.length;
        return iterate;
    }

    public static int getEnv(SetVar set, int index) {
        ISetIterator iter = set.getUB().iterator();
        for (int i = 0; i < index && iter.hasNext(); i++) {
            iter.nextInt();
        }
        if (iter.hasNext()) {
            return iter.nextInt();
        }
        throw new IllegalArgumentException("Cannot retrieve element " + index + " in " + set);
    }

    /**
     * Checks if at least one of the integer's domain contains a value.
     *
     * @param union the integers
     * @param value the value
     * @return {@code true} if {@code value ∈ dom(union[i]) for some i},
     * {@code false} otherwise
     */
    public static boolean domsContain(IntVar[] union, int value) {
        for (IntVar var : union) {
            if (var.contains(value)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if at least one of the set's envelope contains a value.
     *
     * @param union the sets
     * @param value the value
     * @return {@code true} if {@code value ∈ env(union[i]) for some i},
     * {@code false} otherwise
     */
    public static boolean envsContain(SetVar[] union, int value) {
        for (SetVar var : union) {
            if (var.getUB().contains(value)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if at least one of the set's kernel contains a value.
     *
     * @param union the sets
     * @param value the value
     * @return {@code true} if {@code value ∈ kernel(union[i]) for some i},
     * {@code false} otherwise
     */
    public static boolean kersContain(SetVar[] union, int value) {
        for (SetVar var : union) {
            if (var.getLB().contains(value)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if an integer's domain is contained entirely in the other
     * integer's domain.
     *
     * @param i1 the first operand
     * @param i2 the second operand
     * @return {@code true} if {@code dom(i1) ⋂ dom(i2) ≠ {}}, {@code false}
     * otherwise
     */
    public static boolean isDomIntersectDom(IntVar i1, IntVar i2) {
        return getDomIntersectDom(i1, i2) != Integer.MAX_VALUE;
    }

    public static int getDomIntersectDom(IntVar i1, IntVar i2) {
        int v1 = i1.getLB();
        int v2 = i2.getLB();
        boolean smaller = v1 < v2;
        do {
            if (v1 == v2) {
                return v1;
            }
            if (smaller) {
                v1 = i1.nextValue(v2 - 1);
                smaller = false;
            } else {
                v2 = i2.nextValue(v1 - 1);
                smaller = true;
            }
        } while (v1 != Integer.MAX_VALUE && v2 != Integer.MAX_VALUE);
        return Integer.MAX_VALUE;
    }

    public static int intersectionSize(ISet s1, ISet s2) {
        ISetIterator iter = s1.iterator();
        int count = 0;
        while (iter.hasNext()) {
            if (s2.contains(iter.nextInt())) {
                count++;
            }
        }
        return count;
    }

    /**
     * Checks if an integer's domain is contained entirely in the set's
     * envelope.
     *
     * @param i1 the first operand
     * @param i2 the second operand
     * @return {@code true} if {@code dom(i1) ⋂ env(i2) ≠ {}}, {@code false}
     * otherwise
     */
    public static boolean isDomIntersectEnv(IntVar i1, SetVar i2) {
        if (i1.getDomainSize() < i2.getUB().size()) {
            int i = i1.getLB();
            int envFirst = i2.getUB().min();
            if (envFirst >= i) {
                if (envFirst == i || i1.contains(envFirst)) {
                    return true;
                }
                i = i1.nextValue(envFirst);
            }
            int ub = i1.getUB();
            for (; i <= ub; i = i1.nextValue(i)) {
                if (i2.getUB().contains(i)) {
                    return true;
                }
            }
        } else {
            ISetIterator iter = i2.getUB().iterator();
            if (iter.hasNext()) {
                int i = iter.nextInt();
                while (true) {
                    int next = i1.nextValue(i - 1);
                    while (i < next) {
                        if (!iter.hasNext()) {
                            return false;
                        }
                        i = iter.nextInt();
                    }
                    if (i == next) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * Checks if an integer's domain is contained entirely in the set's kernel.
     *
     * @param i1 the first operand
     * @param i2 the second operand
     * @return {@code true} if {@code dom(i1) ⋂ ker(i2) ≠ {}}, {@code false}
     * otherwise
     */
    public static boolean isDomIntersectKer(IntVar i1, SetVar i2) {
        if (i1.getDomainSize() < i2.getLB().size()) {
            int i = i1.getLB();
            int kerFirst = i2.getLB().min();
            if (kerFirst >= i) {
                if (kerFirst == i || i1.contains(kerFirst)) {
                    return true;
                }
                i = i1.nextValue(kerFirst);
            }
            int ub = i1.getUB();
            for (; i <= ub; i = i1.nextValue(i)) {
                if (i2.getLB().contains(i)) {
                    return true;
                }
            }
        } else {
            ISetIterator iter = i2.getLB().iterator();
            if (iter.hasNext()) {
                int i = iter.nextInt();
                while (true) {
                    int next = i1.nextValue(i - 1);
                    while (i < next) {
                        if (!iter.hasNext()) {
                            return false;
                        }
                        i = iter.nextInt();
                    }
                    if (i == next) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * Checks if a set's envelope is contained entirely in the other set's
     * envelope.
     *
     * @param i1 the first operand
     * @param i2 the second operand
     * @return {@code true} if {@code env(i1) ⋂ env(i2) ≠ {}}, {@code false}
     * otherwise
     */
    public static boolean isEnvIntersectEnv(SetVar i1, SetVar i2) {
        SetVar small = i1;
        SetVar large = i2;
        if (i1.getUB().size() > i2.getUB().size()) {
            small = i2;
            large = i1;
        }
        ISetIterator iter = small.getUB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (large.getUB().contains(i)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if a set's envelope is contained entirely in the set's kernel.
     *
     * @param i1 the first operand
     * @param i2 the second operand
     * @return {@code true} if {@code env(i1) ⋂ ker(i2) ≠ {}}, {@code false}
     * otherwise
     */
    public static boolean isEnvIntersectKer(SetVar i1, SetVar i2) {
        if (i1.getUB().size() < i2.getUB().size()) {
            ISetIterator iter = i1.getUB().iterator();
            while (iter.hasNext()) {
                int i = iter.nextInt();
                if (i2.getLB().contains(i)) {
                    return true;
                }
            }
        } else {
            ISetIterator iter = i2.getLB().iterator();
            while (iter.hasNext()) {
                int i = iter.nextInt();
                if (i1.getUB().contains(i)) {
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Checks if a set's kernel is contained entirely in the other set's kernel.
     *
     * @param i1 the first operand
     * @param i2 the second operand
     * @return {@code true} if {@code ker(i1) ⋂ ker(i2) ≠ {}}, {@code false}
     * otherwise
     */
    public static boolean isKerIntersectKer(SetVar i1, SetVar i2) {
        SetVar small = i1;
        SetVar large = i2;
        if (i1.getLB().size() > i2.getLB().size()) {
            small = i2;
            large = i1;
        }
        ISetIterator iter = small.getLB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (large.getLB().contains(i)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks if an integer's domain is contained entirely in the other
     * integer's domain.
     *
     * @param sub the subset
     * @param sup the superset
     * @return {@code true} if {@code dom(sub) ⊆ dom(sup)}, {@code false}
     * otherwise
     */
    public static boolean isDomSubsetDom(IntVar sub, IntVar sup) {
        if (sub.getDomainSize() > sup.getDomainSize()) {
            return false;
        }
        int ub = sub.getUB();
        for (int i = sub.getLB(); i <= ub; i = sub.nextValue(i)) {
            if (!sup.contains(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks if an integer's domain is contained entirely in the set's
     * envelope.
     *
     * @param sub the subset
     * @param sup the superset
     * @return {@code true} if {@code dom(sub) ⊆ env(sup)}, {@code false}
     * otherwise
     */
    public static boolean isDomSubsetEnv(IntVar sub, SetVar sup) {
        if (sub.getDomainSize() > sup.getUB().size()) {
            return false;
        }
        int ub = sub.getUB();
        for (int i = sub.getLB(); i <= ub; i = sub.nextValue(i)) {
            if (!sup.getUB().contains(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks if an integer's domain is contained entirely in the set's kernel.
     *
     * @param sub the subset
     * @param sup the superset
     * @return {@code true} if {@code dom(sub) ⊆ ker(sup)}, {@code false}
     * otherwise
     */
    public static boolean isDomSubsetKer(IntVar sub, SetVar sup) {
        if (sub.getDomainSize() > sup.getLB().size()) {
            return false;
        }
        int ub = sub.getUB();
        for (int i = sub.getLB(); i <= ub; i = sub.nextValue(i)) {
            if (!sup.getLB().contains(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks if a set's envelope is contained entirely in the integer's domain.
     *
     * @param sub the subset
     * @param sup the superset
     * @return {@code true} if {@code env(sub) ⊆ dom(sup)}, {@code false}
     * otherwise
     */
    public static boolean isEnvSubsetDom(SetVar sub, IntVar sup) {
        if (sub.getUB().size() > sup.getDomainSize()) {
            return false;
        }
        ISetIterator iter = sub.getUB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (!sup.contains(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks if a set's envelope is contained entirely in the other set's
     * envelope.
     *
     * @param sub the subset
     * @param sup the superset
     * @return {@code true} if {@code env(sub) ⊆ env(sup)}, {@code false}
     * otherwise
     */
    public static boolean isEnvSubsetEnv(SetVar sub, SetVar sup) {
        if (sub.getUB().size() > sup.getUB().size()) {
            return false;
        }
        ISetIterator iter = sub.getUB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (!sup.getUB().contains(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks if a set's envelope is contained entirely in the set's kernel.
     *
     * @param sub the subset
     * @param sup the superset
     * @return {@code true} if {@code env(sub) ⊆ ker(sup)}, {@code false}
     * otherwise
     */
    public static boolean isEnvSubsetKer(SetVar sub, SetVar sup) {
        if (sub.getUB().size() > sup.getLB().size()) {
            return false;
        }
        ISetIterator iter = sub.getUB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (!sup.getLB().contains(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks if a set's kernel is contained entirely in the integer's domain.
     *
     * @param sub the subset
     * @param sup the superset
     * @return {@code true} if {@code ker(sub) ⊆ dom(sup)}, {@code false}
     * otherwise
     */
    public static boolean isKerSubsetDom(SetVar sub, IntVar sup) {
        ISetIterator iter = sub.getLB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (!sup.contains(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks if a set's kernel is contained entirely in the set's envelope.
     *
     * @param sub the subset
     * @param sup the superset
     * @return {@code true} if {@code ker(sub) ⊆ env(sup)}, {@code false}
     * otherwise
     */
    public static boolean isKerSubsetEnv(SetVar sub, SetVar sup) {
        ISetIterator iter = sub.getLB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (!sup.getUB().contains(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Checks if a set's kernel is contained entirely in the other set's kernel.
     *
     * @param sub the subset
     * @param sup the superset
     * @return {@code true} if {@code ker(sub) ⊆ ker(sup)}, {@code false}
     * otherwise
     */
    public static boolean isKerSubsetKer(SetVar sub, SetVar sup) {
        ISetIterator iter = sub.getLB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (!sup.getLB().contains(i)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Removes every element in an integer's domain that is not in the set.
     *
     * @param sub the subset
     * @param sup the superset
     * @param propagator the propagator
     * @throws ContradictionException
     * @return {@code true} if a variable has been changed, {@code false}
     * otherwise
     */
    public static boolean domSubsetSet(IntVar sub, TIntSet sup, ICause propagator) throws ContradictionException {
        boolean changed = false;
        int left = Integer.MIN_VALUE;
        int right = left;
        int ub = sub.getUB();
        for (int val = sub.getLB(); val <= ub; val = sub.nextValue(val)) {
            if (!sup.contains(val)) {
                if (val == right + 1) {
                    right = val;
                } else {
                    changed |= sub.removeInterval(left, right, propagator);
                    left = val;
                    right = val;
                }
            }
        }
        changed |= sub.removeInterval(left, right, propagator);
        return changed;
    }

    /**
     * Removes every element in an integer's domain that is not in the other
     * integer's domain.
     *
     * @param sub the subset
     * @param sup the superset
     * @param propagator the propagator
     * @throws ContradictionException
     * @return {@code true} if a variable has been changed, {@code false}
     * otherwise
     */
    public static boolean domSubsetDom(IntVar sub, IntVar sup, ICause propagator) throws ContradictionException {
        boolean changed = false;
        changed |= sub.updateLowerBound(sup.getLB(), propagator);
        changed |= sub.updateUpperBound(sup.getUB(), propagator);
        int left = Integer.MIN_VALUE;
        int right = left;
        int ub = sub.getUB();
        for (int val = sub.getLB(); val <= ub; val = sub.nextValue(val)) {
            if (!sup.contains(val)) {
                if (val == right + 1) {
                    right = val;
                } else {
                    changed |= sub.removeInterval(left, right, propagator);
                    left = val;
                    right = val;
                }
            }
        }
        changed |= sub.removeInterval(left, right, propagator);
        return changed;
    }

    /**
     * Removes every element in an integer's domain that is not in the set's
     * envelope.
     *
     * @param sub the subset
     * @param sup the superset
     * @param propagator the propagator
     * @throws ContradictionException
     * @return {@code true} if a variable has been changed, {@code false}
     * otherwise
     */
    public static boolean domSubsetEnv(IntVar sub, SetVar sup, ICause propagator) throws ContradictionException {
        boolean changed = false;
        int left = Integer.MIN_VALUE;
        int right = left;
        int ub = sub.getUB();
        for (int val = sub.getLB(); val <= ub; val = sub.nextValue(val)) {
            if (!sup.getUB().contains(val)) {
                if (val == right + 1) {
                    right = val;
                } else {
                    changed |= sub.removeInterval(left, right, propagator);
                    left = val;
                    right = val;
                }
            }
        }
        changed |= sub.removeInterval(left, right, propagator);
        return changed;
    }

    /**
     * Removes every element in an integer's domain that is not in the set's
     * kernel.
     *
     * @param sub the subset
     * @param sup the superset
     * @param propagator the propagator
     * @throws ContradictionException
     * @return {@code true} if a variable has been changed, {@code false}
     * otherwise
     */
    public static boolean domSubsetKer(IntVar sub, SetVar sup, ICause propagator) throws ContradictionException {
        boolean changed = false;
        int left = Integer.MIN_VALUE;
        int right = left;
        int ub = sub.getUB();
        for (int val = sub.getLB(); val <= ub; val = sub.nextValue(val)) {
            if (!sup.getLB().contains(val)) {
                if (val == right + 1) {
                    right = val;
                } else {
                    changed |= sub.removeInterval(left, right, propagator);
                    left = val;
                    right = val;
                }
            }
        }
        changed |= sub.removeInterval(left, right, propagator);
        return changed;
    }

    /**
     * Removes every element in a set's envelope that is not in the set.
     *
     * @param sub the subset
     * @param sup the superset
     * @param propagator the propagator
     * @throws ContradictionException
     * @return {@code true} if a variable has been changed, {@code false}
     * otherwise
     */
    public static boolean envSubsetSet(SetVar sub, TIntSet sup, ICause propagator) throws ContradictionException {
        boolean changed = false;
        ISetIterator iter = sub.getUB().iterator();
        while (iter.hasNext()) {
            int val = iter.nextInt();
            if (!sup.contains(val)) {
                changed |= sub.remove(val, propagator);
            }
        }
        return changed;
    }

    /**
     * Removes every element in a set's envelope that is not in the integer's
     * domain.
     *
     * @param sub the subset
     * @param sup the superset
     * @param propagator the propagator
     * @throws ContradictionException
     * @return {@code true} if a variable has been changed, {@code false}
     * otherwise
     */
    public static boolean envSubsetDom(SetVar sub, IntVar sup, ICause propagator) throws ContradictionException {
        boolean changed = false;
        ISetIterator iter = sub.getUB().iterator();
        while (iter.hasNext()) {
            int val = iter.nextInt();
            if (!sup.contains(val)) {
                changed |= sub.remove(val, propagator);
            }
        }
        return changed;
    }

    /**
     * Removes every element in a set's envelope that is not in the other set's
     * envelope.
     *
     * @param sub the subset
     * @param sup the superset
     * @param propagator the propagator
     * @throws ContradictionException
     * @return {@code true} if a variable has been changed, {@code false}
     * otherwise
     */
    public static boolean envSubsetEnv(SetVar sub, SetVar sup, ICause propagator) throws ContradictionException {
        boolean changed = false;
        ISetIterator iter = sub.getUB().iterator();
        while (iter.hasNext()) {
            int val = iter.nextInt();
            if (!sup.getUB().contains(val)) {
                changed |= sub.remove(val, propagator);
            }
        }
        return changed;
    }

    /**
     * Removes every element in a set's envelope that is not in the set's
     * kernel.
     *
     * @param sub the subset
     * @param sup the superset
     * @param propagator the propagator
     * @throws ContradictionException
     * @return {@code true} if a variable has been changed, {@code false}
     * otherwise
     */
    public static boolean envSubsetKer(SetVar sub, SetVar sup, ICause propagator) throws ContradictionException {
        boolean changed = false;
        ISetIterator iter = sub.getUB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            if (!sup.getLB().contains(i)) {
                changed |= sub.remove(i, propagator);
            }
        }
        return changed;
    }

    /**
     * Adds every element in a set's kernel to the other set's kernel.
     *
     * @param sub the subset
     * @param sup the superset
     * @param propagator the propagator
     * @throws ContradictionException
     * @return {@code true} if a variable has been changed, {@code false}
     * otherwise
     */
    public static boolean kerSubsetKer(SetVar sub, SetVar sup, ICause propagator) throws ContradictionException {
        boolean changed = false;
        ISetIterator iter = sub.getLB().iterator();
        while (iter.hasNext()) {
            int i = iter.nextInt();
            changed |= sup.force(i, propagator);
        }
        return changed;
    }

    /**
     * Returns the null-terminated string.
     *
     * @param chars the characters
     * @return the string
     */
    public static String toString(IntVar[] chars) {
        char[] string = new char[chars.length];
        for (int i = 0; i < string.length; i++) {
            assert chars[i].isInstantiated();
            int val = chars[i].getValue();
            if (val < Character.MIN_VALUE || val > Character.MAX_VALUE) {
                throw new IllegalArgumentException();
            }
            if (val == 0) {
                return new String(string, 0, i);
            }
            string[i] = (char) val;
        }
        return new String(string);
    }
}
