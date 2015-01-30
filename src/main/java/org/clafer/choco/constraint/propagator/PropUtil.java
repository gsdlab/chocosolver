package org.clafer.choco.constraint.propagator;

import gnu.trove.set.TIntSet;
import org.chocosolver.solver.ICause;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.IIntDeltaMonitor;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;

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
        int[] iterate = new int[set.getEnvelopeSize()];
        int count = 0;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            iterate[count++] = i;
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
        int[] iterate = new int[set.getKernelSize()];
        int count = 0;
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext()) {
            iterate[count++] = i;
        }
        assert count == iterate.length;
        return iterate;
    }

    public static int getEnv(SetVar set, int index) {
        int env = set.getEnvelopeFirst();
        for (int i = 0; i < index && env != SetVar.END; i++) {
            env = set.getEnvelopeNext();
        }
        return env;
    }

    /**
     * Returns the largest element in the set's envelope. Returns
     * {@link SetVar#END} if the envelope is empty.
     *
     * @param set the set variable
     * @return the largest element in the set's envelope
     */
    public static int maxEnv(SetVar set) {
        int max = SetVar.END;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            max = i;
        }
        return max;
    }

    /**
     * Returns the largest element in the set's kernel. Returns
     * {@link SetVar#END} if the kernel is empty.
     *
     * @param set the set variable
     * @return the largest element in the set's kernel
     */
    public static int maxKer(SetVar set) {
        int max = SetVar.END;
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext()) {
            max = i;
        }
        return max;
    }

    /**
     * Returns the smallest element in the set's envelope. Returns
     * {@link SetVar#END} if the envelope is empty.
     *
     * @param set the set variable
     * @return the smallest element in the set's envelope
     */
    public static int minEnv(SetVar set) {
        return set.getEnvelopeFirst();
    }

    /**
     * Returns the smallest element in the set's kernel. Returns
     * {@link SetVar#END} if the kernel is empty.
     *
     * @param set the set variable
     * @return the smallest element in the set's kernel
     */
    public static int minKer(SetVar set) {
        return set.getKernelFirst();
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
            if (var.envelopeContains(value)) {
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
            if (var.kernelContains(value)) {
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
        if (i1.getDomainSize() < i2.getEnvelopeSize()) {
            int i = i1.getLB();
            int envFirst = i2.getEnvelopeFirst();
            if (envFirst >= i) {
                if (envFirst == i || i1.contains(envFirst)) {
                    return true;
                }
                i = i1.nextValue(envFirst);
            }
            int ub = i1.getUB();
            for (; i <= ub; i = i1.nextValue(i)) {
                if (i2.envelopeContains(i)) {
                    return true;
                }
            }
        } else {
            for (int i = i2.getEnvelopeFirst(); i != SetVar.END;) {
                int next = i1.nextValue(i - 1);
                while (i < next && i != SetVar.END) {
                    i = i2.getEnvelopeNext();
                }
                if (i == next) {
                    return true;
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
        if (i1.getDomainSize() < i2.getKernelSize()) {
            int i = i1.getLB();
            int envFirst = i2.getKernelFirst();
            if (envFirst >= i) {
                if (envFirst == i || i1.contains(envFirst)) {
                    return true;
                }
                i = i1.nextValue(envFirst);
            }
            int ub = i1.getUB();
            for (; i <= ub; i = i1.nextValue(i)) {
                if (i2.kernelContains(i)) {
                    return true;
                }
            }
        } else {
            for (int i = i2.getKernelFirst(); i != SetVar.END;) {
                int next = i1.nextValue(i - 1);
                while (i < next && i != SetVar.END) {
                    i = i2.getKernelNext();
                }
                if (i == next) {
                    return true;
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
        if (i1.getEnvelopeSize() > i2.getEnvelopeSize()) {
            small = i2;
            large = i1;
        }
        for (int i = small.getEnvelopeFirst(); i != SetVar.END; i = small.getEnvelopeNext()) {
            if (large.envelopeContains(i)) {
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
        if (i1.getEnvelopeSize() < i2.getEnvelopeSize()) {
            for (int i = i1.getEnvelopeFirst(); i != SetVar.END; i = i1.getEnvelopeNext()) {
                if (i2.kernelContains(i)) {
                    return true;
                }
            }
        } else {
            for (int i = i2.getKernelFirst(); i != SetVar.END; i = i2.getKernelNext()) {
                if (i1.envelopeContains(i)) {
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
        if (i1.getKernelSize() > i2.getKernelSize()) {
            small = i2;
            large = i1;
        }
        for (int i = small.getKernelFirst(); i != SetVar.END; i = small.getKernelNext()) {
            if (large.kernelContains(i)) {
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
        if (sub.getDomainSize() > sup.getEnvelopeSize()) {
            return false;
        }
        int ub = sub.getUB();
        for (int i = sub.getLB(); i <= ub; i = sub.nextValue(i)) {
            if (!sup.envelopeContains(i)) {
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
        if (sub.getDomainSize() > sup.getKernelSize()) {
            return false;
        }
        int ub = sub.getUB();
        for (int i = sub.getLB(); i <= ub; i = sub.nextValue(i)) {
            if (!sup.kernelContains(i)) {
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
        if (sub.getEnvelopeSize() > sup.getDomainSize()) {
            return false;
        }
        for (int i = sub.getEnvelopeFirst(); i != SetVar.END; i = sub.getEnvelopeNext()) {
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
        if (sub.getEnvelopeSize() > sup.getEnvelopeSize()) {
            return false;
        }
        for (int i = sub.getEnvelopeFirst(); i != SetVar.END; i = sub.getEnvelopeNext()) {
            if (!sup.envelopeContains(i)) {
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
        if (sub.getEnvelopeSize() > sup.getKernelSize()) {
            return false;
        }
        for (int i = sub.getEnvelopeFirst(); i != SetVar.END; i = sub.getEnvelopeNext()) {
            if (!sup.kernelContains(i)) {
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
        for (int i = sub.getKernelFirst(); i != SetVar.END; i = sub.getKernelNext()) {
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
        for (int i = sub.getKernelFirst(); i != SetVar.END; i = sub.getKernelNext()) {
            if (!sup.envelopeContains(i)) {
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
        for (int i = sub.getKernelFirst(); i != SetVar.END; i = sub.getKernelNext()) {
            if (!sup.kernelContains(i)) {
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
            if (!sup.envelopeContains(val)) {
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
            if (!sup.kernelContains(val)) {
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
        for (int val = sub.getEnvelopeFirst(); val != SetVar.END; val = sub.getEnvelopeNext()) {
            if (!sup.contains(val)) {
                changed |= sub.removeFromEnvelope(val, propagator);
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
        for (int val = sub.getEnvelopeFirst(); val != SetVar.END; val = sub.getEnvelopeNext()) {
            if (!sup.contains(val)) {
                changed |= sub.removeFromEnvelope(val, propagator);
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
        for (int i = sub.getEnvelopeFirst(); i != SetVar.END; i = sub.getEnvelopeNext()) {
            if (!sup.envelopeContains(i)) {
                changed |= sub.removeFromEnvelope(i, propagator);
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
        for (int i = sub.getEnvelopeFirst(); i != SetVar.END; i = sub.getEnvelopeNext()) {
            if (!sup.kernelContains(i)) {
                changed |= sub.removeFromEnvelope(i, propagator);
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
        for (int i = sub.getKernelFirst(); i != SetVar.END; i = sub.getKernelNext()) {
            changed |= sup.addToKernel(i, propagator);
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
