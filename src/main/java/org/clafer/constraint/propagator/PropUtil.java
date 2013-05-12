package org.clafer.constraint.propagator;

import gnu.trove.TIntCollection;
import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.hash.TIntHashSet;
import org.clafer.Util;
import org.clafer.collection.Pair;
import solver.ICause;
import solver.exception.ContradictionException;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.delta.IDeltaMonitor;
import solver.variables.delta.IIntDeltaMonitor;
import solver.variables.delta.monitor.SetDeltaMonitor;

/**
 * 
 * @author jimmy
 */
public class PropUtil {

    public static IIntDeltaMonitor[] monitorDeltas(IntVar[] vars, ICause propogator) {
        IIntDeltaMonitor[] deltas = new IIntDeltaMonitor[vars.length];
        for (int i = 0; i < vars.length; i++) {
            deltas[i] = vars[i].monitorDelta(propogator);
        }
        return deltas;
    }

    public static SetDeltaMonitor[] monitorDeltas(SetVar[] vars, ICause propogator) {
        SetDeltaMonitor[] deltas = new SetDeltaMonitor[vars.length];
        for (int i = 0; i < vars.length; i++) {
            deltas[i] = vars[i].monitorDelta(propogator);
        }
        return deltas;
    }

    public static void freezeAll(IDeltaMonitor[] deltas) {
        for (IDeltaMonitor delta : deltas) {
            delta.freeze();
        }
    }

    public static void unfreezeAll(IDeltaMonitor[] deltas) {
        for (IDeltaMonitor delta : deltas) {
            delta.unfreeze();
        }
    }

    public static boolean approxCanIntersect(IntVar e1, SetVar e2, boolean otherwise) {
        if (e1.instantiated()) {
            return e2.envelopeContains(e1.getValue());
        }
        if (Math.min(e1.getDomainSize(), e2.getEnvelopeSize()) < 100) {
            return canIntersect(e1, e2);
        }
        return otherwise;
    }

    /**
     * @param e1
     * @param e2
     * @return true if and only if e1 in e2 is possible, false otherwise
     */
    public static boolean canIntersect(IntVar e1, SetVar e2) {
        if (e1.getDomainSize() < e2.getEnvelopeSize()) {
            int ub = e1.getUB();
            for (int i = Math.max(e1.getLB(), e2.getEnvelopeFirst()); i <= ub; i = e1.nextValue(i)) {
                if (e2.envelopeContains(i)) {
                    return true;
                }
            }
        } else {
            for (int i = e2.getEnvelopeFirst(); i != SetVar.END;) {
                int next = e1.nextValue(i - 1);
                while (i < next && i != SetVar.END) {
                    i = e2.getEnvelopeNext();
                }
                if (i == next) {
                    return true;
                }
            }
        }
        return false;
    }

    public static boolean isSubsetOfEnv(TIntCollection sub, SetVar sup) {
        TIntIterator iter = sub.iterator();
        while (iter.hasNext()) {
            if (!sup.envelopeContains(iter.next())) {
                return false;
            }
        }
        return true;
    }

    public static boolean isKerSubsetOf(SetVar sub, TIntCollection sup) {
        for (int i = sub.getKernelFirst(); i != SetVar.END; i = sub.getKernelNext()) {
            if (!sup.contains(i)) {
                return false;
            }
        }
        return true;
    }

    public static void subsetKer(SetVar sub, SetVar sup, ICause propogator) throws ContradictionException {
        for (int i = sub.getKernelFirst(); i != SetVar.END; i = sub.getKernelNext()) {
            sup.addToKernel(i, propogator);
        }
    }

    public static void subsetEnv(SetVar sub, TIntHashSet sup, ICause propogator) throws ContradictionException {
        for (int i = sub.getEnvelopeFirst(); i != SetVar.END; i = sub.getEnvelopeNext()) {
            if (!sup.contains(i)) {
                sub.removeFromEnvelope(i, propogator);
            }
        }
    }

    public static void subsetEnv(SetVar sub, SetVar sup, ICause propogator) throws ContradictionException {
        for (int i = sub.getEnvelopeFirst(); i != SetVar.END; i = sub.getEnvelopeNext()) {
            if (!sup.envelopeContains(i)) {
                sub.removeFromEnvelope(i, propogator);
            }
        }
    }

    public static void intSubsetEnv(IntVar sub, SetVar sup, ICause propagator) throws ContradictionException {
        int left = Integer.MIN_VALUE;
        int right = left;
        int ub = sub.getUB();
        for (int val = sub.getLB(); val <= ub; val = sub.nextValue(val)) {
            if (!sup.envelopeContains(val)) {
                if (val == right + 1) {
                    right = val;
                } else {
                    sub.removeInterval(left, right, propagator);
                    left = val;
                    right = val;
                }
            }
        }
        sub.removeInterval(left, right, propagator);
    }

    public static void intsSubsetEnv(IntVar[] subs, SetVar sup, ICause propagator) throws ContradictionException {
        for (IntVar sub : subs) {
            intSubsetEnv(sub, sup, propagator);
        }
    }

    public static void envSubsetInt(SetVar sub, IntVar sup, ICause propagator) throws ContradictionException {
        for (int val = sub.getEnvelopeFirst(); val != SetVar.END; val = sub.getEnvelopeNext()) {
            if (!sup.contains(val)) {
                sub.removeFromEnvelope(val, propagator);
            }
        }
    }

    public static void envSubsetInts(SetVar sub, IntVar[] sup, ICause propagator) throws ContradictionException {
        for (int val = sub.getEnvelopeFirst(); val != SetVar.END; val = sub.getEnvelopeNext()) {
            if (!contains(sup, val)) {
                sub.removeFromEnvelope(val, propagator);
            }
        }
    }

    /**
     * @param union
     * @param val
     * @return - true if and only if one of the IntVar contains val, false otherwise
     */
    public static boolean contains(IntVar[] union, int val) {
        for (IntVar var : union) {
            if (var.contains(val)) {
                return true;
            }
        }
        return false;
    }

    public static int[] iterateEnv(SetVar set) {
        int[] iterate = new int[set.getEnvelopeSize()];
        int count = 0;
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            iterate[count++] = i;
        }
        assert count == iterate.length;
        return iterate;
    }

    public static void iterateEnv(SetVar set, TIntCollection collection) {
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            collection.add(i);
        }
    }

    public static int[] iterateKer(SetVar set) {
        int[] iterate = new int[set.getKernelSize()];
        int count = 0;
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext()) {
            iterate[count++] = i;
        }
        assert count == iterate.length;
        return iterate;
    }

    public static void iterateKer(SetVar set, TIntCollection collection) {
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext()) {
            collection.add(i);
        }
    }

    public static Pair<int[], int[]> minMaxEnv(SetVar set, int top) {
        if (set.getEnvelopeSize() <= top) {
            int[] env = iterateEnv(set);
            return new Pair<int[], int[]>(env, env);
        }

        int[] min = new int[top];
        int[] max = new int[top];

        int i = 0;
        int j = set.getEnvelopeFirst();
        for (; i < top; i++, j = set.getEnvelopeNext()) {
            assert j != SetVar.END;
            min[i] = j;
            max[i] = j;
        }

        int index = 0;
        for (; j != SetVar.END; j = set.getEnvelopeNext()) {
            max[index++] = j;
            if (index >= max.length) {
                index = 0;
            }
        }

        // Resort the max array
        Util.shiftLeft(max, index);

        return new Pair<int[], int[]>(min, max);
    }

    public static int[] getValues(IntVar[] vars) {
        int[] values = new int[vars.length];
        for (int i = 0; i < values.length; i++) {
            values[i] = vars[i].getValue();
        }
        return values;
    }

    public static int[][] getValues(SetVar[] vars) {
        int[][] values = new int[vars.length][];
        for (int i = 0; i < values.length; i++) {
            values[i] = vars[i].getValue();
        }
        return values;
    }
}
