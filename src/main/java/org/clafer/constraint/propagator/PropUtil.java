package org.clafer.constraint.propagator;

import gnu.trove.TIntCollection;
import gnu.trove.set.hash.TIntHashSet;
import solver.ICause;
import solver.exception.ContradictionException;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.delta.monitor.SetDeltaMonitor;

/**
 * 
 * @author jimmy
 */
public class PropUtil {

    public static SetDeltaMonitor[] monitorDeltas(SetVar[] vars, ICause propogator) {
        SetDeltaMonitor[] deltas = new SetDeltaMonitor[vars.length];
        for (int i = 0; i < vars.length; i++) {
            deltas[i] = vars[i].monitorDelta(propogator);
        }
        return deltas;
    }

    public static void freezeAll(SetDeltaMonitor[] deltas) {
        for (SetDeltaMonitor delta : deltas) {
            delta.freeze();
        }
    }

    public static void unfreezeAll(SetDeltaMonitor[] deltas) {
        for (SetDeltaMonitor delta : deltas) {
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
     * TODO: use region iterator
     * 
     * @param e1
     * @param e2
     * @return true if and only if e1 in e2 is possible, false otherwise
     */
    public static boolean canIntersect(IntVar e1, SetVar e2) {
        if (e1.getDomainSize() < e2.getEnvelopeSize()) {
            int ub = e1.getUB();
            for (int i = e1.getLB(); i <= ub; i = e1.nextValue(i)) {
                if (e2.envelopeContains(i)) {
                    return true;
                }
            }
        } else {
            for (int i = e2.getEnvelopeFirst(); i != SetVar.END; i = e2.getEnvelopeNext()) {
                if (e1.contains(i)) {
                    return true;
                }
            }
        }
        return false;
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

    public static void subsetEnv(IntVar sub, SetVar sup, ICause propagtor) throws ContradictionException {
        int left = Integer.MIN_VALUE;
        int right = left;
        int ub = sub.getUB();
        for (int val = sub.getLB(); val <= ub; val = sub.nextValue(val)) {
            if (!sup.envelopeContains(val)) {
                if (val == right + 1) {
                    right = val;
                } else {
                    sub.removeInterval(left, right, propagtor);
                    left = val;
                    right = val;
                }
            }
        }
        sub.removeInterval(left, right, propagtor);
    }

    public static void subsetEnv(SetVar sub, IntVar sup, ICause propagator) throws ContradictionException {
        for (int val = sub.getEnvelopeFirst(); val != SetVar.END; val = sub.getEnvelopeNext()) {
            if (!sup.contains(val)) {
                sub.removeFromEnvelope(val, propagator);
            }
        }
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
}
