package org.clafer.constraint;

import gnu.trove.TIntCollection;
import gnu.trove.set.hash.TIntHashSet;
import solver.ICause;
import solver.exception.ContradictionException;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.delta.monitor.SetDeltaMonitor;
import util.procedure.IntProcedure;

/**
 * Rename to PropagatorUtil
 * @author jimmy
 */
@Deprecated
public class ConstraintUtil {

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

    public static void iterateEnv(SetVar set, TIntCollection collection) {
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            collection.add(i);
        }
    }

    public static void forEachKer(SetVar set, IntProcedure proc) throws ContradictionException {
        for (int i = set.getKernelFirst(); i != SetVar.END; i = set.getKernelNext()) {
            proc.execute(i);
        }
    }
}
