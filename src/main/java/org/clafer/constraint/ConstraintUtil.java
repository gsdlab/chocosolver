package org.clafer.constraint;

import gnu.trove.TIntCollection;
import gnu.trove.set.hash.TIntHashSet;
import solver.ICause;
import solver.exception.ContradictionException;
import solver.variables.SetVar;
import solver.variables.delta.monitor.SetDeltaMonitor;

/**
 *
 * @author jimmy
 */
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

    public static void iterateEnv(SetVar set, TIntCollection collection) {
        for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
            collection.add(i);
        }
    }
}
