package org.clafer.constraint.propagator;

import gnu.trove.set.hash.TIntHashSet;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.constraints.set.SetConstraintsFactory;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;
import util.ESat;

/**
 * Assumptions: Take set is over a small domain. Ref.length is small.
 * 
 * @author jimmy
 */
public class PropJoinRef extends Propagator<Variable> {

    private final SetVar take;
    private final IntVar[] refs;
    private final SetVar to;

    public PropJoinRef(SetVar take, IntVar[] refs, SetVar to) {
        super(buildArray(take, to, refs), PropagatorPriority.BINARY);
        this.take = take;
        this.refs = refs;
        this.to = to;
    }

    private static Variable[] buildArray(SetVar take, SetVar to, IntVar[] refs) {
        Variable[] array = new Variable[refs.length + 2];
        array[0] = take;
        array[1] = to;
        System.arraycopy(refs, 0, array, 2, refs.length);
        return array;
    }

    boolean isTake(int idx) {
        return idx == 0;
    }

    boolean isTo(int idx) {
        return idx == 1;
    }

    boolean isRefsVar(int idx) {
        return idx >= 2;
    }

    int getRefsVarIndex(int idx) {
        return idx - 2;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isTake(vIdx)) {
            return EventType.ADD_TO_KER.mask + EventType.REMOVE_FROM_ENVELOPE.mask + EventType.INSTANTIATE.mask;
        }
        if (isTo(vIdx)) {
            return EventType.ADD_TO_KER.mask + EventType.REMOVE_FROM_ENVELOPE.mask;
        }
        assert isRefsVar(vIdx);
        return EventType.REMOVE.mask + EventType.INSTANTIATE.mask;
    }

    private boolean possibleTo(int[] take, int to) {
        for (int i : take) {
            if (refs[i].contains(to)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        // Prune env
        // Need to iterate env(take) many times so read it into an array
        int[] takeEnv = PropUtil.iterateEnv(take);
        for (int i = to.getEnvelopeFirst(); i != SetVar.END; i = to.getEnvelopeNext()) {
            if (!possibleTo(takeEnv, i)) {
                to.removeFromEnvelope(i, aCause);
            }
        }

        // Prune refs, Pick to
        for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
            PropUtil.subsetEnv(refs[i], to, aCause);
            if (refs[i].instantiated()) {
                to.addToKernel(refs[i].getValue(), aCause);
            }
        }

        // Prune take
        for (int i : takeEnv) {
            if (!PropUtil.canIntersect(refs[i], to)) {
                take.removeFromEnvelope(i, aCause);
            }
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        propagate(mask);
    }

    @Override
    public ESat isEntailed() {
        for (int i = take.getKernelFirst(); i != SetVar.END; i = take.getKernelNext()) {
            if (!PropUtil.approxCanIntersect(refs[i], to, true)) {
                return ESat.FALSE;
            }
        }
        if (!take.instantiated() || !to.instantiated()) {
            return ESat.UNDEFINED;
        }
        TIntHashSet values = new TIntHashSet();
        for (IntVar ref : refs) {
            if (!ref.instantiated()) {
                return ESat.UNDEFINED;
            }
            values.add(ref.getValue());
        }
        return values.containsAll(to.getValue()) ? ESat.TRUE : ESat.UNDEFINED;
    }
}
