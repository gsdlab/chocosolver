package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;

/**
 *
 * @author jimmy
 */
public class PropMask extends Propagator<SetVar> {

    private final SetVar set;
    private final SetVar masked;
    private final ISetDeltaMonitor setD;
    private final ISetDeltaMonitor maskedD;
    // Inclusive
    private final int from;
    // Exclusive
    private final int to;

    public PropMask(SetVar set, SetVar masked, int from, int to) {
        super(new SetVar[]{set, masked}, PropagatorPriority.UNARY, true);

        if (from > to) {
            throw new IllegalArgumentException();
        }

        this.set = set;
        this.masked = masked;
        this.setD = set.monitorDelta(this);
        this.maskedD = masked.monitorDelta(this);
        this.from = from;
        this.to = to;
    }

    private boolean isSetVar(int idx) {
        return idx == 0;
    }

    private boolean isMaskedVar(int idx) {
        return idx == 1;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return SetEventType.all();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        ISetIterator setKer = set.getLB().iterator();
        while (setKer.hasNext()) {
            int i = setKer.nextInt();
            if (i >= from && i < to) {
                masked.force(i - from, this);
            }
        }
        ISetIterator setEnv = set.getUB().iterator();
        while (setEnv.hasNext()) {
            int i = setEnv.nextInt();
            if (i >= from && i < to && !masked.getUB().contains(i - from)) {
                set.remove(i, this);
            }
        }
        ISetIterator maskedKer = masked.getLB().iterator();
        while (maskedKer.hasNext()) {
            int i = maskedKer.nextInt();
            set.force(i + from, this);
        }
        ISetIterator maskedEnv = masked.getUB().iterator();
        while (maskedEnv.hasNext()) {
            int i = maskedEnv.nextInt();
            if (i < 0 || i >= to - from || !set.getUB().contains(i + from)) {
                masked.remove(i, this);
            }
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isSetVar(idxVarInProp)) {
            setD.freeze();
            setD.forEach(this::pickMaskedOnSetKer, SetEventType.ADD_TO_KER);
            setD.forEach(this::pruneMaskedOnSetEnv, SetEventType.REMOVE_FROM_ENVELOPE);
            setD.unfreeze();
        } else {
            assert isMaskedVar(idxVarInProp);
            maskedD.freeze();
            maskedD.forEach(this::pickSetOnMaskedKer, SetEventType.ADD_TO_KER);
            maskedD.forEach(this::pruneSetOnMaskedEnv, SetEventType.REMOVE_FROM_ENVELOPE);
            maskedD.unfreeze();
        }
    }

    private void pickMaskedOnSetKer(int ker) throws ContradictionException {
        if (ker >= from && ker < to) {
            masked.force(ker - from, this);
        }
    }

    private void pruneMaskedOnSetEnv(int env) throws ContradictionException {
        if (env >= from && env < to) {
            masked.remove(env - from, this);
        }
    }

    private void pickSetOnMaskedKer(int ker) throws ContradictionException {
        assert ker < to - from;
        set.force(ker + from, this);
    }

    private void pruneSetOnMaskedEnv(int env) throws ContradictionException {
        assert env < to - from;
        set.remove(env + from, this);
    }

    @Override
    public ESat isEntailed() {
        ISetIterator setKer = set.getLB().iterator();
        while (setKer.hasNext()) {
            int i = setKer.nextInt();
            if (i >= from && i < to && !masked.getUB().contains(i - from)) {
                return ESat.FALSE;
            }
        }
        ISetIterator maskedKer = masked.getLB().iterator();
        while (maskedKer.hasNext()) {
            int i = maskedKer.nextInt();
            if (i < 0 || i >= to - from || !set.getUB().contains(i + from)) {
                return ESat.FALSE;
            }
        }
        return set.isInstantiated() && masked.isInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "mask(" + set + ", " + masked + ", " + from + ", " + to + ")";
    }
}
