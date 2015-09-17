package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.procedure.IntProcedure;

/**
 *
 * @author jimmy
 */
public class PropContinuousUnion extends Propagator<Variable> {

    private final SetVar[] sets;
    private final ISetDeltaMonitor[] setsD;
    private final IntVar totalCard;
    private final int[] supports;

    public PropContinuousUnion(SetVar[] sets, IntVar totalCard) {
        super(buildArray(sets, totalCard), PropagatorPriority.BINARY, true);
        this.sets = sets;
        this.setsD = PropUtil.monitorDeltas(sets, this);
        this.totalCard = totalCard;
        this.supports = new int[totalCard.getUB()];
    }

    private static Variable[] buildArray(SetVar[] sets, IntVar totalCard) {
        Variable[] variables = new Variable[sets.length + 1];
        System.arraycopy(sets, 0, variables, 0, sets.length);
        variables[sets.length] = totalCard;
        return variables;
    }

    boolean isSetVar(int idx) {
        return idx < sets.length;
    }

    int getSetVarIndex(int idx) {
        return idx;
    }

    boolean isTotalCardVar(int idx) {
        return idx == sets.length;
    }
    
//TODO: Sept16
//    @Override
//    public int getPropagationConditions(int vIdx) {
//        if (isSetVar(vIdx)) {
//            return SetEventType.all();
//        }
//        assert isTotalCardVar(vIdx);
//        return IntEventType.all();
//    }

    private boolean support(int value) {
        int support = supports[value];
        if (sets[support].envelopeContains(value)) {
            return true;
        }
        for (int i = 0; i < sets.length; i++) {
            if (sets[i].envelopeContains(value)) {
                supports[value] = i;
                return true;
            }
        }
        return false;
    }

    private void findMate(int unionKer) throws ContradictionException {
        int mate = -1;
        for (int j = 0; j < sets.length; j++) {
            if (sets[j].envelopeContains(unionKer)) {
                // Found a second mate or in kernel.
                if (mate != -1 || sets[j].kernelContains(unionKer)) {
                    return;
                }
                mate = j;
            }
        }
        if (mate == -1) {
            // No mates.
            contradiction(totalCard, "too high");
        } else if (mate != -2) {
            // One mate.
            sets[mate].addToKernel(unionKer, this);
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        for (SetVar set : sets) {
            for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
                if (i < 0 || i >= totalCard.getUB()) {
                    set.removeFromEnvelope(i, this);
                }
            }
        }
        int maxEnv = PropUtil.maxEnv(sets[sets.length - 1]);
        int maxKer = PropUtil.maxKer(sets[sets.length - 1]);
        for (int i = sets.length - 2; i >= 0; i--) {
            maxEnv = Math.max(maxEnv, PropUtil.maxEnv(sets[i]));
            maxKer = Math.max(maxKer, PropUtil.maxKer(sets[i]));
        }
        totalCard.updateUpperBound(maxEnv == SetVar.END ? 0 : maxEnv + 1, this);
        if (totalCard.getLB() > 0) {
            totalCard.updateLowerBound(maxKer == SetVar.END ? 0 : maxKer + 1, this);
        }
        int ub = totalCard.getUB();
        for (int i = 0; i < ub; i++) {
            if (!support(i)) {
                totalCard.updateUpperBound(i, this);
                break;
            }
        }
        int lb = totalCard.getLB();
        for (int i = 0; i < lb; i++) {
            findMate(i);
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        if (isSetVar(idxVarInProp)) {
            int id = getSetVarIndex(idxVarInProp);
            setsD[id].freeze();
            setsD[id].forEach(pruneUnionOnSetEnv, SetEventType.REMOVE_FROM_ENVELOPE);
            setsD[id].forEach(pickUnionOnSetKer, SetEventType.ADD_TO_KER);
            setsD[id].unfreeze();
        } else {
            assert isTotalCardVar(idxVarInProp);

            int lb = totalCard.getLB();
            for (int i = 0; i < lb; i++) {
                findMate(i);
            }
            for (SetVar set : sets) {
                for (int i = set.getEnvelopeFirst(); i != SetVar.END; i = set.getEnvelopeNext()) {
                    if (i < 0 || i >= totalCard.getUB()) {
                        set.removeFromEnvelope(i, this);
                    }
                }
            }
            int ub = totalCard.getUB();
            for (int i = 0; i < ub; i++) {
                if (!support(i)) {
                    totalCard.updateUpperBound(i, this);
                    break;
                }
            }
        }
    }

    private final IntProcedure pruneUnionOnSetEnv = new IntProcedure() {

        @Override
        public void execute(int env) throws ContradictionException {
            int maxEnv = PropUtil.maxEnv(sets[sets.length - 1]);
            for (int i = sets.length - 2; i >= 0; i--) {
                maxEnv = Math.max(maxEnv, PropUtil.maxEnv(sets[i]));
            }
            totalCard.updateUpperBound(maxEnv == SetVar.END ? 0 : maxEnv + 1, PropContinuousUnion.this);
            int lb = totalCard.getLB();
            if (env < lb) {
                findMate(env);
            }
            if (!support(env)) {
                totalCard.updateUpperBound(env, PropContinuousUnion.this);
            }
        }
    };

    private final IntProcedure pickUnionOnSetKer = new IntProcedure() {

        @Override
        public void execute(int ker) throws ContradictionException {
            int f = totalCard.getLB();
            if (totalCard.updateLowerBound(ker + 1, PropContinuousUnion.this)) {
                int t = totalCard.getLB();
                for (int i = f; i < t; i++) {
                    findMate(i);
                }
            }
        }
    };

    @Override
    public ESat isEntailed() {
        for (SetVar set : sets) {
            if (set.getKernelSize() > 0) {
                if (set.getKernelFirst() < 0 || PropUtil.maxKer(set) >= totalCard.getUB()) {
                    return ESat.FALSE;
                }
            }
        }
        if (!totalCard.isInstantiated()) {
            return ESat.UNDEFINED;
        }
        for (SetVar set : sets) {
            if (set.getEnvelopeSize() > 0) {
                if (set.getEnvelopeFirst() < 0 || PropUtil.maxEnv(set) >= totalCard.getUB()) {
                    return ESat.UNDEFINED;
                }
            }
        }
        int tc = totalCard.getValue();

        boolean allRealized = true;
        for (int i = 0; i < tc; i++) {
            boolean realized = false;
            boolean support = false;
            for (SetVar set : sets) {
                if (set.kernelContains(i)) {
                    realized = true;
                    support = true;
                    break;
                } else if (set.envelopeContains(i)) {
                    support = true;
                }
            }
            allRealized &= realized;
            if (!support) {
                return ESat.FALSE;
            }
        }
        return allRealized ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "union(" + Arrays.toString(sets) + ") = {0, 1, ..., " + totalCard + " - 1}";
    }
}
