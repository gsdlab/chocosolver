package org.clafer.choco.constraint.propagator;

import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.IntEventType;
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

    public PropContinuousUnion(SetVar[] sets, IntVar totalCard) {
        super(buildArray(sets, totalCard), PropagatorPriority.BINARY, true);
        this.sets = sets;
        this.setsD = PropUtil.monitorDeltas(sets, aCause);
        this.totalCard = totalCard;
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

    @Override
    protected int getPropagationConditions(int vIdx) {
        if (isSetVar(vIdx)) {
            return SetEventType.all();
        }
        assert isTotalCardVar(vIdx);
        return IntEventType.INCLOW.getMask() + IntEventType.instantiation();
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
            sets[mate].addToKernel(unionKer, aCause);
        }
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        int maxEnv = PropUtil.maxEnv(sets[sets.length - 1]);
        int maxKer = PropUtil.maxKer(sets[sets.length - 1]);
        for (int i = sets.length - 2; i >= 0; i--) {
            maxEnv = Math.max(maxEnv, PropUtil.maxEnv(sets[i]));
            maxKer = Math.max(maxKer, PropUtil.maxKer(sets[i]));
        }
        totalCard.updateUpperBound(maxEnv == SetVar.END ? 0 : maxEnv + 1, aCause);
        totalCard.updateLowerBound(maxKer == SetVar.END ? 0 : maxKer + 1, aCause);
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
        }
    }

    private final IntProcedure pruneUnionOnSetEnv = new IntProcedure() {

        @Override
        public void execute(int env) throws ContradictionException {
//            int maxEnv = PropUtil.maxEnv(sets[sets.length - 1]);
//            for (int i = sets.length - 2; i >= 0 && sets[i + 1].getKernelSize() == 0; i--) {
//                maxEnv = Math.max(maxEnv, PropUtil.maxEnv(sets[i]));
//            }
//            totalCard.updateUpperBound(maxEnv == SetVar.END ? 0 : maxEnv + 1, aCause);
//            if (env < totalCard.getLB()) {
//                findMate(env);
//            }
            propagate(env);
        }
    };

    private final IntProcedure pickUnionOnSetKer = new IntProcedure() {

        @Override
        public void execute(int ker) throws ContradictionException {
            int f = totalCard.getLB();
            if (totalCard.updateLowerBound(ker + 1, aCause)) {
                int t = totalCard.getLB();
                for (int i = f; i < t; i++) {
                    findMate(i);
                }
            }
        }
    };

    @Override
    public ESat isEntailed() {
        return ESat.TRUE;
    }
}
