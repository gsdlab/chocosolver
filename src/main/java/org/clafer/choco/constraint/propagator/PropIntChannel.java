package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.solver.variables.Variable;
import org.chocosolver.solver.variables.delta.IIntDeltaMonitor;
import org.chocosolver.solver.variables.delta.ISetDeltaMonitor;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.procedure.IntProcedure;

/**
 * An idempotent and more efficient propagator than the default one. Does not
 * require an supplementary disjoint propagator like the default one.
 *
 * @author jimmy
 */
public class PropIntChannel extends Propagator<Variable> {

    private final SetVar[] sets;
    private ISetDeltaMonitor[] setsD;
    private final IntVar[] ints;
    private IIntDeltaMonitor[] intsD;

    public PropIntChannel(SetVar[] sets, IntVar[] ints) {
        super(buildArray(sets, ints), PropagatorPriority.LINEAR, true);
        this.sets = sets;
        this.setsD = PropUtil.monitorDeltas(sets, aCause);
        this.ints = ints;
        this.intsD = PropUtil.monitorDeltas(ints, aCause);
    }

    private static Variable[] buildArray(SetVar[] sets, IntVar[] ints) {
        Variable[] vars = new Variable[sets.length + ints.length];
        System.arraycopy(sets, 0, vars, 0, sets.length);
        System.arraycopy(ints, 0, vars, sets.length, ints.length);
        return vars;
    }

    private boolean isSetVar(int idx) {
        return idx < sets.length;
    }

    private int getSetVarIndex(int idx) {
        assert isSetVar(idx);
        return idx;
    }

    private boolean isIntVar(int idx) {
        return idx >= sets.length;
    }

    private int getIntVarIndex(int idx) {
        assert isIntVar(idx);
        return idx - sets.length;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        if (isSetVar(vIdx)) {
            return SetEventType.all();
        }
        assert isIntVar(vIdx);
        return IntEventType.all();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        for (int i = 0; i < ints.length; i++) {
            int ub = ints[i].getUB();
            for (int j = ints[i].getLB(); j <= ub; j = ints[i].nextValue(j)) {
                if (j < 0 || j >= sets.length || !sets[j].envelopeContains(i)) {
                    ints[i].removeValue(j, aCause);
                }
            }
            if (ints[i].isInstantiated()) {
                sets[ints[i].getValue()].addToKernel(i, aCause);
            }
        }
        for (int i = 0; i < sets.length; i++) {
            for (int j = sets[i].getKernelFirst(); j != SetVar.END; j = sets[i].getKernelNext()) {
                if (j >= 0 && j < ints.length) {
                    ints[j].instantiateTo(i, aCause);
                }
            }
        }
        for (int i = 0; i < sets.length; i++) {
            for (int j = sets[i].getEnvelopeFirst(); j != SetVar.END; j = sets[i].getEnvelopeNext()) {
                if (j < 0 || j >= ints.length || !ints[j].contains(i)) {
                    sets[i].removeFromEnvelope(j, aCause);
                }
            }
        }
    }

    @Override
    public void propagate(final int idxVarInProp, final int mask) throws ContradictionException {
        if (isSetVar(idxVarInProp)) {
            final int id = getSetVarIndex(idxVarInProp);

            setsD[id].freeze();
            setsD[id].forEach(new IntProcedure() {
                @Override
                public void execute(int setKer) throws ContradictionException {
                    ints[setKer].instantiateTo(id, aCause);
                    for (int i = 0; i < sets.length; i++) {
                        if (i != id) {
                            sets[i].removeFromEnvelope(setKer, aCause);
                        }
                    }
                }
            }, SetEventType.ADD_TO_KER);
            setsD[id].forEach(new IntProcedure() {
                @Override
                public void execute(int setEnv) throws ContradictionException {
                    if (ints[setEnv].removeValue(id, aCause) && ints[setEnv].isInstantiated()) {
                        int val = ints[setEnv].getValue();
                        sets[val].addToKernel(setEnv, aCause);
                        for (int i = 0; i < sets.length; i++) {
                            if (i != val) {
                                sets[i].removeFromEnvelope(setEnv, aCause);
                            }
                        }
                    }
                }
            }, SetEventType.REMOVE_FROM_ENVELOPE);
            setsD[id].unfreeze();
        } else {
            assert isIntVar(idxVarInProp);
            final int id = getIntVarIndex(idxVarInProp);

            intsD[id].freeze();
            intsD[id].forEachRemVal(new IntProcedure() {
                @Override
                public void execute(int intRem) throws ContradictionException {
                    sets[intRem].removeFromEnvelope(id, aCause);
                }
            });
            if (ints[id].isInstantiated()) {
                sets[ints[id].getValue()].addToKernel(id, aCause);
            }
            intsD[id].unfreeze();
        }
    }

    @Override
    public ESat isEntailed() {
        for (int i = 0; i < ints.length; i++) {
            if (ints[i].isInstantiated()) {
                int value = ints[i].getValue();
                if (value < 0 || value >= sets.length || !sets[value].envelopeContains(i)) {
                    return ESat.FALSE;
                }
            }
        }
        for (int i = 0; i < sets.length; i++) {
            for (int j = sets[i].getKernelFirst(); j != SetVar.END; j = sets[i].getKernelNext()) {
                if (j < 0 || j >= ints.length || !ints[j].contains(i)) {
                    return ESat.FALSE;
                }
            }
        }
        return isCompletelyInstantiated() ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "intChannel(" + Arrays.toString(sets) + ", " + Arrays.toString(ints) + ")";
    }
}
