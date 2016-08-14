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
import org.chocosolver.solver.variables.events.SetEventType;
import org.chocosolver.util.ESat;
import org.chocosolver.util.objects.setDataStructures.ISetIterator;
import org.chocosolver.util.procedure.IntProcedure;

/**
 * An idempotent and more efficient propagator than the default one. Does not
 * require an supplementary disjoint propagator like the default one.
 *
 * @author jimmy
 */
public class PropIntChannel extends Propagator<Variable> {

    private final SetVar[] sets;
    private final ISetDeltaMonitor[] setsD;
    private final IntVar[] ints;
    private final IIntDeltaMonitor[] intsD;

    public PropIntChannel(SetVar[] sets, IntVar[] ints) {
        super(buildArray(sets, ints), PropagatorPriority.LINEAR, true);
        this.sets = sets;
        this.setsD = PropUtil.monitorDeltas(sets, this);
        this.ints = ints;
        this.intsD = PropUtil.monitorDeltas(ints, this);
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
    public void propagate(int evtmask) throws ContradictionException {
        for (int i = 0; i < ints.length; i++) {
            int ub = ints[i].getUB();
            for (int j = ints[i].getLB(); j <= ub; j = ints[i].nextValue(j)) {
                if (j < 0 || j >= sets.length || !sets[j].getUB().contains(i)) {
                    ints[i].removeValue(j, this);
                }
            }
            if (ints[i].isInstantiated()) {
                sets[ints[i].getValue()].force(i, this);
            }
        }
        for (int i = 0; i < sets.length; i++) {
            ISetIterator iter = sets[i].getLB().iterator();
            while (iter.hasNext()) {
                int j = iter.nextInt();
                if (j >= 0 && j < ints.length) {
                    ints[j].instantiateTo(i, this);
                }
            }
        }
        for (int i = 0; i < sets.length; i++) {
            ISetIterator iter = sets[i].getUB().iterator();
            while (iter.hasNext()) {
                int j = iter.nextInt();
                if (j < 0 || j >= ints.length || !ints[j].contains(i)) {
                    sets[i].remove(j, this);
                }
            }
        }
    }

    @Override
    public void propagate(final int idxVarInProp, final int mask) throws ContradictionException {
        if (isSetVar(idxVarInProp)) {
            final int id = getSetVarIndex(idxVarInProp);

            setsD[id].freeze();
            setsD[id].forEach(setKer -> {
                ints[setKer].instantiateTo(id, this);
                for (int i = 0; i < sets.length; i++) {
                    if (i != id) {
                        sets[i].remove(setKer, this);
                    }
                }
            }, SetEventType.ADD_TO_KER);
            setsD[id].forEach(setEnv -> {
                if (ints[setEnv].removeValue(id, this) && ints[setEnv].isInstantiated()) {
                    int val = ints[setEnv].getValue();
                    sets[val].force(setEnv, this);
                    for (int i = 0; i < sets.length; i++) {
                        if (i != val) {
                            sets[i].remove(setEnv, this);
                        }
                    }
                }
            }, SetEventType.REMOVE_FROM_ENVELOPE);
            setsD[id].unfreeze();
        } else {
            assert isIntVar(idxVarInProp);
            final int id = getIntVarIndex(idxVarInProp);

            intsD[id].freeze();
            intsD[id].forEachRemVal((IntProcedure) intRem -> {
                sets[intRem].remove(id, this);
            });
            if (ints[id].isInstantiated()) {
                sets[ints[id].getValue()].force(id, this);
            }
            intsD[id].unfreeze();
        }
    }

    @Override
    public ESat isEntailed() {
        for (int i = 0; i < ints.length; i++) {
            if (ints[i].isInstantiated()) {
                int value = ints[i].getValue();
                if (value < 0 || value >= sets.length || !sets[value].getUB().contains(i)) {
                    return ESat.FALSE;
                }
            }
        }
        for (int i = 0; i < sets.length; i++) {
            ISetIterator iter = sets[i].getLB().iterator();
            while(iter.hasNext()) {
                int j = iter.nextInt();
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
