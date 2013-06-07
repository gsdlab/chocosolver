package org.clafer.choco.constraint.propagator;

import org.clafer.common.Util;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.BoolVar;
import solver.variables.EventType;
import util.ESat;

/**
 *
 * @author jimmy
 */
public class PropOr extends Propagator<BoolVar> {

    public PropOr(BoolVar[] vars) {
        super(vars, PropagatorPriority.BINARY, true);
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        return EventType.INSTANTIATE.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        // The number of uninstantiated variables.
        int count = 0;
        BoolVar last = null;
        for (BoolVar var : vars) {
            if (var.instantiated()) {
                if (var.getValue() == 1) {
                    setPassive();
                    return;
                }
            } else {
                count++;
                last = var;
            }
        }
        // Every variable if false except for last.
        if (count == 1) {
            last.setToTrue(aCause);
        }
        if (count == 0) {
            contradiction(vars[0], "All false.");
        }
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        assert EventType.isInstantiate(mask);
        if (vars[idxVarInProp].getValue() == 1) {
            setPassive();
        } else {
            propagate(mask);
        }
    }

    @Override
    public ESat isEntailed() {
        boolean allInstantiated = true;
        for (BoolVar var : vars) {
            if (var.instantiated()) {
                if (var.getValue() == 1) {
                    return ESat.TRUE;
                }
            } else {
                allInstantiated = false;
            }
        }
        return allInstantiated ? ESat.FALSE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return Util.intercalate(" or ", vars);
    }
}
