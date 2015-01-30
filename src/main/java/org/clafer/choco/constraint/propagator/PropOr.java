package org.clafer.choco.constraint.propagator;

import org.clafer.common.Util;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.events.IntEventType;
import org.chocosolver.util.ESat;

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
        return IntEventType.instantiation();
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        // The number of uninstantiated variables.
        int count = 0;
        BoolVar last = null;
        for (BoolVar var : vars) {
            if (var.isInstantiated()) {
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
            if (var.isInstantiated()) {
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
