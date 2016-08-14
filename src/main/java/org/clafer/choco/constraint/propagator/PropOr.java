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
        BoolVar last = null;
        for (BoolVar var : vars) {
            if (var.isInstantiated()) {
                if (var.getValue() == 1) {
                    setPassive();
                    return;
                }
            } else if (last != null) {
                return;
            } else {
                last = var;
            }
        }
        if (last == null) {
            fails();
        } else {
            last.setToTrue(this);
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
