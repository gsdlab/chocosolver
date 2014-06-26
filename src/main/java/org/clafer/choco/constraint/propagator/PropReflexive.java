package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import solver.constraints.Propagator;
import solver.constraints.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.EventType;
import solver.variables.SetVar;
import util.ESat;

/**
 *
 * @author jimmy
 */
public class PropReflexive extends Propagator<SetVar> {

    private static final long serialVersionUID = 1L;

    public PropReflexive(SetVar[] relation) {
        super(relation, PropagatorPriority.UNARY, false);
    }

    @Override
    protected int getPropagationConditions(int vIdx) {
        return EventType.VOID.mask;
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        for (int i = 0; i < vars.length; i++) {
            vars[i].addToKernel(i, aCause);
        }
        setPassive();
    }

    @Override
    public ESat isEntailed() {
        boolean allInstantiated = true;
        for (int i = 0; i < vars.length; i++) {
            SetVar var = vars[i];
            allInstantiated &= var.isInstantiated();
            if (!var.envelopeContains(i)) {
                return ESat.FALSE;
            }
        }
        return allInstantiated ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "reflexive(" + Arrays.toString(vars) + ")";
    }
}
