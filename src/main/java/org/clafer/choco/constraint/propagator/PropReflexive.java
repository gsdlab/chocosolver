package org.clafer.choco.constraint.propagator;

import java.util.Arrays;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.solver.constraints.PropagatorPriority;
import org.chocosolver.solver.exception.ContradictionException;
import org.chocosolver.solver.variables.SetVar;
import org.chocosolver.util.ESat;

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
    public void propagate(int evtmask) throws ContradictionException {
        for (int i = 0; i < vars.length; i++) {
            vars[i].force(i, this);
        }
        setPassive();
    }

    @Override
    public ESat isEntailed() {
        boolean allUbContains = true;
        for (int i = 0; i < vars.length; i++) {
            SetVar var = vars[i];
            if (!var.getUB().contains(i)) {
                return ESat.FALSE;
            }
            allUbContains &= var.getLB().contains(i);
        }
        return allUbContains ? ESat.TRUE : ESat.UNDEFINED;
    }

    @Override
    public String toString() {
        return "reflexive(" + Arrays.toString(vars) + ")";
    }
}
