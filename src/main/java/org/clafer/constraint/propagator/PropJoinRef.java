package org.clafer.constraint.propagator;

import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.PropagatorPriority;
import solver.exception.ContradictionException;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;
import util.ESat;

/**
 *
 * @author jimmy
 */
public class PropJoinRef extends Propagator<Variable> {

    public PropJoinRef(SetVar take, IntVar[] refs, SetVar to) {
        super(buildArray(take, to, refs), PropagatorPriority.BINARY);
    }

    private static Variable[] buildArray(SetVar take, SetVar to, IntVar[] refs) {
        SetVar[] array = new SetVar[refs.length + 2];
        array[0] = take;
        array[1] = to;
        System.arraycopy(refs, 0, array, 2, refs.length);
        return array;
    }

    @Override
    public int getPropagationConditions(int vIdx) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void propagate(int evtmask) throws ContradictionException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void propagate(int idxVarInProp, int mask) throws ContradictionException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public ESat isEntailed() {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
