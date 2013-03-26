package org.clafer.constraint;

import solver.Solver;
import solver.constraints.IntConstraint;
import solver.constraints.propagators.Propagator;
import solver.constraints.propagators.binary.PropGreaterOrEqualX_Y;
import solver.variables.IntVar;
import util.ESat;

/**
 *
 * @author jimmy
 */
public class Increasing extends IntConstraint<IntVar> {

    public Increasing(IntVar[] vars, Solver solver) {
        super(vars, solver);

        Propagator[] propogators = new Propagator[vars.length - 1];
        for (int i = 1; i < vars.length; i++) {
            propogators[i - 1] = new PropGreaterOrEqualX_Y(new IntVar[]{vars[i], vars[i - 1]});
        }
        setPropagators(propogators);
    }

    @Override
    public ESat isSatisfied(int[] tuple) {
        for (int i = 1; i < tuple.length; i++) {
            if (tuple[i] > tuple[i - 1]) {
                return ESat.FALSE;
            }
        }
        return ESat.TRUE;
    }
}
