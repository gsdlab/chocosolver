package org.clafer.choco.constraint;

import org.clafer.choco.constraint.propagator.PropReifyEqualXY;
import solver.constraints.Constraint;
import solver.variables.BoolVar;
import solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
public class ReifyEqualXY extends Constraint<IntVar, PropReifyEqualXY> {

    private final boolean reifyC;

    public ReifyEqualXY(BoolVar reify, boolean reifyC, IntVar x, IntVar y) {
        super(new IntVar[]{reify, x, y}, reify.getSolver());
        this.reifyC = reifyC;
        setPropagators(new PropReifyEqualXY(reify, reifyC, x, y));
    }

    @Override
    public Constraint makeOpposite() {
        return new ReifyEqualXY((BoolVar) vars[0], !reifyC, vars[1], vars[2]);
    }
}
