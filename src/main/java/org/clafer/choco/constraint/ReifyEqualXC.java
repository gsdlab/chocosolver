package org.clafer.choco.constraint;

import org.clafer.choco.constraint.propagator.PropReifyEqualXC;
import solver.constraints.Constraint;
import solver.variables.BoolVar;
import solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
public class ReifyEqualXC extends Constraint<IntVar, PropReifyEqualXC> {

    private final boolean reifyC;
    private final int c;

    public ReifyEqualXC(BoolVar reify, boolean reifyC, IntVar x, int c) {
        super(new IntVar[]{reify, x}, reify.getSolver());
        this.reifyC = reifyC;
        this.c = c;
        setPropagators(new PropReifyEqualXC(reify, reifyC, x, c));
    }

    @Override
    public Constraint makeOpposite() {
        return new ReifyEqualXC((BoolVar) vars[0], !reifyC, vars[1], c);
    }
}
