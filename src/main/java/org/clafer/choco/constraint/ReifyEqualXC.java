package org.clafer.choco.constraint;

import org.clafer.choco.constraint.propagator.PropReifyEqualXC;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
public class ReifyEqualXC extends Constraint {

    private final BoolVar reify;
    private final boolean reifyC;
    private final IntVar x;
    private final int c;

    public ReifyEqualXC(BoolVar reify, boolean reifyC, IntVar x, int c) {
        super("reifyEqualXC", new PropReifyEqualXC(reify, reifyC, x, c));
        this.reify = reify;
        this.reifyC = reifyC;
        this.x = x;
        this.c = c;
    }

    @Override
    public Constraint makeOpposite() {
        return new ReifyEqualXC(reify, !reifyC, x, c);
    }
}
