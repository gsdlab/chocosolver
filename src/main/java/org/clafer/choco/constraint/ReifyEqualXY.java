package org.clafer.choco.constraint;

import org.clafer.choco.constraint.propagator.PropReifyEqualXY;
import solver.constraints.Constraint;
import solver.variables.BoolVar;
import solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
public class ReifyEqualXY extends Constraint {

    private final BoolVar reify;
    private final boolean reifyC;
    private final IntVar x, y;

    public ReifyEqualXY(BoolVar reify, boolean reifyC, IntVar x, IntVar y) {
        super("reifyEqualXY", new PropReifyEqualXY(reify, reifyC, x, y));
        this.reify = reify;
        this.reifyC = reifyC;
        this.x = x;
        this.y = y;
    }

    @Override
    public Constraint makeOpposite() {
        return new ReifyEqualXY(reify, !reifyC, x, y);
    }
}
