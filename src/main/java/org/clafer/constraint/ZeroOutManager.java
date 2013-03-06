package org.clafer.constraint;

import static choco.Choco.*;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;

/**
 *
 * @author jimmy
 */
public class ZeroOutManager {

    /**
     * x_i = z => y_i = 0
     */
    public static Constraint zeroOut(IntegerVariable[] x, IntegerVariable[] y, int z) {
        if (x.length != y.length) {
            throw new IllegalArgumentException();
        }
        Constraint[] cs = new Constraint[x.length];
        for (int i = 0; i < x.length; i++) {
            cs[i] = implies(eq(x[i], z), neq(y[i], z));
        }
        return and(cs);
    }

    public static Constraint zeroOut(IntegerVariable[] parents, IntegerVariable[] refs) {
        return zeroOut(parents, refs, parents[0].getUppB());
    }
}
