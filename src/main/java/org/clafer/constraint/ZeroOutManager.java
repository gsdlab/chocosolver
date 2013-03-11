package org.clafer.constraint;

import static choco.Choco.*;
import choco.kernel.model.constraints.Constraint;
import choco.kernel.model.variables.integer.IntegerVariable;
import choco.kernel.model.variables.set.SetVariable;

/**
 *
 * @author jimmy
 */
public class ZeroOutManager {

    /**
     * x not in s => i[x] = 0
     */
    public static Constraint zeroOut(SetVariable s, IntegerVariable[] i) {
        Constraint[] cs = new Constraint[i.length];
        for (int x = 0; x < i.length; x++) {
            cs[x] = implies(notMember(x, s), eq(i[x], 0));
        }
        return and(cs);
    }
}
