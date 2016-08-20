package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.arrayToSet;
import static org.clafer.ir.Irs.equal;
import org.clafer.test.NonEmpty;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrArrayToSetTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(@NonEmpty IrIntVar[] array, int globalCardinality, IrSetVar set) {
        return equal(arrayToSet(array, null), set);
    }

    @Solution
    public Constraint setup(IntVar[] array, int globalCardinality, SetVar set) {
        return Constraints.arrayToSet(array, set, set.getCard(), null);
    }
}
