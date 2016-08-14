package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.SetVar;
import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.clafer.test.Positive;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrJoinRelationTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(@Positive IrSetVar take, IrSetVar[] children, boolean injective, IrSetVar join) {
        assumeTrue(take.getEnv().isEmpty() || take.getEnv().getHighBound() < children.length);
        return equal(joinRelation(take, Irs.array(children), false), join);
    }

    @Solution
    public Constraint setup(SetVar take, SetVar[] children, boolean injective, SetVar join) {
        return Constraints.joinRelation(take, children, join);
    }
}
