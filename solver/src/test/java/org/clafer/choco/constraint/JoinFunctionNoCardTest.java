package org.clafer.choco.constraint;

import gnu.trove.map.hash.TIntIntHashMap;
import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.env;
import static org.chocosolver.solver.variables.Var.ker;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.choco.constraint.propagator.PropJoinFunction;
import org.clafer.test.NoCard;
import org.clafer.test.Positive;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class JoinFunctionNoCardTest {

    @Input(solutions = 512)
    public Object testJoinFunction(Model model) {
        /*
         * solutions = 2^3*4^3
         */
        return $(model.setVar("take", ker(), env(0, 1, 2)),
                model.intVarArray("ref", 3, 0, 3),
                model.setVar("to", ker(), env(0, 1, 2, 3)));
    }

    @Check
    public void check(int[] take, int[] refs, TIntSet to) {
        TIntIntHashMap count = new TIntIntHashMap();
        for (int i : take) {
            assertTrue(i >= 0 && i < refs.length);
            count.adjustOrPutValue(refs[i], 1, 1);
        }
        assertEquals(count.keySet(), new TIntHashSet(to));
    }

    @Test(timeout = 60000)
    public Constraint setup(@NoCard @Positive SetVar take, IntVar[] refs, @NoCard SetVar to) {
        return new Constraint("JoinFunction", new PropJoinFunction(take, refs, to));
    }
}
