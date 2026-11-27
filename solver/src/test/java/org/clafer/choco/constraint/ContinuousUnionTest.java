package org.clafer.choco.constraint;

import gnu.trove.iterator.TIntIterator;
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
import org.clafer.choco.constraint.propagator.PropContinuousUnion;
import org.clafer.test.NonEmpty;
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
public class ContinuousUnionTest {

    @Input(solutions = 40)
    public Object testContinuousUnion(Model model) {
        return $(model.setVarArray("s", 2, ker(), env(0, 1, 2)),
                model.intVar("totalCard", 0, 3));
    }

    @Check
    public void check(TIntSet[] sets, int totalCard) {
        TIntSet union = new TIntHashSet();
        for (TIntSet set : sets) {
            union.addAll(set);
        }
        assertEquals(totalCard, union.size());
        TIntIterator iter = union.iterator();
        while (iter.hasNext()) {
            int i = iter.next();
            assertTrue(i >= 0);
            assertTrue(i < totalCard);
        }
    }

    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty SetVar[] sets, @Positive IntVar totalCard) {
        return new Constraint("continuousUnion", new PropContinuousUnion(sets, totalCard));
    }
}
