package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.setArray;
import static org.chocosolver.solver.variables.VariableFactory.enumerated;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.choco.constraint.propagator.PropContinuousUnion;
import org.clafer.test.NonEmpty;
import org.clafer.test.Positive;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class ContinuousUnionTest {

    @Input(solutions = 400)
    public Object testContinuousUnion(Solver s) {
        /*
         * import Control.Monad
         * import Data.List
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     sets <- replicateM 3 $ powerset [0..2]
         *     totalCard <- [0..3]
         *     let union = sort $ nub $ concat sets
         *     guard $ [0..totalCard-1] == union
         *     return (totalCard, sets)
         */
        return $(setArray("set", 3, 0, 2, s), enumerated("totalCard", 0, 3, s));
    }

    @Check
    public void check(TIntSet[] sets, int totalCard) {
        TIntSet union = new TIntHashSet();
        for (TIntSet set : sets) {
            union.addAll(set);
        }
        assertEquals(union.size(), totalCard);
        for (int i = 0; i < totalCard; i++) {
            assertTrue(union.contains(i));
        }
    }

    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty SetVar[] sets, @Positive IntVar totalCard) {
        return new Constraint("ContinuousUnion", new PropContinuousUnion(sets, totalCard));
    }
}
