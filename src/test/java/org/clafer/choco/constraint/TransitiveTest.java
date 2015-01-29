package org.clafer.choco.constraint;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.TIntSet;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.test.NonEmpty;
import org.clafer.test.Positive;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.VariableFactory.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class TransitiveTest {

    @Input(solutions = 3994)
    public Object testTransitive(Solver solver) {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * positive = do
         *     relation <- replicateM 4 $ powerset [0..3]
         *     guard $ and $ do
         *         [i,j,k] <- replicateM 3 $ [0..3]
         *         return $ ((j `elem` relation !! i) && (k `elem` relation !! j)) `implies` (k `elem` relation !! i)
         *     return relation
         *     where
         *         implies True False = False
         *         implies _ _ = True
         */
        SetVar[] relation = new SetVar[4];
        for (int i = 0; i < relation.length; i++) {
            relation[i] = set("relation[" + i + "]", 0, relation.length - 1, solver);
        }
        return $(relation);
    }

    @Check
    public void check(TIntSet[] relation) {
        for (TIntSet set : relation) {
            TIntIterator iter = set.iterator();
            while (iter.hasNext()) {
                int val = iter.next();
                assertTrue(val >= 0 && val < relation.length);
                assertTrue(set.containsAll(relation[val]));
            }
        }
    }

    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty @Positive SetVar[] relation) {
        return Constraints.transitive(relation);
    }
}
