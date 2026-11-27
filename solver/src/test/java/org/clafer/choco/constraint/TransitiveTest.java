package org.clafer.choco.constraint;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.TIntSet;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.env;
import static org.chocosolver.solver.variables.Var.ker;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.test.NoCard;
import org.clafer.test.NonEmpty;
import org.clafer.test.Positive;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class TransitiveTest {

    @Input(solutions = 171)
    public Object testTransitive(Model model) {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     relation <- replicateM 3 $ powerset [0..2]
         *     guard $ and $ do
         *         [i,j,k] <- replicateM 3 $ [0..2]
         *         return $ ((j `elem` relation !! i) && (k `elem` relation !! j)) `implies` (k `elem` relation !! i)
         *     return relation
         *     where
         *         implies True False = False
         *         implies _ _ = True
         */
        return $(model.setVarArray("relation", 3, ker(), env(0, 1, 2)));
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
    public Constraint setup(@NoCard @NonEmpty @Positive SetVar[] relation) {
        return Constraints.transitive(relation);
    }
}
