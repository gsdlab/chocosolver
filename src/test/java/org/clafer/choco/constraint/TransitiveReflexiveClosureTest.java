package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import org.chocosolver.solver.Model;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.test.NonEmpty;
import org.clafer.test.Positive;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.*;
import org.clafer.test.NoCard;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class TransitiveReflexiveClosureTest {

    private void transitiveReflexiveClosure(TIntSet[] relation, int c, TIntSet closure) {
        assertTrue(c >= 0 && c < relation.length);
        closure.add(c);
        relation[c].forEach(next -> {
            if (closure.add(next)) {
                transitiveReflexiveClosure(relation, next, closure);
            }
            return true;
        });
    }

    @Input(solutions = 512)
    public Object testTransitive(Model model) {
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
        return $(model.setVarArray("relation", 3, ker(), env(0, 1, 2)),
                model.setVarArray("closure", 3, ker(), env(0, 1, 2)));
    }

    @Check
    public void check(TIntSet[] relation, TIntSet[] closure) {
        for (int i = 0; i < relation.length; i++) {
            TIntSet c = new TIntHashSet(relation.length);
            transitiveReflexiveClosure(relation, i, c);
            assertTrue(c.equals(closure[i]));
        }
    }

    @Test(timeout = 100000)
    public Constraint setup(@NoCard @NonEmpty @Positive SetVar[] relation, @NoCard @NonEmpty @Positive SetVar[] closure) {
        assumeTrue(relation.length == closure.length);
        return Constraints.transitiveReflexiveClosure(relation, closure);
    }
}
