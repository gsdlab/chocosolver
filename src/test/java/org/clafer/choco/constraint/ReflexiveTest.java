package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import org.chocosolver.solver.Model;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.test.NonEmpty;
import org.clafer.test.Positive;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class ReflexiveTest {

    @Input(solutions = 4096)
    public Object testReflexive(Model model) {
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
        return $(model.setVarArray("relation", 4, ker(), env(0, 1, 2, 3)));
    }

    @Check
    public void check(TIntSet[] relation) {
        for (int i = 0; i < relation.length; i++) {
            assertTrue(relation[i].contains(i));
        }
    }

    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty @Positive SetVar[] relation) {
        return Constraints.reflexive(relation);
    }
}
