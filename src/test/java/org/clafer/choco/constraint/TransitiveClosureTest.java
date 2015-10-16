package org.clafer.choco.constraint;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.test.NonEmpty;
import org.clafer.test.Positive;
import static org.junit.Assert.assertTrue;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.VariableFactory.set;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class TransitiveClosureTest {

    private void transitiveClosure(TIntSet[] relation, int c, TIntSet closure) {
        assertTrue(c >= 0 && c < relation.length);
        relation[c].forEach(next -> {
            if (closure.add(next)) {
                transitiveClosure(relation, next, closure);
            }
            return true;
        });
    }

    @Input(solutions = 512)
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
        SetVar[] relation = new SetVar[3];
        for (int i = 0; i < relation.length; i++) {
            relation[i] = set("relation[" + i + "]", 0, relation.length - 1, solver);
        }
        SetVar[] closure = new SetVar[relation.length];
        for (int i = 0; i < relation.length; i++) {
            closure[i] = set("closure[" + i + "]", 0, closure.length - 1, solver);
        }
        return $(relation, closure);
    }

    @Check
    public void check(TIntSet[] relation, TIntSet[] closure) {
        for (int i = 0; i < relation.length; i++) {
            TIntSet c = new TIntHashSet(relation.length);
            transitiveClosure(relation, i, c);
            assertTrue(c.equals(closure[i]));
        }
    }

    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty @Positive SetVar[] relation, @NonEmpty @Positive SetVar[] closure) {
        assumeTrue(relation.length == closure.length);
        return Constraints.transitiveClosure(relation, closure);
    }
}
