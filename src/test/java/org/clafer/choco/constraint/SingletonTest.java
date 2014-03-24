package org.clafer.choco.constraint;

import org.clafer.collection.Pair;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;

/**
 *
 * @author jimmy
 */
public class SingletonTest extends ConstraintTest<Pair<IntVar, SetVar>> {

    @Override
    protected void check(Pair<IntVar, SetVar> s) {
        assertEquals(1, s.getSnd().getValue().length);
        assertEquals(s.getFst().getValue(), s.getSnd().getValue()[0]);
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Pair<IntVar, SetVar>>() {
            @Override
            public Pair<Constraint, Pair<IntVar, SetVar>> setup(Solver solver) {
                IntVar i = toVar(randInt(), solver);
                SetVar s = toSetVar(randSet(), solver);
                return pair(Constraints.singleton(i, s), pair(i, s));
            }
        });
    }

    @Test(timeout = 60000)
    public void testSingleton() {
        randomizedTest(new TestCase<Pair<IntVar, SetVar>>() {
            /*
             * import Control.Monad
             *
             * powerset = filterM (const [True, False])
             *
             * positive = do
             *     i <- [-3..2]
             *     s <- powerset [-2..3]
             *     guard $ [i] == s
             *     return (i, s)
             *   
             * negative = do
             *     i <- [-3..2]
             *     s <- powerset [-2..3]
             *     guard $ [i] /= s
             *     return (i, s)
             */
            @PositiveSolutions(5)
            @NegativeSolutions(379)
            @Override
            public Pair<Constraint, Pair<IntVar, SetVar>> setup(Solver solver) {
                IntVar i = VF.enumerated("i", -3, 2, solver);
                SetVar s = VF.set("s", -2, 3, solver);
                return pair(Constraints.singleton(i, s), pair(i, s));
            }
        });
    }
}
