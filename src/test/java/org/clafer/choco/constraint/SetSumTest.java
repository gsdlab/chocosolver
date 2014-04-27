package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.common.Util;
import org.clafer.test.TestUtil.CSetVar;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.IntVar;
import static solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SetSumTest {

    @Input(solutions = 32)
    public Object testSumSet(Solver solver) {
        /*import Control.Monad
         *        
         * powerset = filterM (const [True, False])
         *        
         * solutions = do
         *     set <- powerset [-4..3]
         *     guard $ length set <= 2
         *     setSum <- [-4..4]
         *     guard $ sum set == setSum
         *     return set
         */
        return $(cset("set", env(-4, -3, -2, -1, 0, 1, 2, 3), ker(), card(0, 1, 2), solver),
                enumerated("sum", -4, 4, solver));
    }

    @Input(solutions = 14)
    public Object testSumNonPositiveSet(Solver solver) {
        /*import Control.Monad
         *        
         * powerset = filterM (const [True, False])
         *        
         * solutions = do
         *     set <- powerset [-5..0]
         *     guard $ length set <= 4
         *     setSum <- [-4..2]
         *     guard $ sum set == setSum
         *     return set
         *    
         */
        return $(cset("set", env(-5, -4, -3, -2, -1, 0), ker(), card(0, 1, 2, 3, 4), solver),
                enumerated("sum", -4, 2, solver));
    }

    @Input(solutions = 3)
    public Object testSumKnown(Solver solver) {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     set <- powerset [-1..3]
         *     guard $ length set >= 1 && length set <= 2
         *     guard $ sum set == 2
         *     return set
         */
        return $(cset("set", env(-1, 0, 1, 2, 3), ker(), card(1, 2), solver),
                enumerated("sum", 2, 2, solver));
    }

    @Check
    public void check(int[] set, int sum) {
        assertEquals(Util.sum(set), sum);
    }

    @Test(timeout = 60000)
    public Constraint setup(CSetVar set, IntVar sum) {
        return Constraints.setSum(set.getSet(), set.getCard(), sum);
    }
}
