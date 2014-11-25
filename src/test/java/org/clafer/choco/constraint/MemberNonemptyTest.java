package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.CSetVar;
import solver.variables.IntVar;
import static solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class MemberNonemptyTest {

    @Input(solutions = 133)
    public Object testMemberNonempty(Solver solver) {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     i <- [-1..3]
         *     s <- powerset [0..5]
         *     guard $ i `notElem` s
         *     return (i, s)
         */
        return $(enumerated("element", -1, 3, solver),
                cset("set", 0, 5, solver));
    }

    @Input(solutions = 12)
    public Object testTautology(Solver solver) {
        return $(enumerated("element", dom(1, 2, 4), solver),
                cset("set", env(0, 1, 2, 3, 4), ker(1, 2, 4), solver));
    }

    @Input(solutions = 0)
    public Object testFalseTautology(Solver solver) {
        return $(enumerated("element", 6, 8, solver),
                cset("set", env(0, 1, 2, 3, 4, 5), ker(), card(1, 2), solver));
    }

    @Check
    public void check(int element, TIntSet set) {
        assertTrue(set.isEmpty() || set.contains(element));
    }

    @Test(timeout = 60000)
    public Constraint setup(IntVar element, CSetVar set) {
        return Constraints.memberNonempty(element, set.getSet(), set.getCard());
    }
}
