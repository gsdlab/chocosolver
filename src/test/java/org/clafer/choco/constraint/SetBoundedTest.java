package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.*;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.choco.constraint.propagator.PropSetBounded;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SetBoundedTest {

    @Input(solutions = 98)
    public Object testSetBounded(Model model) {
        // import Control.Monad
        //
        // powerset = filterM (const [True, False])
        //
        // solutions = do
        //     from <- [0..3]
        //     to <- [0..3]
        //     guard $ from <= to
        //     s <- powerset [0..3]
        //     guard $ all (`elem` s) [from .. to-1]
        //     return (from, to, s)
        return $(model.intVar("from", 0, 3),
                model.intVar("to", 0, 3),
                model.setVar("s", ker(), env(0, 1, 2, 3)));
    }

    @Check
    public void check(int from, int to, TIntSet s) {
        assertTrue(from <= to);
        for (int i = from; i < to; i++) {
            assertTrue(s.contains(i));
        }
    }

    @Test(timeout = 60000)
    public Constraint setup(IntVar from, IntVar to, SetVar s) {
        return new Constraint("setBounded", new PropSetBounded(from, to, s));
    }
}
