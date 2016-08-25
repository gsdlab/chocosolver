package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.dom;
import static org.chocosolver.solver.variables.Var.env;
import static org.chocosolver.solver.variables.Var.ker;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class MemberNonemptyTest {

    @Input(solutions = 133)
    public Object testMemberNonempty(Model model) {
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
        return $(model.intVar("element", -1, 3),
                model.setVar("set", ker(), env(0, 1, 2, 3, 4, 5)));
    }

    @Input(solutions = 12)
    public Object testTautology(Model model) {
        return $(model.intVar("element", dom(1, 2, 4)),
                model.setVar("set", ker(1, 2, 4), env(0, 1, 2, 3, 4)));
    }

    @Input(solutions = 0)
    public Object testFalseTautology(Model model) {
        IntVar element = model.intVar("element", 6, 8);
        SetVar set = model.setVar("set", ker(), env(0, 1, 2, 3, 4, 5));
        set.setCard(model.intVar("|set|", 1, 2));
        return $(element, set);
    }

    @Check
    public void check(int element, TIntSet set) {
        assertTrue(set.isEmpty() || set.contains(element));
    }

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint setup(IntVar element, SetVar set) {
        return Constraints.memberNonempty(element, set, set.getCard());
    }
}
