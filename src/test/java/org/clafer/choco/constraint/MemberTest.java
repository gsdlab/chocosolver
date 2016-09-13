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
import org.clafer.test.NoCard;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class MemberTest {

    @Input(solutions = 128)
    public Object testMember(Model model) {
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
        return $(model.intVar("element", 6, 8),
                model.setVar("set", ker(), env(0, 1, 2, 3, 4, 5)));
    }

    @Check
    public void check(int element, TIntSet set) {
        assertTrue(set.contains(element));
    }

    @ArcConsistent(entailed = true, opposite = true)
    @Test(timeout = 60000)
    public Constraint setup(IntVar element, @NoCard SetVar set) {
        return Constraints.member(element, set);
    }
}
