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
import org.clafer.choco.constraint.propagator.PropIntMemberSetDefault;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class MemberDefaultTest {

    @Input(solutions = 129)
    public Object testMemberDefault(Model model) {
        return $(model.intVar("element", -1, 3),
                model.setVar("set", ker(), env(0, 1, 2, 3, 4, 5)),
                0);
    }

    @Input(solutions = 12)
    public Object testTautology(Model model) {
        return $(model.intVar("element", dom(1, 2, 4)),
                model.setVar("set", ker(1, 2, 4), env(0, 1, 2, 3, 4)),
                0);
    }

    @Input(solutions = 0)
    public Object testFalseTautology(Model model) {
        IntVar element = model.intVar("element", 0);
        SetVar set = model.setVar("set", ker(), env(1, 2, 3, 4, 5));
        set.setCard(model.intVar("|set|", 1, 2));
        return $(element, set, 1);
    }

    @Check
    public void check(int element, TIntSet set, int defaultValue) {
        if (set.isEmpty()) {
            assertEquals(defaultValue, element);
        } else {
            assertTrue(set.contains(element));
        }
    }

    @ArcConsistent
    @Test(timeout = 60000)
    public Constraint setup(IntVar element, SetVar set, int defaultValue) {
        return new Constraint("memberSupport", new PropIntMemberSetDefault(element, set, set.getCard(), defaultValue));
    }
}
