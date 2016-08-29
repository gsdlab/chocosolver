package org.clafer.choco.constraint;

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
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SetMinTest {

    @Input(solutions = 37)
    public Object testSetMin(Model model) {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     set <- powerset [-4..3]
         *     guard $ length set <= 2
         *     min <- [-4..4]
         *     guard $ if null set then min == 0 else || min == minimum set
         *     return (set, min)
         */
        SetVar set = model.setVar("set", ker(), env(-4, -3, -2, -1, 0, 1, 2, 3));
        IntVar card = model.intVar("|set|", 0, 2);
        set.setCard(card);
        IntVar min = model.intVar("min", -4, 4);
        return $(set, min);
    }

    @Input(solutions = 2)
    public Object testExtremalMin(Model model) {
        // set can never contain 1, 2, nor 3 since none of those are the default
        // value nor does min contain those values.
        SetVar set = model.setVar("set", ker(), env(1, 2, 3, 4));
        set.setCard(model.intVar("|set|", dom(0, 1, 2)));
        IntVar min = model.intVar("min", dom(0, 4));
        return $(set, min);
    }

    @Input(solutions = 2)
    public Object testImpossibleEnvs(Model model) {
        // set can never contain 4 due to its cardinality.
        SetVar set = model.setVar("set", ker(), env(2, 3, 4));
        set.setCard(model.intVar("|set|", 2));
        IntVar min = model.intVar("min", dom(2, 4));
        return $(set, min);
    }

    @Input(solutions = 2)
    public Object testOneChoiceLeft(Model model) {
        SetVar set = model.setVar("set", ker(2), env(-1, 0, 2));
        // |set| cannot be 1 since 2 is already taken which does not overlap
        // with min.
        set.setCard(model.intVar("|set|", dom(1, 2)));
        IntVar min = model.intVar("min", dom(-1, 0));
        return $(set, min);
    }

    @Input(solutions = 4)
    public Object testOneChoiceLeftPruneEnv(Model model) {
        // set cannot contain -1 since that will become the minimum but not
        // contained in min.
        SetVar set = model.setVar("set", ker(4), env(-2, -1, 1, 4, 5));
        set.setCard(model.intVar("|set|", dom(1, 2)));
        IntVar min = model.intVar("min", dom(-2, 1, 4));
        return $(set, min);
    }

    @Input(solutions = 2)
    public Object testEntailedForAllChoices(Model model) {
        SetVar set = model.setVar("set", ker(4), env(1, 4));
        set.setCard(model.intVar("|set|", dom(1, 2)));
        IntVar min = model.intVar("min", dom(1, 4));
        return $(set, min);
    }

    @Input(solutions = 1)
    public Object testForceMin(Model model) {
        SetVar set = model.setVar("set", ker(0), env(-4, -3, 0, 2));
        set.setCard(model.intVar("|set|", dom(1, 3)));
        // min must be 0.
        IntVar min = model.intVar("min", dom(-2, 1, 0));
        return $(set, min);
    }

    @Input(solutions = 0)
    public Object testNoSolutionBecauseCard(Model model) {
        SetVar set = model.setVar("set", ker(), env(-3, -2, -1, 0, 1));
        set.setCard(model.intVar("|set|", dom(0, 5)));
        IntVar min = model.intVar("min", dom(-1, 3, 4));
        return $(set, min);
    }

    @Check
    public void check(int[] set, int min) {
        if (set.length > 0) {
            assertEquals(set[0], min);
        } else {
            assertEquals(0, min);
        }
    }

    @ArcConsistent
    @Test(timeout = 600000)
    public Constraint setup(SetVar set, IntVar min) {
        return Constraints.min(set, set.getCard(), min, 0);
    }
}
