package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.env;
import static org.chocosolver.solver.variables.Var.ker;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.common.Util;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SetSumTest {

    @Input(solutions = 32)
    public Object testSumSet(Model model) {
        /*
         * import Control.Monad
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
        SetVar set = model.setVar("set", ker(), env(-4, -3, -2, -1, 0, 1, 2, 3));
        IntVar card = model.intVar("|set|", 0, 2);
        set.setCard(card);
        IntVar sum = model.intVar("sum", -4, 4);
        return $(set, sum);
    }

    @Input(solutions = 14)
    public Object testSumNonPositiveSet(Model model) {
        /*
         * import Control.Monad
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
        SetVar set = model.setVar("set", ker(), env(-5, -4, -3, -2, -1, 0));
        IntVar card = model.intVar("|set|", 0, 4);
        set.setCard(card);
        IntVar sum = model.intVar("sum", -4, 2);
        return $(set, sum);
    }

    @Input(solutions = 3)
    public Object testSumKnown(Model model) {
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
        SetVar set = model.setVar("set", ker(), env(-1, 0, 1, 2, 3));
        IntVar card = model.intVar("|set|", 1, 2);
        set.setCard(card);
        IntVar sum = model.intVar("sum", 2, 2);
        return $(set, sum);
    }

    @Check
    public void check(int[] set, int sum) {
        assertEquals(Util.sum(set), sum);
    }

    @Test(timeout = 60000)
    public Constraint setup(SetVar set, IntVar sum) {
        return Constraints.setSum(set, set.getCard(), sum);
    }
}
