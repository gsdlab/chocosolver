package org.clafer.choco.constraint;

import gnu.trove.set.TIntSet;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
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
public class SubsetEqTest {

    @Input(solutions = 54)
    public Object testSubsetEq(Model model) {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * solutions = do
         *     s1 <- powerset [-1..2]
         *     s2 <- powerset [-2..1]
         *     guard $ s1 == s2
         *     return (s1, s2)
         */
        return $(model.setVar("subset", ker(), env(-1, 0, 1, 2)),
                model.setVar("superset", ker(), env(-2, -1, 0, 1)));
    }

    @Input(solutions = 2)
    public Object testPruneSupsetDueToCard(Model model) {
        SetVar subset = model.setVar("subset", ker(), env(-1, 0));
        subset.setCard(model.intVar(1));
        // supset cannot contain 1.
        SetVar supset = model.setVar("supset", ker(-2), env(-2, -1, 0, 1));
        supset.setCard(model.intVar(2));
        return $(subset, supset);
    }

    @Input(solutions = 2)
    public Object testPruneSubsetCardDueToLimitedOverlap(Model model) {
        SetVar subset = model.setVar("subset", ker(-4), env(-4, -3, 4));
        // subset cannot have cardinality 3.
        subset.setCard(model.intVar("|subset|", dom(2, 3)));
        SetVar supset = model.setVar("supset", ker(-4, 1), env(-4, -3, 1, 4));
        supset.setCard(model.intVar(3));
        return $(subset, supset);
    }

    @Input(solutions = 2)
    public Object testPruneSupsetCardDueToLimitedOverlap(Model model) {
        SetVar subset = model.setVar("subset", ker(0), env(-2, -1, 0));
        subset.setCard(model.intVar(2));
        SetVar supset = model.setVar("supset", ker(-3, 0), env(-3, -2, -1, 0));
        // supset cannot have cardinality 2.
        supset.setCard(model.intVar("|supset|", dom(2, 3)));
        return $(subset, supset);
    }

    @Check
    public void check(TIntSet subset, TIntSet supset) {
        assertTrue(supset.containsAll(subset));
    }

    @ArcConsistent(entailed = true)
    @Test(timeout = 60000)
    public Constraint setup(SetVar subset, SetVar supset) {
        return Constraints.subsetEq(subset, subset.getCard(), supset, supset.getCard());
    }
}
