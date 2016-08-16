package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.*;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.choco.constraint.propagator.PropContinuous;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class ContinuousTest {

    @Input(solutions = 11)
    public Object testContinuous(Model model) {
        return $(model.setVar("set", ker(), env(0, 1, 2, 3)));
    }

    @ConstraintQuickTest.Check
    public void check(int[] set) {
        for (int i = 0; i < set.length - 1; i++) {
            assertEquals(set[i] + 1, set[i + 1]);
        }
    }

    @Test(timeout = 60000)
    public Constraint setup(SetVar set) {
        return new Constraint("continuous", new PropContinuous(set, set.getCard()));
    }

    public static void main(String[] args) {
        Model model = new Model();
        SetVar set = model.setVar("set", ker(), env(0, 1, 2, 3));
        model.post(new Constraint("continuous", new PropContinuous(set, set.getCard())));
        model.getSolver().streamSolutions().forEach(x -> System.out.println(set.toString()));
    }
}
