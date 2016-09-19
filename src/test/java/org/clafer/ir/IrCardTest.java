package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.card;
import static org.clafer.ir.Irs.equal;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrCardTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrIntVar card, IrSetVar set) {
        return equal(card, card(set));
    }

    @Solution
    public Constraint setup(IntVar card, SetVar set) {
        return card.getModel().arithm(card, "=", set.getCard());
    }
}
