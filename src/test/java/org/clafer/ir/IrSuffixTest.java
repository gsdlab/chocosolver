package org.clafer.ir;

import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.CStringVar;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrSuffixTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrStringVar suffix, IrStringVar word) {
        return suffix(suffix, word);
    }

    @Solution
    public Constraint setup(CStringVar suffix, CStringVar word) {
        return Constraints.suffix(
                suffix.getChars(), suffix.getLength(),
                word.getChars(), word.getLength());
    }
}
