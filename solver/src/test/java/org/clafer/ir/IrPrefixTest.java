package org.clafer.ir;

import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.CStringVar;
import org.clafer.choco.constraint.Constraints;
import org.clafer.ir.IrQuickTest.Solution;
import static org.clafer.ir.Irs.prefix;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(IrQuickTest.class)
public class IrPrefixTest {

    @Test(timeout = 60000)
    public IrBoolExpr setup(IrStringVar prefix, IrStringVar word) {
        return prefix(prefix, word);
    }

    @Solution
    public Constraint setup(CStringVar prefix, CStringVar word) {
        return Constraints.prefix(
                prefix.getChars(), prefix.getLength(),
                word.getChars(), word.getLength());
    }
}
