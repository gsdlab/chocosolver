package org.clafer.ir.analysis;

import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.Irs;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class LinearEquationOptimizerTest {

    @Test
    public void testNotOptimizable() {
        IrModule module = new IrModule();
        IrIntVar i = Irs.boundInt("i", -3, 4);
        IrIntVar j = Irs.boundInt("j", -3, 4);
        module.addConstraint(Irs.lessThan(i, Irs.add(j, 1)));

        module = LinearEquationOptimizer.optimize(module);

        assertEquals(1, module.getConstraints().size());
    }

    @Test
    public void testEqualitySubsumesInequality() {
        IrModule module = new IrModule();
        IrIntVar i = Irs.boundInt("i", -3, 4);
        IrIntVar j = Irs.boundInt("j", -3, 4);
        module.addConstraint(Irs.equal(i, Irs.add(j, 1)));
        module.addConstraint(Irs.lessThanEqual(i, Irs.add(j, 2)));

        module = LinearEquationOptimizer.optimize(module);

        assertEquals(1, module.getConstraints().size());
    }
}
