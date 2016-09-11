package org.clafer.ir.analysis;

import org.clafer.domain.Domains;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.Irs;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
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

    @Test
    public void testCombineInequalties() {
        IrModule module = new IrModule();
        IrIntVar i0 = Irs.boundInt("i0", 0, 4);
        IrIntVar i1 = Irs.boundInt("i1", 0, 4);
        IrIntVar i2 = Irs.boundInt("i2", 0, 4);
        IrIntVar i3 = Irs.boundInt("i3", 0, 4);
        module.addConstraint(Irs.greaterThan(Irs.add(i0, i1, i2, i3), 0));
        module.addConstraint(Irs.greaterThanEqual(i0, i1));
        module.addConstraint(Irs.greaterThanEqual(i1, i2));
        module.addConstraint(Irs.greaterThanEqual(i2, i3));

        module = LinearEquationOptimizer.optimize(module);

        assertTrue(module.getConstraints().contains(
                Irs.lessThanEqual(Irs.One, Irs.mul(4, i0, Domains.boundDomain(0, 16)))));
    }
}
