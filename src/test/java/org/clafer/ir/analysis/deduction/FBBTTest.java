package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domains;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import static org.clafer.ir.Irs.bool;
import static org.clafer.ir.Irs.boundInt;
import static org.clafer.ir.Irs.constant;
import static org.clafer.ir.Irs.equal;
import static org.clafer.ir.Irs.implies;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class FBBTTest {

    @Test
    public void testCoalesceIntConstant() {
        IrModule module = new IrModule();
        IrIntVar var = boundInt("var", 0, 10);
        module.addConstraint(equal(var, constant(3)));

        IrIntVar coalesced = new FBBT().propagate(module).getFst().get(var);
        assertTrue(coalesced.isConstant());
        assertEquals(3, coalesced.getLowBound());
    }

    @Test
    public void testConstructiveDisjunction() {
        IrModule module = new IrModule();
        IrBoolVar b = bool("b");
        IrIntVar var = boundInt("var", 0, 10);
        module.addConstraint(implies(b, equal(var, 5)));
        module.addConstraint(implies(b.negate(), equal(var, 4)));

        Coalesce coalesce = new FBBT().constructiveDisjunction(b, b.negate(), module).getFst();

        assertEquals(Domains.enumDomain(4, 5), coalesce.get(var).getDomain());
    }
}
