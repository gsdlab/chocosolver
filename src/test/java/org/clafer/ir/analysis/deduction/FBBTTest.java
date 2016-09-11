package org.clafer.ir.analysis.deduction;

import org.clafer.domain.Domains;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrSetVar;
import static org.clafer.ir.Irs.add;
import static org.clafer.ir.Irs.bool;
import static org.clafer.ir.Irs.boundInt;
import static org.clafer.ir.Irs.constant;
import static org.clafer.ir.Irs.equal;
import static org.clafer.ir.Irs.greaterThan;
import static org.clafer.ir.Irs.implies;
import static org.clafer.ir.Irs.set;
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
    public void testCoalesceIntVars() {
        IrModule module = new IrModule();
        IrIntVar i1 = boundInt("i1", 0, 10);
        IrIntVar i2 = boundInt("i2", -5, 5);
        module.addConstraint(equal(i1, i2));

        Coalesce coalesce = new FBBT().propagate(module).getFst();
        IrIntVar coalesced = coalesce.get(i1);
        assertEquals(coalesced, coalesce.get(i2));
        assertEquals(Domains.boundDomain(0, 5), coalesced.getDomain());
    }

    @Test
    public void testCoalesceSetConstant() {
        IrModule module = new IrModule();
        IrSetVar var = set("s1", 0, 10);
        module.addConstraint(equal(var, constant(new int[]{3, 4, 6})));

        IrSetVar coalesced = new FBBT().propagate(module).getFst().get(var);
        assertTrue(coalesced.isConstant());
        assertEquals(Domains.enumDomain(3, 4, 6), coalesced.getEnv());
    }

    @Test
    public void testCoalesceSetVars() {
        IrModule module = new IrModule();
        IrSetVar s1 = set("s1", Domains.boundDomain(0, 10), Domains.enumDomain(1, 3), Domains.boundDomain(2, 5));
        IrSetVar s2 = set("s2", Domains.enumDomain(1, 3, 4, 5, 7, 9, 11), Domains.boundDomain(3, 5), Domains.enumDomain(5, 7));
        module.addConstraint(equal(s1, s2));

        Coalesce coalesce = new FBBT().propagate(module).getFst();
        IrSetVar coalesced = coalesce.get(s1);
        assertEquals(coalesced, coalesce.get(s2));
        assertEquals(Domains.enumDomain(1, 3, 4, 5), coalesced.getKer());
        assertEquals(Domains.enumDomain(1, 3, 4, 5, 7, 9), coalesced.getEnv());
        assertEquals(Domains.constantDomain(5), coalesced.getCard());
    }

    @Test
    public void testImplicationConstructiveDisjunction() {
        IrModule module = new IrModule();
        IrBoolVar b = bool("b");
        IrIntVar var = boundInt("var", 0, 10);
        module.addConstraint(implies(b, equal(var, 5)));
        module.addConstraint(implies(b.negate(), equal(var, 4)));

        Coalesce coalesce = new FBBT().constructiveDisjunction(b, b.negate(), module).getFst();

        assertEquals(Domains.enumDomain(4, 5), coalesce.get(var).getDomain());
    }

    @Test
    public void testAdditionConstructiveDisjunction() {
        IrModule module = new IrModule();
        IrIntVar i1 = boundInt("i1", 0, 10);
        IrIntVar i2 = boundInt("i2", 0, 10);
        IrIntVar sum = boundInt("sum", 0, 10);
        module.addConstraint(equal(add(i1, i2), sum));

        Coalesce coalesce = new FBBT().constructiveDisjunction(
                greaterThan(i1, 0), greaterThan(i2, 0), module).getFst();

        assertEquals(Domains.boundDomain(1, 10), coalesce.get(sum).getDomain());
    }
}
