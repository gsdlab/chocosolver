package org.clafer.ir.compiler.analysis;

import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrUtil;
import static org.clafer.ir.Irs.*;
import org.clafer.ir.analysis.Coalescer;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 *
 * @author jimmy
 */
public class CoalescerTest {

    @Test
    public void testCoalesceIntConstant() {
        IrModule module = new IrModule();
        IrIntVar var = boundInt("var", 0, 10);
        module.addConstraint(equal($(var), $(constant(3))));
        
        IrIntVar coalesced = Coalescer.coalesce(module).getSnd().get(var);
        assertEquals(Integer.valueOf(3), IrUtil.getConstant(coalesced));
    }
}
