package org.clafer.ir;

import static org.clafer.ir.Irs.array;
import static org.clafer.ir.Irs.constant;
import static org.clafer.ir.Irs.subarray;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

/**
 * @author Michał Antkiewicz
 */
public class IrsTest {

    /**
     * A constant subarray that runs past the end of the array is padded with
     * -1, as in Constraints.subarray, instead of reading past the end.
     */
    @Test
    public void testConstantSubarrayPastTheEnd() {
        IrIntArrayExpr sub = subarray(array(constant(1), constant(2), constant(3)), constant(2), constant(3));

        assertTrue(sub instanceof IrIntArrayVar);
        IrIntExpr[] elements = ((IrIntArrayVar) sub).getArray();
        assertEquals(3, elements.length);
        assertEquals(constant(3), elements[0]);
        assertEquals(constant(-1), elements[1]);
        assertEquals(constant(-1), elements[2]);
    }
}
