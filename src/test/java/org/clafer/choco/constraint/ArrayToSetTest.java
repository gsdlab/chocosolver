package org.clafer.choco.constraint;

import gnu.trove.map.hash.TIntIntHashMap;
import gnu.trove.set.TIntSet;
import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.common.Util;
import org.clafer.test.NonEmpty;
import solver.variables.CSetVar;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.IntVar;
import static solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class ArrayToSetTest {

    @Input(solutions = 216)
    public Object testArrayToSet(Solver solver) {
        /*
         * solutions = sequence $ replicate 3 [0..5]
         */
        return $(enumeratedArray("array", 3, 0, 5, solver),
                cset("set", 0, 5, solver),
                -1);
    }

    @Input(solutions = 120)
    public Object testArrayToSetWithGlobalCardinality(Solver solver) {
        /*
         * isUnique [] = True
         * isUnique (x : xs) = x `notElem` xs && isUnique xs
         * 
         * solutions =  filter isUnique $ sequence $ replicate 3 [0..5]
         */
        return $(enumeratedArray("array", 3, 0, 5, solver),
                cset("set", 0, 5, solver),
                1);
    }

    @Input(solutions = 0)
    public Object testSetTooSmall(Solver solver) {
        return $(new IntVar[]{enumerated("i1", 0, 1, solver), enumerated("i2", 2, 3, solver)},
                cset("set", env(0, 1, 2, 3), ker(), card(0, 1), solver),
                1);
    }

    @Check
    public void check(int[] array, TIntSet set, int globalCardinality) {
        TIntIntHashMap count = new TIntIntHashMap();
        for (int i = 0; i < array.length; i++) {
            count.adjustOrPutValue(array[i], 1, 1);
        }
        assertEquals(count.keySet(), set);
        if (globalCardinality > 0 && count.size() > 0) {
            assertTrue(Util.max(count.valueCollection().iterator()) <= globalCardinality);
        }
    }

    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty IntVar[] array, CSetVar set, int globalCardinality) {
        return globalCardinality > 0
                ? Constraints.arrayToSet(array, set.getSet(), set.getCard(), globalCardinality)
                : Constraints.arrayToSet(array, set.getSet(), set.getCard());
    }
}
