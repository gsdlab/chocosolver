package org.clafer.choco.constraint;

import gnu.trove.map.hash.TIntIntHashMap;
import gnu.trove.set.TIntSet;
import org.chocosolver.solver.Model;
import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.common.Util;
import org.clafer.test.NonEmpty;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import static org.chocosolver.solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class ArrayToSetTest {

    @Input(solutions = 216)
    public Object testArrayToSet(Model model) {
        /*
         * solutions = sequence $ replicate 3 [0..5]
         */
        return $(model.intVarArray("array", 3, 0, 5),
                model.setVar("set", new int[0], Util.range(0, 5)),
                -1);
    }

    @Input(solutions = 120)
    public Object testArrayToSetWithGlobalCardinality(Model model) {
        /*
         * isUnique [] = True
         * isUnique (x : xs) = x `notElem` xs && isUnique xs
         *
         * solutions =  filter isUnique $ sequence $ replicate 3 [0..5]
         */
        return $(model.intVarArray("array", 3, 0, 5),
                model.setVar("set", ker(), env(0, 1, 2, 3, 4, 5)),
                1);
    }

    @Input(solutions = 0)
    public Object testSetTooSmall(Model model) {
        IntVar i1 = model.intVar("i1", 0, 1);
        IntVar i2 = model.intVar("i2", 2, 3);
        SetVar set = model.setVar("set", ker(), env(0, 1, 2, 3));
        set.setCard(model.intVar("|set|", 0, 1));
        return $(new IntVar[]{i1, i2}, set, 1);
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
    public Constraint setup(@NonEmpty IntVar[] array, SetVar set, int globalCardinality) {
        return globalCardinality > 0
                ? Constraints.arrayToSet(array, set, set.getCard(), globalCardinality)
                : Constraints.arrayToSet(array, set, set.getCard());
    }
}
