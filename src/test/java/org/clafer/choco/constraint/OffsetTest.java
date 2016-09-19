package org.clafer.choco.constraint;

import gnu.trove.iterator.TIntIterator;
import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.SetVar;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.test.NoCard;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class OffsetTest {

    @Check
    public void check(TIntSet set1, TIntSet set2, int offset) {
        TIntSet offsetSet = new TIntHashSet();
        TIntIterator iter = set1.iterator();
        while (iter.hasNext()) {
            offsetSet.add(iter.next() + offset);
        }
        assertEquals(offsetSet, set2);
    }

    @ArcConsistent(entailed = true)
    @Test(timeout = 60000)
    public Constraint setup(@NoCard SetVar set1, @NoCard SetVar set2, int offset) {
        return set1.getModel().offSet(set1, set2, offset);
    }
}
