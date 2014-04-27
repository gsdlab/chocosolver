package org.clafer.choco.constraint;

import gnu.trove.map.hash.TIntIntHashMap;
import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.common.Util;
import org.clafer.test.Positive;
import org.clafer.test.TestUtil.CSetVar;
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
public class JoinFunctionTest {

    @Input(solutions = 512)
    public Object testJoinFunction(Solver solver) {
        /*
         * solutions = 2^3*4^3
         */
        return $(cset("take", 0, 2, solver),
                new IntVar[]{
                    enumerated("ref[0]", 0, 3, solver),
                    enumerated("ref[1]", 0, 3, solver),
                    enumerated("ref[2]", 0, 3, solver)
                },
                cset("to", 0, 3, solver),
                -1);
    }

    @Input(solutions = 168)
    public Object testJoinFunctionWithGlobalUniqueness(Solver solver) {
        /*
         * import Control.Monad
         *
         * powerset = filterM (const [True, False])
         *
         * isUnique [] = True
         * isUnique (x : xs) = x `notElem` xs && isUnique xs
         *
         * solutions = do
         *     from <- powerset [0..2]
         *     refs <- sequence $ replicate 3 [0..2]
         *     let to = map (refs !!) from
         *     guard $ isUnique to
         *     return (from, refs, to)
         */
        return $(cset("take", 0, 2, solver),
                new IntVar[]{
                    enumerated("ref[0]", 0, 2, solver),
                    enumerated("ref[1]", 0, 2, solver),
                    enumerated("ref[2]", 0, 2, solver)
                },
                cset("to", 0, 2, solver),
                1);
    }

    @Input(solutions = 213)
    public Object testJoinFunctionWithGlobalCardinality(Solver solver) {
        /*
         * import Control.Monad
         * import Data.List
         *
         * powerset = filterM (const [True, False])
         *
         * is2Unique = all ((<= 2) . length) . group . sort 
         *
         * solutions = do
         *     from <- powerset [0..2]
         *     refs <- sequence $ replicate 3 [0..2]
         *     let to = map (refs !!) from
         *     guard $ is2Unique to
         *     return (from, refs, to)
         */
        return $(cset("take", 0, 2, solver),
                new IntVar[]{
                    enumerated("ref[0]", 0, 2, solver),
                    enumerated("ref[1]", 0, 2, solver),
                    enumerated("ref[2]", 0, 2, solver)
                },
                cset("to", 0, 2, solver),
                2
        );
    }

    @Input(solutions = 4)
    public Object testJoinFunctionFixedRefs(Solver solver) {
        return $(cset("take", 0, 1, solver),
                new IntVar[]{
                    fixed(5, solver),
                    fixed(5, solver),
                    fixed(5, solver)
                },
                cset("to", 4, 5, solver),
                -1);
    }

    @Check
    public void check(int[] take, int[] refs, TIntSet to, int globalCardinality) {
        TIntIntHashMap count = new TIntIntHashMap();
        for (int i : take) {
            assertTrue(i >= 0 && i < refs.length);
            count.adjustOrPutValue(refs[i], 1, 1);
        }
        assertEquals(count.keySet(), new TIntHashSet(to));
        if (globalCardinality > 0 && count.size() > 0) {
            assertTrue(Util.max(count.valueCollection().iterator()) <= globalCardinality);
        }
    }

    @Test(timeout = 60000)
    public Constraint setup(@Positive CSetVar take, IntVar[] refs, CSetVar to, int globalCardinality) {
        return globalCardinality > 0
                ? Constraints.joinFunction(
                        take.getSet(), take.getCard(),
                        refs,
                        to.getSet(), to.getCard(), globalCardinality)
                : Constraints.joinFunction(
                        take.getSet(), take.getCard(),
                        refs,
                        to.getSet(), to.getCard());
    }
}
