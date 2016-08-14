package org.clafer.choco.constraint;

import gnu.trove.map.hash.TIntIntHashMap;
import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import org.chocosolver.solver.Model;
import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.common.Util;
import org.clafer.test.Positive;
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
public class JoinFunctionTest {

    @Input(solutions = 512)
    public Object testJoinFunction(Model model) {
        /*
         * solutions = 2^3*4^3
         */
        return $(model.setVar("take", ker(), env(0, 1, 2)),
                model.intVarArray("ref", 3, 0, 3),
                model.setVar("to", ker(), env(0, 1, 2, 3)),
                -1);
    }

    @Input(solutions = 168)
    public Object testJoinFunctionWithGlobalUniqueness(Model model) {
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
        return $(model.setVar("take", ker(), env(0, 1, 2)),
                model.intVarArray("ref", 3, 0, 2),
                model.setVar("to", ker(), env(0, 1, 2)),
                1);
    }

    @Input(solutions = 213)
    public Object testJoinFunctionWithGlobalCardinality(Model model) {
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
        return $(model.setVar("take", ker(), env(0, 1, 2)),
                model.intVarArray("ref", 3, 0, 2),
                model.setVar("to", ker(), env(0, 1, 2)),
                2);
    }

    @Input(solutions = 4)
    public Object testJoinFunctionFixedRefs(Model model) {
        return $(model.setVar("take", ker(), env(0, 1)),
                new IntVar[]{
                    model.intVar(5),
                    model.intVar(5),
                    model.intVar(5)
                },
                model.setVar("to", ker(), env(4, 5)),
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
    public Constraint setup(@Positive SetVar take, IntVar[] refs, SetVar to, int globalCardinality) {
        return globalCardinality > 0
                ? Constraints.joinFunction(
                        take, take.getCard(),
                        refs,
                        to, to.getCard(), globalCardinality)
                : Constraints.joinFunction(
                        take, take.getCard(),
                        refs,
                        to, to.getCard());
    }
}
