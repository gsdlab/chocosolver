package org.clafer.choco.constraint;

import org.clafer.choco.constraint.propagator.PropUtil;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import org.clafer.ir.Irs;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
public class ConcatTest extends ConstraintTest<Triple<IntVar[], IntVar[], IntVar[]>> {

    @Override
    protected void check(Triple<IntVar[], IntVar[], IntVar[]> s) {
        assertEquals(
                PropUtil.toString(s.getThd()),
                PropUtil.toString(s.getFst())
                + PropUtil.toString(s.getSnd()));
    }

    @Test(timeout = 60000)
    public void quickTest() {
        randomizedTest(new TestCase<Triple<IntVar[], IntVar[], IntVar[]>>() {
            @Override
            public Pair<Constraint, Triple<IntVar[], IntVar[], IntVar[]>> setup(Solver solver) {
                CStringVar left = toCStringVar(randString(), solver);
                CStringVar right = toCStringVar(randString(), solver);
                CStringVar concat = toCStringVar(randString(), solver);
                Constraint constraint = Constraints.concat(
                        left.getChars(), left.getLength(),
                        right.getChars(), right.getLength(),
                        concat.getChars(), concat.getLength());
                return pair(constraint, triple(
                        left.getChars(), right.getChars(), concat.getChars()));
            }
        });
    }

    @Test(timeout = 60000)
    public void testConcat() {
        randomizedTest(new TestCase<Triple<IntVar[], IntVar[], IntVar[]>>() {
            /*
             * import Control.Monad
             *
             * strings = [0..3] >>= flip replicateM ['a', 'b']
             * positive = do
             *     left <- strings
             *     right <- strings
             *     cat <- strings
             *     guard $ left ++ right == cat
             *     return (left, right, cat)
             * negative = length (replicateM 3 strings) - length positive
             */
            @PositiveSolutions(49)
            @NegativeSolutions(3326)
            @Override
            public Pair<Constraint, Triple<IntVar[], IntVar[], IntVar[]>> setup(Solver solver) {
                CStringVar left = toCStringVar(Irs.string("left",
                        Irs.boundDomain('a', 'b'), 3), solver);
                CStringVar right = toCStringVar(Irs.string("right",
                        Irs.boundDomain('a', 'b'), 3), solver);
                CStringVar concat = toCStringVar(Irs.string("concat",
                        Irs.boundDomain('a', 'b'),3), solver);
                Constraint constraint = Constraints.concat(
                        left.getChars(), left.getLength(),
                        right.getChars(), right.getLength(),
                        concat.getChars(), concat.getLength());
                return pair(constraint, triple(
                        left.getChars(), right.getChars(), concat.getChars()));
            }
        });
    }
}
