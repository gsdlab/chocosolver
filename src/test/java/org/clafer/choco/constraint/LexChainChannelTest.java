package org.clafer.choco.constraint;

import gnu.trove.set.hash.TIntHashSet;
import java.util.Arrays;
import org.chocosolver.solver.Model;
import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.test.NonEmpty;
import org.clafer.test.SameLength;
import static org.junit.Assert.*;
import static org.junit.Assume.assumeTrue;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class LexChainChannelTest {

    @Input(solutions = 729)
    public Object testLexChainChannel(Model model) {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     string0 <- sequence $ replicate 2 [0..2]
         *     string1 <- sequence $ replicate 2 [0..2]
         *     string2 <- sequence $ replicate 2 [0..2]
         *     int0 <- [0..2]
         *     int1 <- [0..2]
         *     int2 <- [0..2]
         *
         *     guard $ sort (nub [int0, int1, int2]) `isPrefixOf` [0,1,2]
         *
         *     guard $ compare string0 string1 == compare int0 int1
         *     guard $ compare string0 string2 == compare int0 int2
         *     guard $ compare string1 string2 == compare int1 int2
         *
         *     return (string0, string1, string2, int0, int1, int2)
         */
        IntVar[][] strings = new IntVar[3][];
        for (int i = 0; i < strings.length; i++) {
            strings[i] = model.intVarArray("string" + i, 2, 0, 2);
        }
        IntVar[] ints = model.intVarArray("int", 3, 0, 2);
        return $(strings, ints);
    }

    @Check
    public void check(int[][] strings, int[] ints) {
        for (int i = 0; i < strings.length; i++) {
            for (int j = i + 1; j < strings.length; j++) {
                assertEquals(compare(strings[i], strings[j]), compare(ints[i], ints[j]));
            }
        }
        int[] sorted = new TIntHashSet(ints).toArray();
        Arrays.sort(sorted);
        for (int i = 0; i < sorted.length; i++) {
            assertEquals(i, sorted[i]);
        }
    }

    private static int compare(int[] x, int[] y) {
        for (int i = 0; i < x.length; i++) {
            if (x[i] < y[i]) {
                return -1;
            }
            if (x[i] > y[i]) {
                return 1;
            }
        }
        return 0;
    }

    private static int compare(int x, int y) {
        return (x < y) ? -1 : ((x == y) ? 0 : 1);
    }

    @Test(timeout = 60000)
    public Constraint setup(@NonEmpty @SameLength IntVar[][] strings, IntVar[] ints) {
        assumeTrue(strings.length == ints.length);
        return Constraints.lexChainChannel(strings, ints);
    }
}
