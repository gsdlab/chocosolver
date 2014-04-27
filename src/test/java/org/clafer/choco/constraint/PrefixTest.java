package org.clafer.choco.constraint;

import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.clafer.test.TestUtil.CStringVar;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import solver.Solver;
import solver.constraints.Constraint;
import static solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class PrefixTest {

    @Input(solutions = 142)
    public Object testPrefix(Solver solver) {
        /*
         * import Control.Monad
         *
         * solutions = do
         *     length <- [0..3]
         *     string1 <- replicateM 3 ['a', 'b', 'c']
         *     string2 <- replicateM 3 ['a', 'b', 'c']
         *     guard $ take length string1 == take length string2
         *     return (length, string1, string2)
         */
        return $(cstring("prefix", chars(0, 1, 2, 3), 3, solver),
                cstring("word", chars(0, 1, 2, 3), 3, solver));
    }

    @Check
    public void check(String prefix, String word) {
        assertThat(word, startsWith(prefix));
    }

    @Test(timeout = 60000)
    public Constraint setup(CStringVar prefix, CStringVar word) {
        return Constraints.prefix(
                prefix.getChars(), prefix.getLength(),
                word.getChars(), word.getLength());
    }
}
