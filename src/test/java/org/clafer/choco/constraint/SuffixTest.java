package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import static org.clafer.choco.constraint.ConstraintQuickTest.*;
import org.chocosolver.solver.variables.CStringVar;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.chocosolver.solver.constraints.Constraint;
import static org.chocosolver.solver.variables.Var.*;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SuffixTest {

    @Input(solutions = 142)
    public Object testSuffix(Model model) {
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
        return $(cstring("suffix", chars(0, 1, 2, 3), 3, model),
                cstring("word", chars(0, 1, 2, 3), 3, model));
    }

    @Check
    public void check(String suffix, String word) {
        assertThat(word, endsWith(suffix));
    }

    @Test(timeout = 60000)
    public Constraint setup(CStringVar suffix, CStringVar word) {
        return Constraints.suffix(
                suffix.getChars(), suffix.getLength(),
                word.getChars(), word.getLength());
    }
}
