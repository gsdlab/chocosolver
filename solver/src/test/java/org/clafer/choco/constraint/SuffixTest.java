package org.clafer.choco.constraint;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.CStringVar;
import static org.chocosolver.solver.variables.Var.chars;
import static org.chocosolver.solver.variables.Var.cstring;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import static org.hamcrest.CoreMatchers.endsWith;
import static org.junit.Assert.assertThat;
import org.junit.Test;
import org.junit.runner.RunWith;

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
