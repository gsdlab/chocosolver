package org.clafer.choco.constraint;

import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.CStringVar;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.VF;
import static org.chocosolver.solver.variables.Var.*;
import org.clafer.Sample;
import static org.clafer.choco.constraint.ConstraintQuickTest.$;
import org.clafer.choco.constraint.ConstraintQuickTest.Check;
import org.clafer.choco.constraint.ConstraintQuickTest.Input;
import org.clafer.test.Positive;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 *
 * @author jimmy
 */
@RunWith(ConstraintQuickTest.class)
public class SubstringTest {

    @Input(solutions = 239)
    public Object testSubstring(Solver solver) {
        return $(cstring("substring", dom(0, 1, 2), 2, solver),
                enumerated("index", 0, 2, solver),
                cstring("supstring", dom(0, 1, 2), 4, solver));
    }

    @Check
    public void check(String substring, int index, String superstring) {
        assertTrue(index + substring.length() <= superstring.length());
        assertEquals(substring, superstring.substring(index, index + substring.length()));
    }

    @Test(timeout = 60000)
    public Constraint setup(CStringVar substring, @Positive IntVar index, CStringVar superstring) {
        return Constraints.substring(substring.getChars(), substring.getLength(), index, superstring.getChars(), superstring.getLength());
    }

    public static void main(String[] args) {
        Solver s = new Solver();
        IntVar[] substring = new IntVar[]{
            VF.fixed(98, s),
            VF.fixed(98, s),};
        IntVar substringLength = VF.fixed(2, s);
        IntVar index = VF.one(s);
        IntVar[] supstring = new IntVar[]{
            VF.fixed(97, s),
            VF.fixed(98, s),
            VF.fixed(98, s),
            VF.fixed(0, s),};
        IntVar supstringLength = VF.fixed(3, s);
        Constraint c = Constraints.substring(substring, substringLength, index, supstring, supstringLength);
        System.out.println(c.isSatisfied());
    }
}
