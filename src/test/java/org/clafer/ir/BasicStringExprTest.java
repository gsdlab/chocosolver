package org.clafer.ir;

import org.clafer.choco.constraint.Constraints;
import static org.clafer.ir.Irs.*;
import org.junit.Test;
import solver.constraints.Constraint;
import solver.constraints.ICF;
import solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
public class BasicStringExprTest extends IrTest {

    @Test(timeout = 60000)
    public void testEqual() {
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrStringVar var1, IrStringVar var2) {
                return equal(var1, var2);
            }

            Constraint setup(CStringVar var1, CStringVar var2) {
                return Constraints.equal(
                        var1.getChars(), var1.getLength(),
                        var2.getChars(), var2.getLength());
            }
        });
    }

    @Test(timeout = 60000)
    public void testNotEqual() {
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrStringVar var1, IrStringVar var2) {
                return notEqual(var1, var2);
            }

            Constraint setup(CStringVar var1, CStringVar var2) {
                return Constraints.notEqual(
                        var1.getChars(), var1.getLength(),
                        var2.getChars(), var2.getLength());
            }
        });
    }

    @Test(timeout = 60000)
    public void testLength() {
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar length, IrStringVar word) {
                return equal(length, length(word));
            }

            Constraint setup(IntVar length, CStringVar word) {
                return ICF.arithm(length, "=", word.getLength());
            }
        });
    }

    @Test(timeout = 60000)
    public void testPrefix() {
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrStringVar prefix, IrStringVar word) {
                return prefix(prefix, word);
            }

            Constraint setup(CStringVar prefix, CStringVar word) {
                return Constraints.prefix(
                        prefix.getChars(), prefix.getLength(),
                        word.getChars(), word.getLength());
            }
        });
    }

    @Test(timeout = 60000)
    public void testSuffix() {
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrStringVar suffix, IrStringVar word) {
                return suffix(suffix, word);
            }

            Constraint setup(CStringVar suffix, CStringVar word) {
                return Constraints.suffix(
                        suffix.getChars(), suffix.getLength(),
                        word.getChars(), word.getLength());
            }
        });
    }

    @Test(timeout = 60000)
    public void testConcat() {
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrStringVar left, IrStringVar right, IrStringVar concat) {
                return equal(concat, concat(left, right));
            }

            Constraint setup(CStringVar left, CStringVar right, CStringVar concat) {
                return Constraints.concat(
                        left.getChars(), left.getLength(),
                        right.getChars(), right.getLength(),
                        concat.getChars(), concat.getLength());
            }
        });
    }
}
