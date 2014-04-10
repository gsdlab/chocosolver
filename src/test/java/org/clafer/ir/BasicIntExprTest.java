package org.clafer.ir;

import java.lang.annotation.Annotation;
import java.util.Arrays;
import org.clafer.test.Annotations;
import static org.clafer.test.TestUtil.*;
import org.clafer.ir.IrTest.TestCaseByConvention;
import static org.clafer.ir.Irs.*;
import org.clafer.test.Fixed;
import org.clafer.test.Term;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.ICF;
import solver.constraints.set.SCF;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.Variable;
import solver.variables.impl.FixedBoolVarImpl;
import solver.variables.impl.FixedIntVarImpl;
import solver.variables.view.IntView;

/**
 *
 * @author jimmy
 */
public class BasicIntExprTest extends IrTest {

    @Test(timeout = 60000)
    public void testCard() {
        randomizedTest(new TestCaseByConvention() {
            IrBoolExpr setup(IrIntVar card, IrSetVar set) {
                return equal(card, card(set));
            }

            Constraint setup(IntVar card, SetVar set) {
                return SCF.cardinality(set, card);
            }
        });
    }

    @Test(timeout = 60000)
    public void testAdd() {
        randomizedTest(new TestCaseByConvention() {
            IrBoolExpr setup(IrIntVar sum, IrIntVar[] is) {
                return equal(sum, add(is));
            }

            Constraint setup(IntVar sum, IntVar[] is) {
                return ICF.sum(is, sum);
            }
        });
    }

    @Test(timeout = 60000)
    public void testSub() {
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar diff, IrIntVar[] is) {
                return equal(diff, sub(is));
            }

            Constraint setup(IntVar diff, IntVar[] is) {
                int[] coefficients = new int[is.length];
                Arrays.fill(coefficients, -1);
                if (coefficients.length > 0) {
                    coefficients[0] = 1;
                }
                return ICF.scalar(is, coefficients, diff);
            }
        });
    }

    @Test(timeout = 60000)
    public void testEqualXY() {
        randomizedTest(new ArithmXYTestCaseByConvention() {

            IrBoolExpr setup(Term var1, Term var2) {
                return equal(var1.toIrExpr(), var2.toIrExpr());
            }

            Constraint setup(IntVar var1, IntVar var2) {
                return ICF.arithm(var1, "=", var2);
            }
        });
    }

    @Test(timeout = 60000)
    public void testNotEqualXY() {
        randomizedTest(new ArithmXYTestCaseByConvention() {

            IrBoolExpr setup(Term var1, Term var2) {
                return notEqual(var1.toIrExpr(), var2.toIrExpr());
            }

            Constraint setup(IntVar var1, IntVar var2) {
                return ICF.arithm(var1, "!=", var2);
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThanXY() {
        randomizedTest(new ArithmXYTestCaseByConvention() {

            IrBoolExpr setup(Term var1, Term var2) {
                return lessThan(var1.toIrExpr(), var2.toIrExpr());
            }

            Constraint setup(IntVar var1, IntVar var2) {
                return ICF.arithm(var1, "<", var2);
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThanEqualXY() {
        randomizedTest(new ArithmXYTestCaseByConvention() {

            IrBoolExpr setup(Term var1, Term var2) {
                return lessThanEqual(var1.toIrExpr(), var2.toIrExpr());
            }

            Constraint setup(IntVar var1, IntVar var2) {
                return ICF.arithm(var1, "<=", var2);
            }
        });
    }

    @Test(timeout = 60000)
    public void testGreaterThanXY() {
        randomizedTest(new ArithmXYTestCaseByConvention() {

            IrBoolExpr setup(Term var1, Term var2) {
                return greaterThan(var1.toIrExpr(), var2.toIrExpr());
            }

            Constraint setup(IntVar var1, IntVar var2) {
                return ICF.arithm(var1, ">", var2);
            }
        });
    }

    @Test(timeout = 60000)
    public void testGreaterThanEqualXY() {
        randomizedTest(new ArithmXYTestCaseByConvention() {

            IrBoolExpr setup(Term var1, Term var2) {
                return greaterThanEqual(var1.toIrExpr(), var2.toIrExpr());
            }

            Constraint setup(IntVar var1, IntVar var2) {
                return ICF.arithm(var1, ">=", var2);
            }
        });
    }

    @Test(timeout = 60000)
    public void testEqualXYC() {
        randomizedTest(new ArithmXYCTestCaseByConvention() {

            IrBoolExpr setup(Term var1, Term var2, Term var3) {
                IrIntExpr add = add(var1.toIrExpr(), var2.toIrExpr());
                return randBool()
                        ? equal(add, var3.toIrExpr())
                        : equal(var3.toIrExpr(), add);
            }

            Constraint setup(IntVar var1, IntVar var2, IntVar var3) {
                return ICF.sum(new IntVar[]{var1, var2}, var3);
            }
        });
    }

    @Test(timeout = 60000)
    public void testNotEqualXYC() {
        randomizedTest(new ArithmXYCTestCaseByConvention() {

            IrBoolExpr setup(Term var1, Term var2, Term var3) {
                IrIntExpr add = add(var1.toIrExpr(), var2.toIrExpr());
                return randBool()
                        ? notEqual(add, var3.toIrExpr())
                        : notEqual(var3.toIrExpr(), add);
            }

            Constraint setup(IntVar var1, IntVar var2, IntVar var3) {
                return ICF.sum(new IntVar[]{var1, var2}, var3).getOpposite();
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThanXYC() {
        randomizedTest(new ArithmXYCTestCaseByConvention() {

            IrBoolExpr setup(Term var1, Term var2, Term var3) {
                return lessThan(add(var1.toIrExpr(), var2.toIrExpr()), var3.toIrExpr());
            }

            Constraint setup(IntVar var1, IntVar var2, IntVar var3) {
                return ICF.sum(new IntVar[]{var1, var2}, "<", var3);
            }
        });
        randomizedTest(new ArithmXYCTestCaseByConvention() {

            IrBoolExpr setup(Term var1, Term var2, Term var3) {
                return lessThan(var1.toIrExpr(), add(var2.toIrExpr(), var3.toIrExpr()));
            }

            Constraint setup(IntVar var1, IntVar var2, IntVar var3) {
                return ICF.sum(new IntVar[]{var2, var3}, ">", var1);
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThanEqualXYC() {
        randomizedTest(new ArithmXYCTestCaseByConvention() {

            IrBoolExpr setup(Term var1, Term var2, Term var3) {
                return lessThanEqual(add(var1.toIrExpr(), var2.toIrExpr()), var3.toIrExpr());
            }

            Constraint setup(IntVar var1, IntVar var2, IntVar var3) {
                return ICF.sum(new IntVar[]{var1, var2}, "<=", var3);
            }
        });
        randomizedTest(new ArithmXYCTestCaseByConvention() {

            IrBoolExpr setup(Term var1, Term var2, Term var3) {
                return lessThanEqual(var1.toIrExpr(), add(var2.toIrExpr(), var3.toIrExpr()));
            }

            Constraint setup(IntVar var1, IntVar var2, IntVar var3) {
                return ICF.sum(new IntVar[]{var2, var3}, ">=", var1);
            }
        });
    }

    @Test(timeout = 60000)
    public void testGreaterThanXYC() {
        randomizedTest(new ArithmXYCTestCaseByConvention() {

            IrBoolExpr setup(Term var1, Term var2, Term var3) {
                return greaterThan(add(var1.toIrExpr(), var2.toIrExpr()), var3.toIrExpr());
            }

            Constraint setup(IntVar var1, IntVar var2, IntVar var3) {
                return ICF.sum(new IntVar[]{var1, var2}, ">", var3);
            }
        });
        randomizedTest(new ArithmXYCTestCaseByConvention() {

            IrBoolExpr setup(Term var1, Term var2, Term var3) {
                return greaterThan(var1.toIrExpr(), add(var2.toIrExpr(), var3.toIrExpr()));
            }

            Constraint setup(IntVar var1, IntVar var2, IntVar var3) {
                return ICF.sum(new IntVar[]{var2, var3}, "<", var1);
            }
        });
    }

    @Test(timeout = 60000)
    public void testGreaterThanEqualXYC() {
        randomizedTest(new ArithmXYCTestCaseByConvention() {

            IrBoolExpr setup(Term var1, Term var2, Term var3) {
                return greaterThanEqual(add(var1.toIrExpr(), var2.toIrExpr()), var3.toIrExpr());
            }

            Constraint setup(IntVar var1, IntVar var2, IntVar var3) {
                return ICF.sum(new IntVar[]{var1, var2}, ">=", var3);
            }
        });
        randomizedTest(new ArithmXYCTestCaseByConvention() {

            IrBoolExpr setup(Term var1, Term var2, Term var3) {
                return greaterThanEqual(var1.toIrExpr(), add(var2.toIrExpr(), var3.toIrExpr()));
            }

            Constraint setup(IntVar var1, IntVar var2, IntVar var3) {
                return ICF.sum(new IntVar[]{var2, var3}, "<=", var1);
            }
        });
    }

    private class ArithmXYTestCaseByConvention extends TestCaseByConvention {

        @Override
        void validateTranslation(Solver solver) {
            assertTrue("Correct but not optimized.", solver.getNbCstrs() <= 1);
            assertTrue("Correct but not optimized.", solver.getNbVars() <= 4);
            for (Variable var : solver.getVars()) {
                assertFalse("Correct but not optimized.",
                        var instanceof FixedIntVarImpl && !(var instanceof FixedBoolVarImpl));
                assertFalse("Correct but not optimized.", var instanceof IntView);
            }
        }
    }

    private class ArithmXYCTestCaseByConvention extends ArithmXYTestCaseByConvention {

        @Override
        protected Annotations[] annotations() {
            Annotations[] annotations = super.annotations();
            annotations[randInt(0, annotations.length - 1)].addAnnotation(fixed);
            return annotations;
        }
    }

    private static final Fixed fixed = new Fixed() {

        @Override
        public Class<? extends Annotation> annotationType() {
            return Fixed.class;
        }
    };
}
