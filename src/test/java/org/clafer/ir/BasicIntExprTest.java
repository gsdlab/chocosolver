package org.clafer.ir;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.util.Arrays;
import org.clafer.ir.IrTest.IrVarField;
import org.clafer.ir.IrTest.TestCaseByConvention;
import static org.clafer.ir.Irs.*;
import org.clafer.ir.compiler.IrSolutionMap;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.ICF;
import solver.constraints.set.SCF;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.SetVar;
import solver.variables.VF;
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
                return nextBool()
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
                return nextBool()
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

    private Term randTerm() {
        switch (nextInt(3)) {
            case 0:
                return addRandTerm(randIntTerm());
            case 1:
                return addRandTerm(randBoolTerm());
            case 2:
                return addRandTerm(randFixedTerm());
            default:
                throw new IllegalStateException();
        }
    }

    private Term randIntTerm() {
        return new IntTerm(randInt(-5, 5));
    }

    private BoolTerm randBoolTerm() {
        return new BoolTerm(randBool());
    }

    private FixedTerm randFixedTerm() {
        return new FixedTerm(nextIntBetween(-5, 5));
    }

    private Term addRandTerm(Term view) {
        switch (nextInt(10)) {
            case 0:
            case 1:
                return new MinusTerm(view);
            case 3:
            case 4:
                return new OffsetTerm(view, nextIntBetween(-5, 5));
            default:
                return view;
        }
    }

    private Term addRandTerm(BoolTerm bool) {
        if (nextBool()) {
            return addRandTerm(new NotTerm(bool));
        }
        return addRandTerm((Term) bool);
    }

    private static interface Term {

        IrIntExpr toIrExpr();

        IrIntVar getIrVar();

        IntVar toChocoVar(Solver solver);

        int getValue(IrSolutionMap map);
    }

    private static class IntTerm implements Term {

        @IrVarField
        private final IrIntVar var;

        IntTerm(IrIntVar var) {
            this.var = var;
        }

        @Override
        public IrIntExpr toIrExpr() {
            return var;
        }

        @Override
        public IrIntVar getIrVar() {
            return var;
        }

        @Override
        public IntVar toChocoVar(Solver solver) {
            return toVar(var, solver);
        }

        @Override
        public int getValue(IrSolutionMap map) {
            return map.getValue(var);
        }
    }

    private static class BoolTerm implements Term {

        @IrVarField
        private final IrBoolVar var;

        BoolTerm(IrBoolVar var) {
            this.var = var;
        }

        @Override
        public IrBoolExpr toIrExpr() {
            return var;
        }

        @Override
        public IrIntVar getIrVar() {
            return var;
        }

        @Override
        public BoolVar toChocoVar(Solver solver) {
            return toVar(var, solver);
        }

        @Override
        public int getValue(IrSolutionMap map) {
            return map.getValue((IrIntVar) var);
        }
    }

    private static class FixedTerm implements Term {

        private final int c;

        FixedTerm(int c) {
            this.c = c;
        }

        @Override
        public IrIntExpr toIrExpr() {
            return constant(c);
        }

        @Override
        public IrIntVar getIrVar() {
            return constant(c);
        }

        @Override
        public IntVar toChocoVar(Solver solver) {
            return VF.fixed(c, solver);
        }

        @Override
        public int getValue(IrSolutionMap map) {
            return c;
        }
    }

    private static class MinusTerm implements Term {

        @IrVarField
        private final Term view;

        MinusTerm(Term view) {
            this.view = view;
        }

        @Override
        public IrIntExpr toIrExpr() {
            return minus(view.toIrExpr());
        }

        @Override
        public IrIntVar getIrVar() {
            return view.getIrVar();
        }

        @Override
        public IntVar toChocoVar(Solver solver) {
            return VF.minus(view.toChocoVar(solver));
        }

        @Override
        public int getValue(IrSolutionMap map) {
            return -view.getValue(map);
        }
    }

    private static class OffsetTerm implements Term {

        @IrVarField
        private final Term view;
        private final int offset;

        OffsetTerm(Term view, int offset) {
            this.view = view;
            this.offset = offset;
        }

        @Override
        public IrIntExpr toIrExpr() {
            return add(view.toIrExpr(), offset);
        }

        @Override
        public IrIntVar getIrVar() {
            return view.getIrVar();
        }

        @Override
        public IntVar toChocoVar(Solver solver) {
            return VF.offset(view.toChocoVar(solver), offset);
        }

        @Override
        public int getValue(IrSolutionMap map) {
            return view.getValue(map) + offset;
        }
    }

    private static class NotTerm implements Term {

        @IrVarField
        private final Term view;

        NotTerm(Term view) {
            this.view = view;
        }

        @Override
        public IrIntExpr toIrExpr() {
            return not((IrBoolExpr) view.toIrExpr());
        }

        @Override
        public IrIntVar getIrVar() {
            return view.getIrVar();
        }

        @Override
        public IntVar toChocoVar(Solver solver) {
            return VF.not((BoolVar) view.toChocoVar(solver));
        }

        @Override
        public int getValue(IrSolutionMap map) {
            return 1 - view.getValue(map);
        }
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

        @Override
        Object create(IrModule module, Annotations annotations, Class<?> type) {
            if (Term.class.equals(type)) {
                Term term = annotations.hasAnnotation(Fixed.class) ? randFixedTerm() : randTerm();
                module.addVariable(term.getIrVar());
                return term;
            }
            return super.create(module, annotations, type);
        }

        @Override
        Object create(Solver solver, Class<?> type, Object value) {
            if (value instanceof Term) {
                return ((Term) value).toChocoVar(solver);
            }
            return super.create(solver, type, value);
        }

        @Override
        Object value(IrSolutionMap solution, Object var) {
            if (var instanceof Term) {
                return ((Term) var).getValue(solution);
            }
            return super.value(solution, var);
        }
    }

    private class ArithmXYCTestCaseByConvention extends ArithmXYTestCaseByConvention {

        @Override
        protected Annotations[] annotations() {
            Annotations[] annotations = super.annotations();
            annotations[nextInt(annotations.length)].addAnnotation(fixed);
            return annotations;
        }
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.FIELD})
    private @interface Fixed {
    }

    private static final Fixed fixed = new Fixed() {

        @Override
        public Class<? extends Annotation> annotationType() {
            return Fixed.class;
        }
    };
}
