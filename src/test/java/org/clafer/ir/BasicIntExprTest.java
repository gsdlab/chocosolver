package org.clafer.ir;

import java.util.Arrays;
import org.clafer.common.Util;
import static org.clafer.ir.Irs.*;
import org.clafer.ir.compiler.IrSolutionMap;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.ICF;
import solver.variables.BoolVar;
import solver.variables.IntVar;
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
    public void testAdd() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrIntVar sum;
            @IrVarField
            IrIntVar[] is;

            @Override
            void check(IrSolutionMap solution) {
                assertEquals(
                        solution.getValue(sum),
                        Util.sum(solution.getValues(is)));
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return equal(sum, add(is));
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.sum(toVars(is, solver), toVar(sum, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testSub() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrIntVar diff;
            @IrVarField
            IrIntVar[] is;

            @Override
            void check(IrSolutionMap solution) {
                int[] IS = solution.getValues(is);
                if (IS.length == 0) {
                    assertEquals(solution.getValue(diff), 0);
                } else {
                    int DIFF = IS[0];
                    for (int i = 1; i < IS.length; i++) {
                        DIFF -= IS[i];
                    }
                    assertEquals(solution.getValue(diff), DIFF);
                }
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return equal(diff, sub(is));
            }

            @Override
            Constraint setup(Solver solver) {
                IntVar DIFF = toVar(diff, solver);
                IntVar[] IS = toVars(is, solver);
                int[] coefficients = new int[IS.length];
                Arrays.fill(coefficients, -1);
                if (coefficients.length > 0) {
                    coefficients[0] = 1;
                }
                return ICF.scalar(IS, coefficients, DIFF);
            }
        });
    }

    private void assertOptimizedArithm(Solver solver) {
        assertTrue("Correct but not optimized.", solver.getNbCstrs() <= 1);
        assertTrue("Correct but not optimized.", solver.getNbVars() <= 4);
        for (Variable var : solver.getVars()) {
            assertFalse("Correct but not optimized.",
                    var instanceof FixedIntVarImpl && !(var instanceof FixedBoolVarImpl));
            assertFalse("Correct but not optimized.", var instanceof IntView);
        }
    }

    @Test(timeout = 60000)
    public void testEqualXY() {
        randomizedTest(new TestCase() {
            @IrVarField
            Term var1;
            @IrVarField
            Term var2;

            @Override
            void check(IrSolutionMap solution) {
                assertEquals(
                        var1.getValue(solution),
                        var2.getValue(solution));
            }

            @Override
            void initialize() {
                var1 = randTerm();
                var2 = randTerm();
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return equal(var1.toIrExpr(), var2.toIrExpr());
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.arithm(var1.toChocoVar(solver),
                        "=", var2.toChocoVar(solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testNotEqualXY() {
        randomizedTest(new TestCase() {
            @IrVarField
            Term var1;
            @IrVarField
            Term var2;

            @Override
            void check(IrSolutionMap solution) {
                assertNotEquals(
                        var1.getValue(solution),
                        var2.getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            void initialize() {
                var1 = randTerm();
                var2 = randTerm();
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return notEqual(var1.toIrExpr(), var2.toIrExpr());
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.arithm(var1.toChocoVar(solver),
                        "!=", var2.toChocoVar(solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThanXY() {
        randomizedTest(new TestCase() {
            @IrVarField
            Term var1;
            @IrVarField
            Term var2;

            @Override
            void check(IrSolutionMap solution) {
                assertTrue(
                        var1.getValue(solution)
                        < var2.getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            void initialize() {
                var1 = randTerm();
                var2 = randTerm();
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return lessThan(var1.toIrExpr(), var2.toIrExpr());
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.arithm(var1.toChocoVar(solver),
                        "<", var2.toChocoVar(solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThanEqualXY() {
        randomizedTest(new TestCase() {
            @IrVarField
            Term var1;
            @IrVarField
            Term var2;

            @Override
            void check(IrSolutionMap solution) {
                assertTrue(
                        var1.getValue(solution)
                        <= var2.getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            void initialize() {
                var1 = randTerm();
                var2 = randTerm();
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return lessThanEqual(var1.toIrExpr(), var2.toIrExpr());
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.arithm(var1.toChocoVar(solver),
                        "<=", var2.toChocoVar(solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testEqualXYC() {
        randomizedTest(new TestCase() {
            @IrVarField
            Term var1;
            @IrVarField
            Term var2;
            @IrVarField
            Term var3;

            @Override
            void check(IrSolutionMap solution) {
                assertEquals(
                        var1.getValue(solution) + var2.getValue(solution),
                        var3.getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            void initialize() {
                Term[] vars = new Term[]{randFixedTerm(), randTerm(), randTerm()};
                Util.shuffle(vars, rand);
                var1 = vars[0];
                var2 = vars[1];
                var3 = vars[2];
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                IrIntExpr add = add(var1.toIrExpr(), var2.toIrExpr());
                return nextBool()
                        ? equal(add, var3.toIrExpr())
                        : equal(var3.toIrExpr(), add);
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.sum(new IntVar[]{
                    var1.toChocoVar(solver), var2.toChocoVar(solver)},
                        var3.toChocoVar(solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testNotEqualXYC() {
        randomizedTest(new TestCase() {
            @IrVarField
            Term var1;
            @IrVarField
            Term var2;
            @IrVarField
            Term var3;

            @Override
            void check(IrSolutionMap solution) {
                assertNotEquals(
                        var1.getValue(solution) + var2.getValue(solution),
                        var3.getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            void initialize() {
                Term[] vars = new Term[]{randFixedTerm(), randTerm(), randTerm()};
                Util.shuffle(vars, rand);
                var1 = vars[0];
                var2 = vars[1];
                var3 = vars[2];
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                IrIntExpr add = add(var1.toIrExpr(), var2.toIrExpr());
                return nextBool()
                        ? notEqual(add, var3.toIrExpr())
                        : notEqual(var3.toIrExpr(), add);
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.sum(new IntVar[]{
                    var1.toChocoVar(solver), var2.toChocoVar(solver)},
                        var3.toChocoVar(solver)).getOpposite();
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThanXYC() {
        randomizedTest(new TestCase() {
            @IrVarField
            Term var1;
            @IrVarField
            Term var2;
            @IrVarField
            Term var3;

            @Override
            void check(IrSolutionMap solution) {
                assertTrue(
                        var1.getValue(solution) + var2.getValue(solution)
                        < var3.getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            void initialize() {
                Term[] vars = new Term[]{randFixedTerm(), randTerm(), randTerm()};
                Util.shuffle(vars, rand);
                var1 = vars[0];
                var2 = vars[1];
                var3 = vars[2];
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return lessThan(add(var1.toIrExpr(), var2.toIrExpr()), var3.toIrExpr());
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.sum(new IntVar[]{
                    var1.toChocoVar(solver), var2.toChocoVar(solver)},
                        "<", var3.toChocoVar(solver));
            }
        });
        randomizedTest(new TestCase() {
            @IrVarField
            Term var1;
            @IrVarField
            Term var2;
            @IrVarField
            Term var3;

            @Override
            void check(IrSolutionMap solution) {
                assertTrue(
                        var1.getValue(solution) + var2.getValue(solution)
                        > var3.getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            void initialize() {
                Term[] vars = new Term[]{randFixedTerm(), randTerm(), randTerm()};
                Util.shuffle(vars, rand);
                var1 = vars[0];
                var2 = vars[1];
                var3 = vars[2];
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return lessThan(var3.toIrExpr(), add(var1.toIrExpr(), var2.toIrExpr()));
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.sum(new IntVar[]{
                    var1.toChocoVar(solver), var2.toChocoVar(solver)},
                        ">", var3.toChocoVar(solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThanEqualXYC() {
        randomizedTest(new TestCase() {
            @IrVarField
            Term var1;
            @IrVarField
            Term var2;
            @IrVarField
            Term var3;

            @Override
            void check(IrSolutionMap solution) {
                assertTrue(
                        var1.getValue(solution) + var2.getValue(solution)
                        <= var3.getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            void initialize() {
                Term[] vars = new Term[]{randFixedTerm(), randTerm(), randTerm()};
                Util.shuffle(vars, rand);
                var1 = vars[0];
                var2 = vars[1];
                var3 = vars[2];
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return lessThanEqual(add(var1.toIrExpr(), var2.toIrExpr()), var3.toIrExpr());
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.sum(new IntVar[]{
                    var1.toChocoVar(solver), var2.toChocoVar(solver)},
                        "<=", var3.toChocoVar(solver));
            }
        });
        randomizedTest(new TestCase() {
            @IrVarField
            Term var1;
            @IrVarField
            Term var2;
            @IrVarField
            Term var3;

            @Override
            void check(IrSolutionMap solution) {
                assertTrue(
                        var1.getValue(solution) + var2.getValue(solution)
                        >= var3.getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            void initialize() {
                Term[] vars = new Term[]{randFixedTerm(), randTerm(), randTerm()};
                Util.shuffle(vars, rand);
                var1 = vars[0];
                var2 = vars[1];
                var3 = vars[2];
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return lessThanEqual(var3.toIrExpr(), add(var1.toIrExpr(), var2.toIrExpr()));
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.sum(new IntVar[]{
                    var1.toChocoVar(solver), var2.toChocoVar(solver)},
                        ">=", var3.toChocoVar(solver));
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
}
