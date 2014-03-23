package org.clafer.ir;

import java.util.Arrays;
import static org.clafer.ClaferTest.toIntVars;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
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
        randomizedTest(new TestCase<Pair<IrIntVar, IrIntVar[]>>() {

            @Override
            void check(IrSolutionMap solution, Pair<IrIntVar, IrIntVar[]> s) {
                assertEquals(
                        solution.getValue(s.getFst()),
                        Util.sum(solution.getValues(s.getSnd())));
            }

            @Override
            Pair<IrBoolExpr, Pair<IrIntVar, IrIntVar[]>> setup(IrModule module) {
                IrIntVar sum = boundInt("sum", -10, 10);
                IrIntVar[] is = randInts(nextInt(5));
                return pair(equal(sum, add(is)), pair(sum, is));
            }

            @Override
            Constraint setup(Pair<IrIntVar, IrIntVar[]> s, Solver solver) {
                IntVar sum = toIntVar(s.getFst(), solver);
                IntVar[] is = toIntVars(s.getSnd(), solver);
                return ICF.sum(is, sum);
            }
        });
    }

    @Test(timeout = 60000)
    public void testSub() {
        randomizedTest(new TestCase<Pair<IrIntVar, IrIntVar[]>>() {

            @Override
            void check(IrSolutionMap solution, Pair<IrIntVar, IrIntVar[]> s) {
                int[] is = solution.getValues(s.getSnd());
                if (is.length == 0) {
                    assertEquals(solution.getValue(s.getFst()), 0);
                } else {
                    int diff = is[0];
                    for (int i = 1; i < is.length; i++) {
                        diff -= is[i];
                    }
                    assertEquals(solution.getValue(s.getFst()), diff);
                }
            }

            @Override
            Pair<IrBoolExpr, Pair<IrIntVar, IrIntVar[]>> setup(IrModule module) {
                IrIntVar diff = boundInt("diff", -10, 10);
                IrIntVar[] is = randInts(nextInt(5));
                return pair(equal(diff, sub(is)), pair(diff, is));
            }

            @Override
            Constraint setup(Pair<IrIntVar, IrIntVar[]> s, Solver solver) {
                IntVar diff = toIntVar(s.getFst(), solver);
                IntVar[] is = toIntVars(s.getSnd(), solver);
                int[] coefficients = new int[is.length];
                Arrays.fill(coefficients, -1);
                if (coefficients.length > 0) {
                    coefficients[0] = 1;
                }
                return ICF.scalar(is, coefficients, diff);
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
        randomizedTest(new TestCase<Pair<Term, Term>>() {

            @Override
            void check(IrSolutionMap solution, Pair<Term, Term> s) {
                assertEquals(
                        s.getFst().getValue(solution),
                        s.getSnd().getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            Pair<IrBoolExpr, Pair<Term, Term>> setup(IrModule module) {
                Term var1 = randTerm();
                Term var2 = randTerm();
                return pair(
                        equal(var1.toIrExpr(), var2.toIrExpr()),
                        pair(var1, var2));
            }

            @Override
            Constraint setup(Pair<Term, Term> s, Solver solver) {
                return ICF.arithm(s.getFst().toChocoVar(solver),
                        "=", s.getSnd().toChocoVar(solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testNotEqualXY() {
        randomizedTest(new TestCase<Pair<Term, Term>>() {

            @Override
            void check(IrSolutionMap solution, Pair<Term, Term> s) {
                assertNotEquals(
                        s.getFst().getValue(solution),
                        s.getSnd().getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            Pair<IrBoolExpr, Pair<Term, Term>> setup(IrModule module) {
                Term var1 = randTerm();
                Term var2 = randTerm();
                return pair(
                        notEqual(var1.toIrExpr(), var2.toIrExpr()),
                        pair(var1, var2));
            }

            @Override
            Constraint setup(Pair<Term, Term> s, Solver solver) {
                return ICF.arithm(s.getFst().toChocoVar(solver),
                        "!=", s.getSnd().toChocoVar(solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThanXY() {
        randomizedTest(new TestCase<Pair<Term, Term>>() {

            @Override
            void check(IrSolutionMap solution, Pair<Term, Term> s) {
                assertTrue(
                        s.getFst().getValue(solution)
                        < s.getSnd().getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            Pair<IrBoolExpr, Pair<Term, Term>> setup(IrModule module) {
                Term var1 = randTerm();
                Term var2 = randTerm();
                return pair(
                        lessThan(var1.toIrExpr(), var2.toIrExpr()),
                        pair(var1, var2));
            }

            @Override
            Constraint setup(Pair<Term, Term> s, Solver solver) {
                return ICF.arithm(s.getFst().toChocoVar(solver),
                        "<", s.getSnd().toChocoVar(solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThanEqualXY() {
        randomizedTest(new TestCase<Pair<Term, Term>>() {

            @Override
            void check(IrSolutionMap solution, Pair<Term, Term> s) {
                assertTrue(
                        s.getFst().getValue(solution)
                        <= s.getSnd().getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            Pair<IrBoolExpr, Pair<Term, Term>> setup(IrModule module) {
                Term var1 = randTerm();
                Term var2 = randTerm();
                return pair(
                        lessThanEqual(var1.toIrExpr(), var2.toIrExpr()),
                        pair(var1, var2));
            }

            @Override
            Constraint setup(Pair<Term, Term> s, Solver solver) {
                return ICF.arithm(s.getFst().toChocoVar(solver),
                        "<=", s.getSnd().toChocoVar(solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testEqualXYC() {
        randomizedTest(new TestCase<Triple<Term, Term, Term>>() {

            @Override
            void check(IrSolutionMap solution, Triple<Term, Term, Term> s) {
                assertEquals(
                        s.getFst().getValue(solution) + s.getSnd().getValue(solution),
                        s.getThd().getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            Pair<IrBoolExpr, Triple<Term, Term, Term>> setup(IrModule module) {
                Term[] vars = new Term[]{randFixedTerm(), randTerm(), randTerm()};
                Util.shuffle(vars, rand);
                Term var1 = vars[0];
                Term var2 = vars[1];
                Term var3 = vars[2];
                IrIntExpr add = add(var1.toIrExpr(), var2.toIrExpr());
                return pair(
                        nextBool()
                        ? equal(add, var3.toIrExpr())
                        : equal(var3.toIrExpr(), add),
                        triple(var1, var2, var3));
            }

            @Override
            Constraint setup(Triple<Term, Term, Term> s, Solver solver) {
                return ICF.sum(new IntVar[]{
                    s.getFst().toChocoVar(solver), s.getSnd().toChocoVar(solver)},
                        s.getThd().toChocoVar(solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testNotEqualXYC() {
        randomizedTest(new TestCase<Triple<Term, Term, Term>>() {

            @Override
            void check(IrSolutionMap solution, Triple<Term, Term, Term> s) {
                assertNotEquals(
                        s.getFst().getValue(solution) + s.getSnd().getValue(solution),
                        s.getThd().getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            Pair<IrBoolExpr, Triple<Term, Term, Term>> setup(IrModule module) {
                Term[] vars = new Term[]{randFixedTerm(), randTerm(), randTerm()};
                Util.shuffle(vars, rand);
                Term var1 = vars[0];
                Term var2 = vars[1];
                Term var3 = vars[2];
                IrIntExpr add = add(var1.toIrExpr(), var2.toIrExpr());
                return pair(
                        nextBool()
                        ? notEqual(add, var3.toIrExpr())
                        : notEqual(var3.toIrExpr(), add),
                        triple(var1, var2, var3));
            }

            @Override
            Constraint setup(Triple<Term, Term, Term> s, Solver solver) {
                return ICF.sum(new IntVar[]{
                    s.getFst().toChocoVar(solver), s.getSnd().toChocoVar(solver)},
                        s.getThd().toChocoVar(solver)).getOpposite();
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThanXYC() {
        randomizedTest(new TestCase<Triple<Term, Term, Term>>() {

            @Override
            void check(IrSolutionMap solution, Triple<Term, Term, Term> s) {
                assertTrue(
                        s.getFst().getValue(solution) + s.getSnd().getValue(solution)
                        < s.getThd().getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            Pair<IrBoolExpr, Triple<Term, Term, Term>> setup(IrModule module) {
                Term[] vars = new Term[]{randFixedTerm(), randTerm(), randTerm()};
                Util.shuffle(vars, rand);
                Term var1 = vars[0];
                Term var2 = vars[1];
                Term var3 = vars[2];
                return pair(
                        lessThan(add(var1.toIrExpr(), var2.toIrExpr()), var3.toIrExpr()),
                        triple(var1, var2, var3));
            }

            @Override
            Constraint setup(Triple<Term, Term, Term> s, Solver solver) {
                return ICF.sum(new IntVar[]{
                    s.getFst().toChocoVar(solver), s.getSnd().toChocoVar(solver)},
                        "<", s.getThd().toChocoVar(solver));
            }
        });
        randomizedTest(new TestCase<Triple<Term, Term, Term>>() {

            @Override
            void check(IrSolutionMap solution, Triple<Term, Term, Term> s) {
                assertTrue(
                        s.getFst().getValue(solution) + s.getSnd().getValue(solution)
                        > s.getThd().getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            Pair<IrBoolExpr, Triple<Term, Term, Term>> setup(IrModule module) {
                Term[] vars = new Term[]{randFixedTerm(), randTerm(), randTerm()};
                Util.shuffle(vars, rand);
                Term var1 = vars[0];
                Term var2 = vars[1];
                Term var3 = vars[2];
                return pair(
                        lessThan(var3.toIrExpr(), add(var1.toIrExpr(), var2.toIrExpr())),
                        triple(var1, var2, var3));
            }

            @Override
            Constraint setup(Triple<Term, Term, Term> s, Solver solver) {
                return ICF.sum(new IntVar[]{
                    s.getFst().toChocoVar(solver), s.getSnd().toChocoVar(solver)},
                        ">", s.getThd().toChocoVar(solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThanEqualXYC() {
        randomizedTest(new TestCase<Triple<Term, Term, Term>>() {

            @Override
            void check(IrSolutionMap solution, Triple<Term, Term, Term> s) {
                assertTrue(
                        s.getFst().getValue(solution) + s.getSnd().getValue(solution)
                        <= s.getThd().getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            Pair<IrBoolExpr, Triple<Term, Term, Term>> setup(IrModule module) {
                Term[] vars = new Term[]{randFixedTerm(), randTerm(), randTerm()};
                Util.shuffle(vars, rand);
                Term var1 = vars[0];
                Term var2 = vars[1];
                Term var3 = vars[2];
                return pair(
                        lessThanEqual(add(var1.toIrExpr(), var2.toIrExpr()), var3.toIrExpr()),
                        triple(var1, var2, var3));
            }

            @Override
            Constraint setup(Triple<Term, Term, Term> s, Solver solver) {
                return ICF.sum(new IntVar[]{
                    s.getFst().toChocoVar(solver), s.getSnd().toChocoVar(solver)},
                        "<=", s.getThd().toChocoVar(solver));
            }
        });
        randomizedTest(new TestCase<Triple<Term, Term, Term>>() {

            @Override
            void check(IrSolutionMap solution, Triple<Term, Term, Term> s) {
                assertTrue(
                        s.getFst().getValue(solution) + s.getSnd().getValue(solution)
                        >= s.getThd().getValue(solution));
            }

            @Override
            void validateTranslation(Solver solver) {
                assertOptimizedArithm(solver);
            }

            @Override
            Pair<IrBoolExpr, Triple<Term, Term, Term>> setup(IrModule module) {
                Term[] vars = new Term[]{randFixedTerm(), randTerm(), randTerm()};
                Util.shuffle(vars, rand);
                Term var1 = vars[0];
                Term var2 = vars[1];
                Term var3 = vars[2];
                return pair(
                        lessThanEqual(var3.toIrExpr(), add(var1.toIrExpr(), var2.toIrExpr())),
                        triple(var1, var2, var3));
            }

            @Override
            Constraint setup(Triple<Term, Term, Term> s, Solver solver) {
                return ICF.sum(new IntVar[]{
                    s.getFst().toChocoVar(solver), s.getSnd().toChocoVar(solver)},
                        ">=", s.getThd().toChocoVar(solver));
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
            return toIntVar(var, solver);
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
            return toBoolVar(var, solver);
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
