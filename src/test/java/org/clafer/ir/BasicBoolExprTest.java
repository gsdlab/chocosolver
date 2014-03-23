package org.clafer.ir;

import java.util.Arrays;
import static org.clafer.ClaferTest.toBoolVars;
import org.clafer.choco.constraint.Constraints;
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
import solver.constraints.set.SCF;

/**
 *
 * @author jimmy
 */
public class BasicBoolExprTest extends IrTest {

    @Test(timeout = 60000)
    public void testAnd() {
        randomizedTest(new TestCase<IrBoolVar[]>() {

            @Override
            void check(IrSolutionMap solution, IrBoolVar[] s) {
                for (IrBoolVar var : s) {
                    assertTrue(solution.getValue(var));
                }
            }

            @Override
            Pair<IrBoolExpr, IrBoolVar[]> setup(IrModule module) {
                IrBoolVar[] vars = randBools(nextInt(5));
                return pair(and(vars), vars);
            }

            @Override
            Constraint setup(IrBoolVar[] s, Solver solver) {
                return s.length == 0
                        ? solver.TRUE
                        : Constraints.and(toBoolVars(s, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testLone() {
        randomizedTest(new TestCase<IrBoolVar[]>() {

            @Override
            void check(IrSolutionMap solution, IrBoolVar[] s) {
                assertTrue(Util.sum(solution.getValues(s)) <= 1);
            }

            @Override
            Pair<IrBoolExpr, IrBoolVar[]> setup(IrModule module) {
                IrBoolVar[] vars = randBools(nextInt(5));
                return pair(lone(vars), vars);
            }

            @Override
            Constraint setup(IrBoolVar[] s, Solver solver) {
                return s.length == 0
                        ? solver.TRUE
                        : Constraints.lone(toBoolVars(s, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testOne() {
        randomizedTest(new TestCase<IrBoolVar[]>() {

            @Override
            void check(IrSolutionMap solution, IrBoolVar[] s) {
                assertEquals(1, Util.sum(solution.getValues(s)));
            }

            @Override
            Pair<IrBoolExpr, IrBoolVar[]> setup(IrModule module) {
                IrBoolVar[] vars = randBools(nextInt(5));
                return pair(one(vars), vars);
            }

            @Override
            Constraint setup(IrBoolVar[] s, Solver solver) {
                return s.length == 0
                        ? solver.FALSE
                        : Constraints.one(toBoolVars(s, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testOr() {
        randomizedTest(new TestCase<IrBoolVar[]>() {

            @Override
            void check(IrSolutionMap solution, IrBoolVar[] s) {
                assertTrue(Util.sum(solution.getValues(s)) >= 1);
            }

            @Override
            Pair<IrBoolExpr, IrBoolVar[]> setup(IrModule module) {
                IrBoolVar[] vars = randBools(nextInt(5));
                return pair(or(vars), vars);
            }

            @Override
            Constraint setup(IrBoolVar[] s, Solver solver) {
                return s.length == 0
                        ? solver.FALSE
                        : Constraints.or(toBoolVars(s, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testIfThenElse() {
        randomizedTest(new TestCase<Triple<IrBoolVar, IrBoolVar, IrBoolVar>>() {

            @Override
            void check(IrSolutionMap solution, Triple<IrBoolVar, IrBoolVar, IrBoolVar> s) {
                assertTrue(solution.getValue(s.getFst())
                        ? solution.getValue(s.getSnd())
                        : solution.getValue(s.getThd()));
            }

            @Override
            Pair<IrBoolExpr, Triple<IrBoolVar, IrBoolVar, IrBoolVar>> setup(IrModule module) {
                IrBoolVar b1 = randBool();
                IrBoolVar b2 = randBool();
                IrBoolVar b3 = randBool();
                return pair(ifThenElse(b1, b2, b3), triple(b1, b2, b3));
            }

            @Override
            Constraint setup(Triple<IrBoolVar, IrBoolVar, IrBoolVar> s, Solver solver) {
                return Constraints.ifThenElse(
                        toBoolVar(s.getFst(), solver),
                        toBoolVar(s.getSnd(), solver),
                        toBoolVar(s.getThd(), solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testEqual() {
        randomizedTest(new TestCase<Pair<IrIntVar, IrIntVar>>() {

            @Override
            void check(IrSolutionMap solution, Pair<IrIntVar, IrIntVar> s) {
                assertEquals(solution.getValue(s.getFst()), solution.getValue(s.getSnd()));
            }

            @Override
            Pair<IrBoolExpr, Pair<IrIntVar, IrIntVar>> setup(IrModule module) {
                IrIntVar i1 = randInt();
                IrIntVar i2 = randInt();
                return pair(equal(i1, i2), pair(i1, i2));
            }

            @Override
            Constraint setup(Pair<IrIntVar, IrIntVar> s, Solver solver) {
                return ICF.arithm(toIntVar(s.getFst(), solver), "=", toIntVar(s.getSnd(), solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testNotEqual() {
        randomizedTest(new TestCase<Pair<IrIntVar, IrIntVar>>() {

            @Override
            void check(IrSolutionMap solution, Pair<IrIntVar, IrIntVar> s) {
                assertNotEquals(solution.getValue(s.getFst()), solution.getValue(s.getSnd()));
            }

            @Override
            Pair<IrBoolExpr, Pair<IrIntVar, IrIntVar>> setup(IrModule module) {
                IrIntVar i1 = randInt();
                IrIntVar i2 = randInt();
                return pair(notEqual(i1, i2), pair(i1, i2));
            }

            @Override
            Constraint setup(Pair<IrIntVar, IrIntVar> s, Solver solver) {
                return ICF.arithm(toIntVar(s.getFst(), solver), "!=", toIntVar(s.getSnd(), solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThan() {
        randomizedTest(new TestCase<Pair<IrIntVar, IrIntVar>>() {

            @Override
            void check(IrSolutionMap solution, Pair<IrIntVar, IrIntVar> s) {
                assertTrue(solution.getValue(s.getFst()) < solution.getValue(s.getSnd()));
            }

            @Override
            Pair<IrBoolExpr, Pair<IrIntVar, IrIntVar>> setup(IrModule module) {
                IrIntVar i1 = randInt();
                IrIntVar i2 = randInt();
                return pair(lessThan(i1, i2), pair(i1, i2));
            }

            @Override
            Constraint setup(Pair<IrIntVar, IrIntVar> s, Solver solver) {
                return ICF.arithm(toIntVar(s.getFst(), solver), "<", toIntVar(s.getSnd(), solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThanEqual() {
        randomizedTest(new TestCase<Pair<IrIntVar, IrIntVar>>() {

            @Override
            void check(IrSolutionMap solution, Pair<IrIntVar, IrIntVar> s) {
                assertTrue(solution.getValue(s.getFst()) <= solution.getValue(s.getSnd()));
            }

            @Override
            Pair<IrBoolExpr, Pair<IrIntVar, IrIntVar>> setup(IrModule module) {
                IrIntVar i1 = randInt();
                IrIntVar i2 = randInt();
                return pair(lessThanEqual(i1, i2), pair(i1, i2));
            }

            @Override
            Constraint setup(Pair<IrIntVar, IrIntVar> s, Solver solver) {
                return ICF.arithm(toIntVar(s.getFst(), solver), "<=", toIntVar(s.getSnd(), solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testGreaterThan() {
        randomizedTest(new TestCase<Pair<IrIntVar, IrIntVar>>() {

            @Override
            void check(IrSolutionMap solution, Pair<IrIntVar, IrIntVar> s) {
                assertTrue(solution.getValue(s.getFst()) > solution.getValue(s.getSnd()));
            }

            @Override
            Pair<IrBoolExpr, Pair<IrIntVar, IrIntVar>> setup(IrModule module) {
                IrIntVar i1 = randInt();
                IrIntVar i2 = randInt();
                return pair(greaterThan(i1, i2), pair(i1, i2));
            }

            @Override
            Constraint setup(Pair<IrIntVar, IrIntVar> s, Solver solver) {
                return ICF.arithm(toIntVar(s.getFst(), solver), ">", toIntVar(s.getSnd(), solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testGreaterThanEqual() {
        randomizedTest(new TestCase<Pair<IrIntVar, IrIntVar>>() {

            @Override
            void check(IrSolutionMap solution, Pair<IrIntVar, IrIntVar> s) {
                assertTrue(solution.getValue(s.getFst()) >= solution.getValue(s.getSnd()));
            }

            @Override
            Pair<IrBoolExpr, Pair<IrIntVar, IrIntVar>> setup(IrModule module) {
                IrIntVar i1 = randInt();
                IrIntVar i2 = randInt();
                return pair(greaterThanEqual(i1, i2), pair(i1, i2));
            }

            @Override
            Constraint setup(Pair<IrIntVar, IrIntVar> s, Solver solver) {
                return ICF.arithm(toIntVar(s.getFst(), solver), ">=", toIntVar(s.getSnd(), solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testSetEqual() {
        randomizedTest(new TestCase<Pair<IrSetVar, IrSetVar>>() {

            @Override
            void check(IrSolutionMap solution, Pair<IrSetVar, IrSetVar> s) {
                assertArrayEquals(solution.getValue(s.getFst()), solution.getValue(s.getSnd()));
            }

            @Override
            Pair<IrBoolExpr, Pair<IrSetVar, IrSetVar>> setup(IrModule module) {
                IrSetVar s1 = randSet();
                IrSetVar s2 = randSet();
                return pair(equal(s1, s2), pair(s1, s2));
            }

            @Override
            Constraint setup(Pair<IrSetVar, IrSetVar> s, Solver solver) {
                CSetVar s1 = toCSetVar(s.getFst(), solver);
                CSetVar s2 = toCSetVar(s.getSnd(), solver);
                return Constraints.equal(s1.getSet(), s1.getCard(), s2.getSet(), s2.getCard());
            }
        });
    }

    @Test(timeout = 60000)
    public void testSetNotEqual() {
        randomizedTest(new TestCase<Pair<IrSetVar, IrSetVar>>() {

            @Override
            void check(IrSolutionMap solution, Pair<IrSetVar, IrSetVar> s) {
                assertFalse(Arrays.equals(solution.getValue(s.getFst()), solution.getValue(s.getSnd())));
            }

            @Override
            Pair<IrBoolExpr, Pair<IrSetVar, IrSetVar>> setup(IrModule module) {
                IrSetVar s1 = randSet();
                IrSetVar s2 = randSet();
                return pair(notEqual(s1, s2), pair(s1, s2));
            }

            @Override
            Constraint setup(Pair<IrSetVar, IrSetVar> s, Solver solver) {
                CSetVar s1 = toCSetVar(s.getFst(), solver);
                CSetVar s2 = toCSetVar(s.getSnd(), solver);
                return Constraints.notEqual(s1.getSet(), s1.getCard(), s2.getSet(), s2.getCard());
            }
        });
    }

    @Test(timeout = 60000)
    public void testBoolChannel() {
        randomizedTest(new TestCase<Pair<IrBoolVar[], IrSetVar>>() {

            @Override
            void check(IrSolutionMap solution, Pair<IrBoolVar[], IrSetVar> s) {
                boolean[] bools = solution.getValues(s.getFst());
                int[] set = solution.getValue(s.getSnd());
                for (int i = 0; i < bools.length; i++) {
                    assertEquals(bools[i], Util.in(i, set));
                }
                for (int i : set) {
                    assertTrue(i >= 0 && i < bools.length);
                }
            }

            @Override
            Pair<IrBoolExpr, Pair<IrBoolVar[], IrSetVar>> setup(IrModule module) {
                IrBoolVar[] bools = randBools(nextInt(5));
                IrSetVar set = randPositiveSet();
                return pair(boolChannel(bools, set), pair(bools, set));
            }

            @Override
            Constraint setup(Pair<IrBoolVar[], IrSetVar> s, Solver solver) {
                return SCF.bool_channel(
                        toBoolVars(s.getFst(), solver),
                        toSetVar(s.getSnd(), solver),
                        0);
            }
        });
    }

    @Test(timeout = 60000)
    public void testIntChannel() {
        randomizedTest(new TestCase<Pair<IrIntVar[], IrSetVar[]>>() {

            @Override
            void check(IrSolutionMap solution, Pair<IrIntVar[], IrSetVar[]> s) {
                int[][] sets = solution.getValues(s.getSnd());
                int[] ints = solution.getValues(s.getFst());
                for (int i = 0; i < sets.length; i++) {
                    for (int j : sets[i]) {
                        assertTrue(j >= 0 && j < ints.length);
                        assertEquals(i, ints[j]);
                    }
                }
                for (int i = 0; i < ints.length; i++) {
                    int value = ints[i];
                    assertTrue(value >= 0 && value < sets.length);
                    assertTrue(Util.in(i, sets[ints[i]]));
                }
            }

            @Override
            Pair<IrBoolExpr, Pair<IrIntVar[], IrSetVar[]>> setup(IrModule module) {
                IrIntVar[] ints = randPositiveInts(nextInt(5) + 1);
                IrSetVar[] sets = randPositiveSets(nextInt(5) + 1);
                return pair(intChannel(ints, sets), pair(ints, sets));
            }

            @Override
            Constraint setup(Pair<IrIntVar[], IrSetVar[]> s, Solver solver) {
                return Constraints.intChannel(
                        toSetVars(s.getSnd(), solver),
                        toIntVars(s.getFst(), solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testAllDifferent() {
        randomizedTest(new TestCase<IrIntVar[]>() {

            @Override
            void check(IrSolutionMap solution, IrIntVar[] s) {
                int[] is = solution.getValues(s);
                for (int i = 0; i < is.length; i++) {
                    for (int j = i + 1; j < is.length; j++) {
                        assertNotEquals(is[i], is[j]);
                    }
                }
            }

            @Override
            Pair<IrBoolExpr, IrIntVar[]> setup(IrModule module) {
                IrIntVar[] is = randInts(1 + nextInt(5));
                return pair(allDifferent(is), is);
            }

            @Override
            Constraint setup(IrIntVar[] s, Solver solver) {
                return ICF.alldifferent(toIntVars(s, solver));
            }
        });
    }
}
