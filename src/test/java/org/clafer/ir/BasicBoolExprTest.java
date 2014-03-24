package org.clafer.ir;

import java.util.Arrays;
import org.clafer.choco.constraint.Constraints;
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
        randomizedTest(new TestCase() {
            @IrVarField
            IrBoolVar[] vars;

            @Override
            void check(IrSolutionMap solution) {
                for (IrBoolVar var : vars) {
                    assertTrue(solution.getValue(var));
                }
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return and(vars);
            }

            @Override
            Constraint setup(Solver solver) {
                return vars.length == 0
                        ? solver.TRUE
                        : Constraints.and(toVars(vars, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testLone() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrBoolVar[] vars;

            @Override
            void check(IrSolutionMap solution) {
                assertTrue(Util.sum(solution.getValues(vars)) <= 1);
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return lone(vars);
            }

            @Override
            Constraint setup(Solver solver) {
                return vars.length == 0
                        ? solver.TRUE
                        : Constraints.lone(toVars(vars, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testOne() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrBoolVar[] vars;

            @Override
            void check(IrSolutionMap solution) {
                assertEquals(1, Util.sum(solution.getValues(vars)));
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return one(vars);
            }

            @Override
            Constraint setup(Solver solver) {
                return vars.length == 0
                        ? solver.FALSE
                        : Constraints.one(toVars(vars, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testOr() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrBoolVar[] vars;

            @Override
            void check(IrSolutionMap solution) {
                assertTrue(Util.sum(solution.getValues(vars)) >= 1);
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return or(vars);
            }

            @Override
            Constraint setup(Solver solver) {
                return vars.length == 0
                        ? solver.FALSE
                        : Constraints.or(toVars(vars, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testIfThenElse() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrBoolVar antecedent;
            @IrVarField
            IrBoolVar consequent;
            @IrVarField
            IrBoolVar alternative;

            @Override
            void check(IrSolutionMap solution) {
                assertTrue(solution.getValue(antecedent)
                        ? solution.getValue(consequent)
                        : solution.getValue(alternative));
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return ifThenElse(antecedent, consequent, alternative);
            }

            @Override
            Constraint setup(Solver solver) {
                return Constraints.ifThenElse(
                        toVar(antecedent, solver),
                        toVar(consequent, solver),
                        toVar(alternative, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testEqual() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrIntVar var1;
            @IrVarField
            IrIntVar var2;

            @Override
            void check(IrSolutionMap solution) {
                assertEquals(solution.getValue(var1), solution.getValue(var2));
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return equal(var1, var2);
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.arithm(toVar(var1, solver), "=", toVar(var2, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testNotEqual() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrIntVar var1;
            @IrVarField
            IrIntVar var2;

            @Override
            void check(IrSolutionMap solution) {
                assertNotEquals(solution.getValue(var1), solution.getValue(var2));
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return notEqual(var1, var2);
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.arithm(toVar(var1, solver), "!=", toVar(var2, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThan() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrIntVar var1;
            @IrVarField
            IrIntVar var2;

            @Override
            void check(IrSolutionMap solution) {
                assertTrue(solution.getValue(var1) < solution.getValue(var2));
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return lessThan(var1, var2);
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.arithm(toVar(var1, solver), "<", toVar(var2, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThanEqual() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrIntVar var1;
            @IrVarField
            IrIntVar var2;

            @Override
            void check(IrSolutionMap solution) {
                assertTrue(solution.getValue(var1) <= solution.getValue(var2));
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return lessThanEqual(var1, var2);
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.arithm(toVar(var1, solver), "<=", toVar(var2, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testGreaterThan() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrIntVar var1;
            @IrVarField
            IrIntVar var2;

            @Override
            void check(IrSolutionMap solution) {
                assertTrue(solution.getValue(var1) > solution.getValue(var2));
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return greaterThan(var1, var2);
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.arithm(toVar(var1, solver), ">", toVar(var2, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testGreaterThanEqual() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrIntVar var1;
            @IrVarField
            IrIntVar var2;

            @Override
            void check(IrSolutionMap solution) {
                assertTrue(solution.getValue(var1) >= solution.getValue(var2));
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return greaterThanEqual(var1, var2);
            }

            @Override
            Constraint setup(Solver solver) {
                return ICF.arithm(toVar(var1, solver), ">=", toVar(var2, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testSetEqual() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrSetVar var1;
            @IrVarField
            IrSetVar var2;

            @Override
            void check(IrSolutionMap solution) {
                assertArrayEquals(solution.getValue(var1), solution.getValue(var2));
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return equal(var1, var2);
            }

            @Override
            Constraint setup(Solver solver) {
                CSetVar VAR1 = toVar(var1, solver);
                CSetVar VAR2 = toVar(var2, solver);
                return Constraints.equal(VAR1.getSet(), VAR1.getCard(), VAR2.getSet(), VAR2.getCard());
            }
        });
    }

    @Test(timeout = 60000)
    public void testSetNotEqual() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrSetVar var1;
            @IrVarField
            IrSetVar var2;

            @Override
            void check(IrSolutionMap solution) {
                assertFalse(Arrays.equals(solution.getValue(var1), solution.getValue(var2)));
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return notEqual(var1, var2);
            }

            @Override
            Constraint setup(Solver solver) {
                CSetVar VAR1 = toVar(var1, solver);
                CSetVar VAR2 = toVar(var2, solver);
                return Constraints.notEqual(VAR1.getSet(), VAR1.getCard(), VAR2.getSet(), VAR2.getCard());
            }
        });
    }

    @Test(timeout = 60000)
    public void testBoolChannel() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrBoolVar[] bools;
            @IrVarField
            IrSetVar set;

            @Override
            void check(IrSolutionMap solution) {
                boolean[] BOOLS = solution.getValues(bools);
                int[] SET = solution.getValue(set);
                for (int i = 0; i < BOOLS.length; i++) {
                    assertEquals(BOOLS[i], Util.in(i, SET));
                }
                for (int i : SET) {
                    assertTrue(i >= 0 && i < BOOLS.length);
                }
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return boolChannel(bools, set);
            }

            @Override
            Constraint setup(Solver solver) {
                return SCF.bool_channel(
                        toVars(bools, solver),
                        toSetVar(set, solver),
                        0);
            }
        });
    }

    @Test(timeout = 60000)
    public void testIntChannel() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrIntVar[] ints;
            @IrVarField
            IrSetVar[] sets;

            @Override
            void check(IrSolutionMap solution) {
                int[] INTS = solution.getValues(ints);
                int[][] SETS = solution.getValues(sets);
                for (int i = 0; i < SETS.length; i++) {
                    for (int j : SETS[i]) {
                        assertTrue(j >= 0 && j < INTS.length);
                        assertEquals(i, INTS[j]);
                    }
                }
                for (int i = 0; i < INTS.length; i++) {
                    int value = INTS[i];
                    assertTrue(value >= 0 && value < SETS.length);
                    assertTrue(Util.in(i, SETS[INTS[i]]));
                }
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return intChannel(ints, sets);
            }

            @Override
            Constraint setup(Solver solver) {
                return ints.length == 0 && sets.length == 0
                        ? solver.TRUE
                        : Constraints.intChannel(
                                toSetVars(sets, solver),
                                toVars(ints, solver));
            }
        });
    }

    @Test(timeout = 60000)
    public void testAllDifferent() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrIntVar[] ints;

            @Override
            void check(IrSolutionMap solution) {
                int[] INTS = solution.getValues(ints);
                for (int i = 0; i < INTS.length; i++) {
                    for (int j = i + 1; j < INTS.length; j++) {
                        assertNotEquals(INTS[i], INTS[j]);
                    }
                }
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return allDifferent(ints);
            }

            @Override
            Constraint setup(Solver solver) {
                return ints.length == 0
                        ? solver.TRUE
                        : ICF.alldifferent(toVars(ints, solver));
            }
        });
    }
}
