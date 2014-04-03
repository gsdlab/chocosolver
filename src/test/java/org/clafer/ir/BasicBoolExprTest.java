package org.clafer.ir;

import org.clafer.choco.constraint.Constraints;
import static org.clafer.ir.Irs.*;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.ICF;
import solver.constraints.LCF;
import solver.constraints.set.SCF;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
public class BasicBoolExprTest extends IrTest {

    @Test(timeout = 60000)
    public void testNot() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrBoolVar var) {
                return not(var);
            }

            Constraint setup(Solver solver, BoolVar var) {
                return ICF.arithm(var, "=", 0);
            }
        });
    }

    @Test(timeout = 60000)
    public void testAnd() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrBoolVar[] vars) {
                return and(vars);
            }

            Constraint setup(Solver solver, BoolVar[] vars) {
                return vars.length == 0
                        ? solver.TRUE
                        : Constraints.and(vars);
            }
        });
    }

    @Test(timeout = 60000)
    public void testLone() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrBoolVar[] vars) {
                return lone(vars);
            }

            Constraint setup(Solver solver, BoolVar[] vars) {
                return vars.length == 0
                        ? solver.TRUE
                        : Constraints.lone(vars);
            }
        });
    }

    @Test(timeout = 60000)
    public void testOne() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrBoolVar[] vars) {
                return one(vars);
            }

            Constraint setup(Solver solver, BoolVar[] vars) {
                return vars.length == 0
                        ? solver.FALSE
                        : Constraints.one(vars);
            }
        });
    }

    @Test(timeout = 60000)
    public void testOr() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrBoolVar[] vars) {
                return or(vars);
            }

            Constraint setup(Solver solver, BoolVar[] vars) {
                return vars.length == 0
                        ? solver.FALSE
                        : Constraints.or(vars);
            }
        });
    }

    @Test(timeout = 60000)
    public void testIfThenElse() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrBoolVar antecedent, IrBoolVar consequent, IrBoolVar alternative) {
                return ifThenElse(antecedent, consequent, alternative);
            }

            Constraint setup(BoolVar antecedent, BoolVar consequent, BoolVar alternative) {
                return Constraints.ifThenElse(antecedent, consequent, alternative);
            }
        });
    }

    @Test(timeout = 60000)
    public void testImplies() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrBoolVar antecedent, IrBoolVar consequent) {
                return implies(antecedent, consequent);
            }

            Constraint setup(BoolVar antecedent, BoolVar consequent) {
                return ICF.arithm(antecedent, "<=", consequent);
            }
        });
    }

    @Test(timeout = 60000)
    public void testNotImplies() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrBoolVar antecedent, IrBoolVar consequent) {
                return notImplies(antecedent, consequent);
            }

            Constraint setup(BoolVar antecedent, BoolVar consequent) {
                return ICF.arithm(antecedent, ">", consequent);
            }
        });
    }

    @Test(timeout = 60000)
    public void testIfOnlyIf() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrBoolVar var1, IrBoolVar var2) {
                return ifOnlyIf(var1, var2);
            }

            Constraint setup(BoolVar var1, BoolVar var2) {
                return ICF.arithm(var1, "=", var2);
            }
        });
    }

    @Test(timeout = 60000)
    public void testXor() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrBoolVar var1, IrBoolVar var2) {
                return xor(var1, var2);
            }

            Constraint setup(BoolVar var1, BoolVar var2) {
                return ICF.arithm(var1, "!=", var2);
            }
        });
    }

    @Test(timeout = 60000)
    public void testEqual() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar var1, IrIntVar var2) {
                return equal(var1, var2);
            }

            Constraint setup(IntVar var1, IntVar var2) {
                return ICF.arithm(var1, "=", var2);
            }
        });
    }

    @Test(timeout = 60000)
    public void testNotEqual() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar var1, IrIntVar var2) {
                return notEqual(var1, var2);
            }

            Constraint setup(IntVar var1, IntVar var2) {
                return ICF.arithm(var1, "!=", var2);
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThan() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar var1, IrIntVar var2) {
                return lessThan(var1, var2);
            }

            Constraint setup(IntVar var1, IntVar var2) {
                return ICF.arithm(var1, "<", var2);
            }
        });
    }

    @Test(timeout = 60000)
    public void testLessThanEqual() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar var1, IrIntVar var2) {
                return lessThanEqual(var1, var2);
            }

            Constraint setup(IntVar var1, IntVar var2) {
                return ICF.arithm(var1, "<=", var2);
            }
        });
    }

    @Test(timeout = 60000)
    public void testGreaterThan() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar var1, IrIntVar var2) {
                return greaterThan(var1, var2);
            }

            Constraint setup(IntVar var1, IntVar var2) {
                return ICF.arithm(var1, ">", var2);
            }
        });
    }

    @Test(timeout = 60000)
    public void testGreaterThanEqual() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar var1, IrIntVar var2) {
                return greaterThanEqual(var1, var2);
            }

            Constraint setup(IntVar var1, IntVar var2) {
                return ICF.arithm(var1, ">=", var2);
            }
        });
    }

    @Test(timeout = 60000)
    public void testSetEqual() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrSetVar var1, IrSetVar var2) {
                return equal(var1, var2);
            }

            Constraint setup(CSetVar var1, CSetVar var2) {
                return Constraints.equal(var1.getSet(), var1.getCard(), var2.getSet(), var2.getCard());
            }
        });
    }

    @Test(timeout = 60000)
    public void testSetNotEqual() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrSetVar var1, IrSetVar var2) {
                return notEqual(var1, var2);
            }

            Constraint setup(CSetVar var1, CSetVar var2) {
                return Constraints.notEqual(var1.getSet(), var1.getCard(), var2.getSet(), var2.getCard());
            }
        });
    }

    @Test(timeout = 60000)
    public void testBoolChannel() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrBoolVar[] bools, IrSetVar set) {
                return boolChannel(bools, set);
            }

            Constraint setup(BoolVar[] bools, SetVar set) {
                return SCF.bool_channel(bools, set, 0);
            }
        });
    }

    @Test(timeout = 60000)
    public void testIntChannel() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar[] ints, IrSetVar[] sets) {
                return intChannel(ints, sets);
            }

            Constraint setup(Solver solver, IntVar[] ints, SetVar[] sets) {
                return ints.length == 0 && sets.length == 0
                        ? solver.TRUE
                        : Constraints.intChannel(sets, ints);
            }
        });
    }

    @Test(timeout = 60000)
    public void testSort() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar[] ints) {
                return sort(ints);
            }

            Constraint setup(Solver solver, IntVar[] ints) {
                if (ints.length == 0) {
                    return solver.TRUE;
                }
                Constraint[] sorted = new Constraint[ints.length - 1];
                for (int i = 0; i < sorted.length; i++) {
                    sorted[i] = ICF.arithm(ints[i], "<=", ints[i + 1]);
                }
                return sorted.length == 0 ? solver.TRUE : LCF.and(sorted);
            }
        });
    }

    @Test(timeout = 60000)
    public void testSortStrict() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar[] ints) {
                return sortStrict(ints);
            }

            Constraint setup(Solver solver, IntVar[] ints) {
                if (ints.length == 0) {
                    return solver.TRUE;
                }
                Constraint[] sorted = new Constraint[ints.length - 1];
                for (int i = 0; i < sorted.length; i++) {
                    sorted[i] = ICF.arithm(ints[i], "<", ints[i + 1]);
                }
                return sorted.length == 0 ? solver.TRUE : LCF.and(sorted);
            }
        });
    }

    @Test(timeout = 60000)
    public void testSortStrings() {
        randomizedTest2(new TestCaseByConvention() {

            @Override
            protected Object[] initializeVariables() {
                int length = 1 + nextInt(3);
                IrIntVar[][] strings = new IrIntVar[nextInt(3)][];
                for (int i = 0; i < strings.length; i++) {
                    strings[i] = randInts(length);
                }
                return new Object[]{strings};
            }

            IrBoolExpr setup(IrIntVar[][] strings) {
                return sort(strings);
            }

            Constraint setup(Solver solver, IntVar[][] strings) {
                return strings.length == 0
                        ? solver.TRUE
                        : ICF.lex_chain_less_eq(strings);
            }
        });
    }

    @Test(timeout = 60000)
    public void testSortStringsStrict() {
        randomizedTest2(new TestCaseByConvention() {

            @Override
            protected Object[] initializeVariables() {
                int length = 1 + nextInt(3);
                IrIntVar[][] strings = new IrIntVar[nextInt(3)][];
                for (int i = 0; i < strings.length; i++) {
                    strings[i] = randInts(length);
                }
                return new Object[]{strings};
            }

            IrBoolExpr setup(IrIntVar[][] strings) {
                return sortStrict(strings);
            }

            Constraint setup(Solver solver, IntVar[][] strings) {
                return strings.length == 0
                        ? solver.TRUE
                        : ICF.lex_chain_less(strings);
            }
        });
    }

    // TODO: sortChannel
    @Test(timeout = 60000)
    public void testAllDifferent() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar[] ints) {
                return allDifferent(ints);
            }

            Constraint setup(Solver solver, IntVar[] ints) {
                return ints.length == 0
                        ? solver.TRUE
                        : ICF.alldifferent(ints);
            }
        });
    }

    @Test(timeout = 60000)
    public void testSelectN() {
        randomizedTest2(new TestCaseByConvention() {

            IrBoolExpr setup(IrBoolVar[] bools, IrIntVar n) {
                return selectN(bools, n);
            }

            Constraint setup(BoolVar[] bools, IntVar n) {
                return Constraints.selectN(bools, n);
            }
        });
    }
}
