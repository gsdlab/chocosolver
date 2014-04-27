package org.clafer.ir;

import static org.clafer.test.TestUtil.*;
import org.clafer.choco.constraint.Constraints;
import static org.clafer.ir.Irs.*;
import org.clafer.test.NonEmpty;
import org.clafer.test.Positive;
import org.junit.Test;
import solver.Solver;
import solver.constraints.Constraint;
import solver.constraints.ICF;
import solver.constraints.LCF;
import solver.constraints.set.SCF;
import solver.variables.BoolVar;
import solver.variables.CSetVar;
import solver.variables.IntVar;
import solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
public class BasicBoolExprTest extends IrTest {

    @Test(timeout = 60000)
    public void testNot() {
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

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
    public void testImplies() {
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrBoolVar antecedent, IrBoolVar consequent) {
                return notImplies(antecedent, consequent);
            }

            Constraint setup(BoolVar antecedent, BoolVar consequent) {
                return ICF.arithm(antecedent, ">", consequent);
            }
        });
    }

    @Test(timeout = 60000)
    public void testIfThenElse() {
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrBoolVar antecedent, IrBoolVar consequent, IrBoolVar alternative) {
                return ifThenElse(antecedent, consequent, alternative);
            }

            Constraint setup(BoolVar antecedent, BoolVar consequent, BoolVar alternative) {
                return Constraints.ifThenElse(antecedent, consequent, alternative);
            }
        });
    }

    @Test(timeout = 60000)
    public void testIfOnlyIf() {
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrBoolVar var1, IrBoolVar var2) {
                return xor(var1, var2);
            }

            Constraint setup(BoolVar var1, BoolVar var2) {
                return ICF.arithm(var1, "!=", var2);
            }
        });
    }

    @Test(timeout = 60000)
    public void testWithin() {
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar value, @NonEmpty IrDomain range) {
                return within(value, range);
            }

            Constraint setup(IntVar value, int[] range) {
                return ICF.member(value, range);
            }
        });
    }

    @Test(timeout = 60000)
    public void testNotWithin() {
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar value, @NonEmpty IrDomain range) {
                return notWithin(value, range);
            }

            Constraint setup(IntVar value, int[] range) {
                return ICF.not_member(value, range);
            }
        });
    }

    @Test(timeout = 60000)
    public void testEqual() {
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrSetVar var1, IrSetVar var2) {
                return notEqual(var1, var2);
            }

            Constraint setup(CSetVar var1, CSetVar var2) {
                return Constraints.notEqual(var1.getSet(), var1.getCard(), var2.getSet(), var2.getCard());
            }
        });
    }

    @Test(timeout = 60000)
    public void testMember() {
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar element, IrSetVar set) {
                return member(element, set);
            }

            Constraint setup(IntVar element, SetVar set) {
                return SCF.member(element, set);
            }
        });
    }

    @Test(timeout = 60000)
    public void testNotMember() {
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar element, IrSetVar set) {
                return notMember(element, set);
            }

            Constraint setup(IntVar element, SetVar set) {
                return SCF.member(element, set).getOpposite();
            }
        });
    }

    @Test(timeout = 60000)
    public void testSubsetEq() {
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrSetVar subset, IrSetVar superSet) {
                return subsetEq(subset, superSet);
            }

            Constraint setup(SetVar subset, SetVar superSet) {
                return SCF.subsetEq(new SetVar[]{subset, superSet});
            }
        });
    }

    @Test(timeout = 60000)
    public void testBoolChannel() {
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

            @Override
            protected Object[] initializeVariables() {
                int length = randInt(1, 3);
                IrIntVar[][] strings = new IrIntVar[randInt(0, 2)][length];
                for (IrIntVar[] string : strings) {
                    for (int j = 0; j < string.length; j++) {
                        string[j] = randIrIntVar();
                    }
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
        randomizedTest(new TestCaseByConvention() {

            @Override
            protected Object[] initializeVariables() {
                int length = randInt(1, 3);
                IrIntVar[][] strings = new IrIntVar[randInt(0, 2)][length];
                for (IrIntVar[] string : strings) {
                    for (int j = 0; j < string.length; j++) {
                        string[j] = randIrIntVar();
                    }
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
        randomizedTest(new TestCaseByConvention() {

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
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrBoolVar[] bools, IrIntVar n) {
                return selectN(bools, n);
            }

            Constraint setup(BoolVar[] bools, IntVar n) {
                return Constraints.selectN(bools, n);
            }
        });
    }

    @Test(timeout = 60000)
    public void testAcyclic() {
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(IrIntVar[] edges) {
                return acyclic(edges);
            }

            Constraint setup(Solver solver, IntVar[] edges) {
                return edges.length == 0
                        ? solver.TRUE
                        : Constraints.acyclic(edges);
            }
        });
    }

    @Test(timeout = 60000)
    public void testUnreachable() {
        randomizedTest(new TestCaseByConvention() {

            IrBoolExpr setup(@NonEmpty IrIntVar[] edges, @Positive int from, @Positive int to) {
                return from >= edges.length || to >= edges.length
                        ? False
                        : unreachable(edges, from, to);
            }

            Constraint setup(Solver solver, IntVar[] edges, int from, int to) {
                return from >= edges.length || to >= edges.length
                        ? solver.FALSE
                        : Constraints.unreachable(edges, from, to);
            }
        });
    }
}
