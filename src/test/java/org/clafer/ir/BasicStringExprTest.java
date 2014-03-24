package org.clafer.ir;

import org.clafer.choco.constraint.Constraints;
import static org.clafer.ir.Irs.*;
import org.clafer.ir.compiler.IrSolutionMap;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;
import org.junit.Test;
import solver.Solver;
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
        randomizedTest(new TestCase() {
            @IrVarField
            IrStringVar left;
            @IrVarField
            IrStringVar right;

            @Override
            void check(IrSolutionMap solution) {
                assertEquals(
                        solution.getValue(left),
                        solution.getValue(right));
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return equal(left, right);
            }

            @Override
            Constraint setup(Solver solver) {
                CStringVar LEFT = toVar(left, solver);
                CStringVar RIGHT = toVar(right, solver);
                return Constraints.equal(
                        LEFT.getChars(), LEFT.getLength(),
                        RIGHT.getChars(), RIGHT.getLength());
            }
        });
    }

    @Test(timeout = 60000)
    public void testLength() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrIntVar length;
            @IrVarField
            IrStringVar word;

            @Override
            void check(IrSolutionMap solution) {
                assertEquals(
                        solution.getValue(length),
                        solution.getValue(word).length());
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return equal(length, length(word));
            }

            @Override
            Constraint setup(Solver solver) {
                IntVar LENGTH = toVar(length, solver);
                CStringVar WORD = toVar(word, solver);
                return ICF.arithm(LENGTH, "=", WORD.getLength());
            }
        });
    }

    @Test(timeout = 60000)
    public void testPrefix() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrStringVar prefix;
            @IrVarField
            IrStringVar word;

            @Override
            void check(IrSolutionMap solution) {
                assertThat(
                        solution.getValue(word),
                        startsWith(solution.getValue(prefix)));
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return prefix(prefix, word);
            }

            @Override
            Constraint setup(Solver solver) {
                CStringVar PREFIX = toVar(prefix, solver);
                CStringVar WORD = toVar(word, solver);
                return Constraints.prefix(
                        PREFIX.getChars(), PREFIX.getLength(),
                        WORD.getChars(), WORD.getLength());
            }
        });
    }

    @Test(timeout = 60000)
    public void testSuffix() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrStringVar suffix;
            @IrVarField
            IrStringVar word;

            @Override
            void check(IrSolutionMap solution) {
                assertThat(
                        solution.getValue(word),
                        endsWith(solution.getValue(suffix)));
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return suffix(suffix, word);
            }

            @Override
            Constraint setup(Solver solver) {
                CStringVar SUFFIX = toVar(suffix, solver);
                CStringVar WORD = toVar(word, solver);
                return Constraints.suffix(
                        SUFFIX.getChars(), SUFFIX.getLength(),
                        WORD.getChars(), WORD.getLength());
            }
        });
    }

    @Test(timeout = 60000)
    public void testConcat() {
        randomizedTest(new TestCase() {
            @IrVarField
            IrStringVar concat;
            @IrVarField
            IrStringVar left;
            @IrVarField
            IrStringVar right;

            @Override
            void check(IrSolutionMap solution) {
                assertEquals(solution.getValue(concat),
                        solution.getValue(left) + solution.getValue(right));
            }

            @Override
            IrBoolExpr setup(IrModule module) {
                return equal(concat, concat(left, right));
            }

            @Override
            Constraint setup(Solver solver) {
                CStringVar CONCAT = toVar(concat, solver);
                CStringVar LEFT = toVar(left, solver);
                CStringVar RIGHT = toVar(right, solver);
                return Constraints.concat(
                        LEFT.getChars(), LEFT.getLength(),
                        RIGHT.getChars(), RIGHT.getLength(),
                        CONCAT.getChars(), CONCAT.getLength());
            }
        });
    }
}
