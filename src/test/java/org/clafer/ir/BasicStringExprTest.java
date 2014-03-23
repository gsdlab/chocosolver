package org.clafer.ir;

import static org.clafer.ClaferTest.toCStringVar;
import static org.clafer.ClaferTest.toIntVar;
import org.clafer.choco.constraint.Constraints;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
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
        randomizedTest(new TestCase<Pair<IrStringVar, IrStringVar>>() {

            @Override
            void check(IrSolutionMap solution, Pair<IrStringVar, IrStringVar> s) {
                assertEquals(
                        solution.getValue(s.getFst()),
                        solution.getValue(s.getSnd()));
            }

            @Override
            Pair<IrBoolExpr, Pair<IrStringVar, IrStringVar>> setup(IrModule module) {
                IrStringVar left = randString();
                IrStringVar right = randString();
                return pair(equal(left, right), pair(left, right));
            }

            @Override
            Constraint setup(Pair<IrStringVar, IrStringVar> s, Solver solver) {
                CStringVar left = toCStringVar(s.getFst(), solver);
                CStringVar right = toCStringVar(s.getSnd(), solver);
                return Constraints.equal(
                        left.getChars(), left.getLength(),
                        right.getChars(), right.getLength());
            }
        });
    }

    @Test(timeout = 60000)
    public void testLength() {
        randomizedTest(new TestCase<Pair<IrIntVar, IrStringVar>>() {

            @Override
            void check(IrSolutionMap solution, Pair<IrIntVar, IrStringVar> s) {
                assertEquals(
                        solution.getValue(s.getFst()),
                        solution.getValue(s.getSnd()).length());
            }

            @Override
            Pair<IrBoolExpr, Pair<IrIntVar, IrStringVar>> setup(IrModule module) {
                IrIntVar length = randInt();
                IrStringVar word = randString();
                return pair(equal(length, length(word)), pair(length, word));
            }

            @Override
            Constraint setup(Pair<IrIntVar, IrStringVar> s, Solver solver) {
                IntVar length = toIntVar(s.getFst(), solver);
                CStringVar word = toCStringVar(s.getSnd(), solver);
                return ICF.arithm(length, "=", word.getLength());
            }
        });
    }

    @Test(timeout = 60000)
    public void testPrefix() {
        randomizedTest(new TestCase<Pair<IrStringVar, IrStringVar>>() {

            @Override
            void check(IrSolutionMap solution, Pair<IrStringVar, IrStringVar> s) {
                assertThat(
                        solution.getValue(s.getSnd()),
                        startsWith(solution.getValue(s.getFst())));
            }

            @Override
            Pair<IrBoolExpr, Pair<IrStringVar, IrStringVar>> setup(IrModule module) {
                IrStringVar prefix = randString();
                IrStringVar word = randString();
                return pair(prefix(prefix, word), pair(prefix, word));
            }

            @Override
            Constraint setup(Pair<IrStringVar, IrStringVar> s, Solver solver) {
                CStringVar prefix = toCStringVar(s.getFst(), solver);
                CStringVar word = toCStringVar(s.getSnd(), solver);
                return Constraints.prefix(
                        prefix.getChars(), prefix.getLength(),
                        word.getChars(), word.getLength());
            }
        });
    }

    @Test(timeout = 60000)
    public void testSuffix() {
        randomizedTest(new TestCase<Pair<IrStringVar, IrStringVar>>() {

            @Override
            void check(IrSolutionMap solution, Pair<IrStringVar, IrStringVar> s) {
                assertThat(
                        solution.getValue(s.getSnd()),
                        endsWith(solution.getValue(s.getFst())));
            }

            @Override
            Pair<IrBoolExpr, Pair<IrStringVar, IrStringVar>> setup(IrModule module) {
                IrStringVar suffix = randString();
                IrStringVar word = randString();
                return pair(suffix(suffix, word), pair(suffix, word));
            }

            @Override
            Constraint setup(Pair<IrStringVar, IrStringVar> s, Solver solver) {
                CStringVar suffix = toCStringVar(s.getFst(), solver);
                CStringVar word = toCStringVar(s.getSnd(), solver);
                return Constraints.suffix(
                        suffix.getChars(), suffix.getLength(),
                        word.getChars(), word.getLength());
            }
        });
    }

    @Test(timeout = 60000)
    public void testConcat() {
        randomizedTest(new TestCase<Triple<IrStringVar, IrStringVar, IrStringVar>>() {

            @Override
            void check(IrSolutionMap solution, Triple<IrStringVar, IrStringVar, IrStringVar> s) {
                assertEquals(solution.getValue(s.getFst()),
                        solution.getValue(s.getSnd()) + solution.getValue(s.getThd()));
            }

            @Override
            Pair<IrBoolExpr, Triple<IrStringVar, IrStringVar, IrStringVar>> setup(IrModule module) {
                IrStringVar concat = randString();
                IrStringVar left = randString();
                IrStringVar right = randString();
                return pair(equal(concat, concat(left, right)),
                        triple(concat, left, right));
            }

            @Override
            Constraint setup(Triple<IrStringVar, IrStringVar, IrStringVar> s, Solver solver) {
                CStringVar concat = toCStringVar(s.getFst(), solver);
                CStringVar left = toCStringVar(s.getSnd(), solver);
                CStringVar right = toCStringVar(s.getThd(), solver);
                return Constraints.concat(
                        left.getChars(), left.getLength(),
                        right.getChars(), right.getLength(),
                        concat.getChars(), concat.getLength());
            }
        });
    }
}
