package org.clafer.choco.constraint;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.clafer.ClaferTest;
import org.clafer.Sample;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import org.clafer.ir.IrVar;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import solver.Solver;
import solver.constraints.Constraint;
import util.ESat;

/**
 *
 * @param <T> type of test input
 * @author jimmy
 */
public abstract class ConstraintTest<T> extends ClaferTest {

    protected abstract void check(T s);

    protected void checkNot(T s) {
        try {
            check(s);
        } catch (AssertionError e) {
            return;
        }
        fail();
    }

    private void check(boolean positive, Constraint constraint, T s) {
        try {
            if (positive) {
                check(s);
            } else {
                checkNot(s);
            }
        } catch (AssertionError e) {
            throw new AssertionError("Incorrect solution: " + constraint, e);
        }
    }

    protected void randomizedTest(TestCase<T> testCase) {
        try {
            Method method = testCase.getClass().getMethod("setup", Solver.class);
            Sample sample = method.getAnnotation(Sample.class);
            int repeat = sample == null ? Sample.Default : sample.value();
            PositiveSolutions positive = method.getAnnotation(PositiveSolutions.class);
            for (int i = 0; i < repeat; i++) {
                if (positive == null) {
                    randomizedTest(testCase, true);
                } else {
                    randomizedTest(testCase, true, positive.value());
                }
            }
            NegativeSolutions negative = method.getAnnotation(NegativeSolutions.class);
            for (int i = 0; i < repeat; i++) {
                if (negative == null) {
                    randomizedTest(testCase, false);
                } else {
                    randomizedTest(testCase, false, negative.value());
                }
            }
        } catch (NoSuchMethodException e) {
            throw new Error(e);
        }
    }

    private void randomizedTest(TestCase<T> testCase, boolean positive) {
        Solver solver = new Solver();

        Pair<Constraint, T> pair = testCase.setup(solver);

        Constraint constraint = positive ? pair.getFst() : pair.getFst().getOpposite();
        T t = pair.getSnd();
        solver.post(constraint);

        randomizeStrategy(solver);
        ESat entailed = isEntailed(constraint);
        if (ESat.FALSE.equals(entailed)) {
            if (solver.findSolution()) {
                fail("Did not expect a solution, found " + constraint);
            }
        } else if (solver.findSolution()) {
            check(positive, constraint, t);
            for (int solutions = 1; solutions < 10 && solver.nextSolution(); solutions++) {
                check(positive, constraint, t);
            }
        } else if (ESat.TRUE.equals(entailed)) {
            fail("Expected at least one solution, " + constraint);
        }
    }

    private void randomizedTest(TestCase<T> testCase,
            boolean positive,
            int expectedNumberSolutions) {
        Solver solver = new Solver();

        Pair<Constraint, T> pair = testCase.setup(solver);

        Constraint constraint = positive ? pair.getFst() : pair.getFst().getOpposite();
        T t = pair.getSnd();
        solver.post(constraint);

        int count = 0;
        if (randomizeStrategy(solver).findSolution()) {
            do {
                check(positive, constraint, t);
                count++;
            } while (solver.nextSolution());
        }
        assertEquals(expectedNumberSolutions, count);
    }

    protected static interface TestCase<T> {

        Pair<Constraint, T> setup(Solver solver);
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.METHOD)
    protected static @interface PositiveSolutions {

        /**
         * @return the expected number of solutions that satisfy the constraint
         */
        int value();
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.METHOD)
    protected static @interface NegativeSolutions {

        /**
         * @return the expected number of solutions that violate the constraint
         */
        int value();
    }
}
