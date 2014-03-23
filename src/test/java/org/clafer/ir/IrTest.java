package org.clafer.ir;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import org.clafer.ClaferTest;
import org.clafer.Sample;
import org.clafer.collection.Pair;
import org.clafer.collection.Triple;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import solver.Solver;
import solver.constraints.Constraint;

/**
 *
 * @author jimmy
 */
public class IrTest extends ClaferTest {

    protected <T> void randomizedTest(TestCase<T> testCase) {
        try {
            Method method = TestCase.class.getDeclaredMethod("setup", IrModule.class);
            Sample sample = method.getAnnotation(Sample.class);
            int repeat = sample == null ? Sample.Default : sample.value();
            for (int i = 0; i < repeat; i++) {
                randomizedTest(testCase, true);
            }
            for (int i = 0; i < repeat; i++) {
                randomizedTest(testCase, false);
            }
        } catch (NoSuchMethodException e) {
            throw new Error(e);
        }
    }

    private <T> void randomizedTest(
            TestCase<T> testCase,
            boolean positive) {
        IrModule module = new IrModule();
        Solver irSolver = new Solver();

        Pair<IrBoolExpr, T> pair = testCase.setup(module);
        IrBoolExpr irConstraint = positive ? pair.getFst() : pair.getFst().negate();
        T s = pair.getSnd();
        module.addConstraint(irConstraint);
        module.addVariables(getVariables(s));

        IrSolutionMap solution = IrCompiler.compile(module, irSolver);
        testCase.validateTranslation(irSolver);

        int count = 0;
        if (randomizeStrategy(irSolver).findSolution()) {
            do {
                testCase.check(positive, irConstraint, solution, s);
                count++;
            } while (irSolver.nextSolution());
        }

        Solver solver = new Solver();
        Constraint constraint = positive
                ? testCase.setup(s, solver)
                : testCase.setup(s, solver).getOpposite();
        solver.post(constraint);

        assertEquals(randomizeStrategy(solver).findAllSolutions(), count);
    }

    private static List<IrVar> getVariables(Object o) {
        List<IrVar> variables = new ArrayList<>();
        getVariables(o, variables);
        return variables;
    }

    private static void getVariables(Object o, List<IrVar> variables) {
        if (o instanceof IrVar) {
            variables.add((IrVar) o);
        } else if (o instanceof Pair) {
            Pair<?, ?> pair = (Pair<?, ?>) o;
            getVariables(pair.getFst(), variables);
            getVariables(pair.getSnd(), variables);
        } else if (o instanceof Triple) {
            Triple<?, ?, ?> triple = (Triple<?, ?, ?>) o;
            getVariables(triple.getFst(), variables);
            getVariables(triple.getSnd(), variables);
            getVariables(triple.getThd(), variables);
        } else if (o instanceof Object[]) {
            for (Object p : ((Object[]) o)) {
                getVariables(p, variables);
            }
        } else if (o instanceof Iterable<?>) {
            for (Object p : ((Iterable<?>) o)) {
                getVariables(p, variables);
            }
        } else {
            for (Field field : o.getClass().getDeclaredFields()) {
                if (field.isAnnotationPresent(IrVarField.class)) {
                    field.setAccessible(true);
                    try {
                        getVariables(field.get(o), variables);
                    } catch (IllegalAccessException e) {
                        throw new Error(e);
                    }
                }
            }
        }
    }

    protected static abstract class TestCase<T> {

        abstract void check(IrSolutionMap solution, T s);

        protected void checkNot(IrSolutionMap solution, T s) {
            try {
                check(solution, s);
            } catch (AssertionError e) {
                return;
            }
            fail();
        }

        private void check(
                boolean positive, IrBoolExpr constraint,
                IrSolutionMap solution, T s) {
            try {
                if (positive) {
                    check(solution, s);
                } else {
                    checkNot(solution, s);
                }
            } catch (AssertionError e) {
                throw new AssertionError("Incorrect solution: " + constraint, e);
            }
        }

        void validateTranslation(Solver solver) {
        }

        abstract Pair<IrBoolExpr, T> setup(IrModule module);

        abstract Constraint setup(T s, Solver solver);
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.FIELD})
    protected @interface IrVarField {
    }
}
