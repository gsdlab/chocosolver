package org.clafer.ir;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import org.clafer.ClaferTest;
import org.clafer.Sample;
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

    protected void randomizedTest(TestCase testCase) {
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

    private void randomizedTest(
            TestCase testCase,
            boolean positive) {
        IrModule module = new IrModule();
        Solver irSolver = new Solver();

        testCase.initialize();

        IrBoolExpr irConstraint = positive
                ? testCase.setup(module)
                : testCase.setup(module).negate();
        module.addConstraint(irConstraint);
        module.addVariables(getVariables(testCase));

        IrSolutionMap solution = IrCompiler.compile(module, irSolver);
        testCase.validateTranslation(irSolver);

        int count = 0;
        if (randomizeStrategy(irSolver).findSolution()) {
            do {
                testCase.check(positive, irConstraint, solution);
                count++;
            } while (irSolver.nextSolution());
        }

        Solver solver = new Solver();
        Constraint constraint = positive
                ? testCase.setup(solver)
                : testCase.setup(solver).getOpposite();
        solver.post(constraint);

        assertEquals("Wrong number of solutions.",
                randomizeStrategy(solver).findAllSolutions(), count);
    }

    private static List<IrVar> getVariables(Object o) {
        List<IrVar> variables = new ArrayList<>();
        getVariables(o, variables);
        return variables;
    }

    private static void getVariables(Object o, List<IrVar> variables) {
        if (o instanceof IrVar) {
            variables.add((IrVar) o);
        } else if (o instanceof Object[]) {
            for (Object p : ((Object[]) o)) {
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

    protected abstract class TestCase {

        abstract void check(IrSolutionMap solution);

        protected void checkNot(IrSolutionMap solution) {
            try {
                check(solution);
            } catch (AssertionError e) {
                return;
            }
            fail();
        }

        private void check(
                boolean positive, IrBoolExpr constraint,
                IrSolutionMap solution) {
            try {
                if (positive) {
                    check(solution);
                } else {
                    checkNot(solution);
                }
            } catch (AssertionError e) {
                throw new AssertionError("Incorrect solution: " + constraint, e);
            }
        }

        void validateTranslation(Solver solver) {
        }

        private Object initalize(boolean positive, boolean nonEmpty, Class<?> type) {
            if (IrBoolVar.class.equals(type)) {
                return randBool();
            } else if (IrIntVar.class.equals(type)) {
                return positive ? randPositiveInt() : randInt();
            } else if (IrSetVar.class.equals(type)) {
                return positive ? randPositiveSet() : randSet();
            } else if (IrStringVar.class.equals(type)) {
                return randString();
            } else if (type.isArray()) {
                int length = nonEmpty ? 1 + nextInt(5) : nextInt(5);
                Object array = Array.newInstance(type.getComponentType(), length);
                for (int i = 0; i < length; i++) {
                    Array.set(array, i, initalize(positive, nonEmpty, type.getComponentType()));
                }
                return array;
            }
            throw new IllegalStateException("Unexpected type " + type);
        }

        void initialize() {
            for (Field field : getClass().getDeclaredFields()) {
                if (field.isAnnotationPresent(IrVarField.class)) {
                    boolean positive = field.isAnnotationPresent(Positive.class);
                    boolean nonEmpty = field.isAnnotationPresent(NonEmpty.class);
                    Class<?> type = field.getType();
                    Object value = initalize(positive, nonEmpty, type);
                    field.setAccessible(true);
                    try {
                        field.set(this, value);
                    } catch (IllegalAccessException e) {
                        throw new Error(e);
                    }
                }
            }
        }

        abstract IrBoolExpr setup(IrModule module);

        abstract Constraint setup(Solver solver);
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.FIELD})
    protected @interface IrVarField {
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.FIELD})
    protected @interface Positive {
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.FIELD})
    protected @interface NonEmpty {
    }
}
