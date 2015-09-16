package org.clafer.choco.constraint;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Parameter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.clafer.test.TestReflection;
import org.clafer.test.TestUtil;
import static org.junit.Assert.*;
import org.junit.internal.AssumptionViolatedException;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.Suite;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.InitializationError;
import org.junit.runners.model.Statement;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.constraints.Propagator;
import org.chocosolver.util.ESat;

/**
 *
 * @author jimmy
 */
public class ConstraintQuickTest extends Suite {

    private final List<FrameworkMethod> parametersMethods;
    private final List<FrameworkMethod> checkMethods;

    public ConstraintQuickTest(Class<?> klass) throws Throwable {
        super(klass, new ArrayList<>());
        this.parametersMethods = getTestClass().getAnnotatedMethods(Input.class);
        for (FrameworkMethod parameterMethod : parametersMethods) {
            if (!parameterMethod.isPublic()) {
                throw new Exception("Methods in " + klass + " marked @Input must be public.");
            }
        }
        this.checkMethods = getTestClass().getAnnotatedMethods(Check.class);
        for (FrameworkMethod checkMethod : checkMethods) {
            if (!checkMethod.isPublic()) {
                throw new Exception("Methods in " + klass + " marked @Check must be public.");
            }
        }
        if (checkMethods.isEmpty()) {
            throw new Exception(klass + " must contain at least one method marked @Check.");
        }
        for (FrameworkMethod parameterMethod : parametersMethods) {
            getChildren().add(new InputTestRunner(klass, parameterMethod, parameterMethod.getName()));
        }
        getChildren().add(new QuickTestRunner(klass));
    }

    void check(Object target, Object[] args) throws Throwable {
        for (FrameworkMethod checkMethod : checkMethods) {
            Object[] translatedArgs = new Object[args.length];
            for (int i = 0; i < translatedArgs.length; i++) {
                translatedArgs[i] = TestReflection.value(
                        checkMethod.getMethod().getParameterTypes()[i],
                        args[i]);
            }
            try {
                checkMethod.invokeExplosively(target, translatedArgs);
            } catch (AssertionError e) {
                throw new AssertionError(
                        "Failed check for arguments " + Arrays.deepToString(args),
                        e);
            }
        }
    }

    void checkNot(Object target, Object[] args) throws Throwable {
        try {
            check(target, args);
        } catch (AssertionError e) {
            // Expected
            return;
        }
        throw new AssertionError("Failed negative check for arguments " + Arrays.deepToString(args));
    }

    Solver newSolver(FrameworkMethod testMethod, boolean positive) {
        Solver solver = new Solver();
        ArcConsistent arc = testMethod.getAnnotation(ArcConsistent.class);
        if (arc != null && (positive || arc.opposite())) {
            solver.plugMonitor(new ArcConsistentCheck(solver));
        }
        return solver;
    }

    public static Object[] $(Object arg1) {
        return new Object[]{arg1};
    }

    public static Object[] $(Object arg1, Object arg2) {
        return new Object[]{arg1, arg2};
    }

    public static Object[] $(Object arg1, Object arg2, Object arg3) {
        return new Object[]{arg1, arg2, arg3};
    }

    public static Object[] $(Object arg1, Object arg2, Object arg3, Object arg4) {
        return new Object[]{arg1, arg2, arg3, arg4};
    }

    public static Object[] $(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {
        return new Object[]{arg1, arg2, arg3, arg4, arg5};
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.METHOD)
    public static @interface Input {

        int solutions();
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.METHOD)
    public static @interface Check {
    }

    class InputTest extends Statement {

        private final FrameworkMethod testMethod;
        private final Object target;
        private final FrameworkMethod parameters;

        public InputTest(FrameworkMethod testMethod, Object target, FrameworkMethod parameters) {
            this.testMethod = testMethod;
            this.target = target;
            this.parameters = parameters;
        }

        @Override
        public void evaluate() throws Throwable {
            for (int i = 0; i < 10; i++) {
                evaluate(true);
            }
            for (int i = 0; i < 10; i++) {
                evaluate(false);
            }
        }

        void evaluate(boolean positive) throws Throwable {
            Solver solver = newSolver(testMethod, positive);

            Object[] args = (Object[]) parameters.invokeExplosively(target, solver);
            int expectedNumberOfSolutions = positive
                    ? parameters.getAnnotation(Input.class).solutions()
                    : TestReflection.countSolutions(args) - parameters.getAnnotation(Input.class).solutions();

            int count = 0;
            Constraint constraint = (Constraint) testMethod.invokeExplosively(target, args);
            solver.post(positive ? constraint : constraint.getOpposite());
            if (TestUtil.randomizeStrategy(solver).findSolution()) {
                do {
                    if (positive) {
                        check(target, args);
                    } else {
                        checkNot(target, args);
                    }
                    count++;
                } while (solver.nextSolution());
            }
            assertEquals(positive ? "Wrong number of solutions." : "Wrong number of negative solutions.",
                    expectedNumberOfSolutions, count);
        }
    }

    class InputTestRunner extends BlockJUnit4ClassRunner {

        private final FrameworkMethod parameters;
        private final String name;

        InputTestRunner(Class<?> type, FrameworkMethod parameters,
                String name) throws InitializationError {
            super(type);
            this.parameters = parameters;
            this.name = name;
        }

        @Override
        protected void validateTestMethods(List<Throwable> errors) {
        }

        @Override
        protected String getName() {
            return name;
        }

        @Override
        protected String testName(FrameworkMethod method) {
            return name;
        }

        @Override
        protected Statement methodInvoker(FrameworkMethod method, Object test) {
            return new InputTest(method, test, parameters);
        }
    }

    class QuickTest extends Statement {

        private final FrameworkMethod testMethod;
        private final Object target;

        public QuickTest(FrameworkMethod testMethod, Object target) {
            this.testMethod = testMethod;
            this.target = target;
        }

        @Override
        public void evaluate() throws Throwable {
            for (int i = 0; i < 10; i++) {
                evaluate(true);
            }
            for (int i = 0; i < 10; i++) {
                evaluate(false);
            }
        }

        void evaluate(boolean positive) throws Throwable {
            Solver solver = newSolver(testMethod, positive);

            Parameter[] parameters = testMethod.getMethod().getParameters();
            Object[] args = new Object[parameters.length];
            for (int i = 0; i < args.length; i++) {
                args[i] = TestReflection.randVar(
                        parameters[i].getName(),
                        parameters[i].getAnnotations(),
                        parameters[i].getType(),
                        solver);
            }
            try {
                Constraint constraint = (Constraint) testMethod.invokeExplosively(target, args);
                constraint = positive ? constraint : constraint.getOpposite();
                solver.post(constraint);

                TestUtil.randomizeStrategy(solver);
                ESat entailed = TestUtil.isEntailed(constraint);
                if (ESat.FALSE.equals(entailed)) {
                    String initial = null;
                    for (Propagator<?> propagator : constraint.getPropagators()) {
                        if (ESat.FALSE.equals(propagator.isEntailed())) {
                            initial = propagator.toString();
                        }
                    }
                    if (solver.findSolution()) {
                        fail("Did not expect a solution for " + initial + ", found " + constraint);

                    }
                } else if (solver.findSolution()) {
                    int solutions = 1;
                    do {
                        if (positive) {
                            check(target, args);
                        } else {
                            checkNot(target, args);
                        }
                    } while (solver.nextSolution() && solutions++ < 10);
                } else if (ESat.TRUE.equals(entailed)) {
                    fail("Expected at least one solution for " + constraint);
                }
            } catch (AssumptionViolatedException e) {
                // Continue
            }
        }
    }

    class QuickTestRunner extends BlockJUnit4ClassRunner {

        QuickTestRunner(Class<?> type) throws InitializationError {
            super(type);
        }

        @Override
        protected void validateTestMethods(List<Throwable> errors) {
        }

        @Override
        protected String getName() {
            return "quickTest";
        }

        @Override
        protected String testName(FrameworkMethod method) {
            return "quickTest";
        }

        @Override
        protected Statement methodInvoker(FrameworkMethod method, Object test) {
            return new QuickTest(method, test);
        }
    }
}
