package org.clafer.ir;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Parameter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.chocosolver.solver.Model;
import org.clafer.common.UnsatisfiableException;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import org.clafer.test.TestReflection;
import static org.clafer.test.TestUtil.*;
import static org.junit.Assert.*;
import org.junit.internal.AssumptionViolatedException;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.Suite;
import org.junit.runners.model.FrameworkMethod;
import org.junit.runners.model.InitializationError;
import org.junit.runners.model.Statement;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;

/**
 *
 * @author jimmy
 */
public class IrQuickTest extends Suite {

    private final List<FrameworkMethod> solutionMethods;
    private final List<FrameworkMethod> checkMethods;

    public IrQuickTest(Class<?> klass) throws Throwable {
        super(klass, new ArrayList<>());
        this.solutionMethods = getTestClass().getAnnotatedMethods(Solution.class);
        for (FrameworkMethod solutionMethod : solutionMethods) {
            if (!solutionMethod.isPublic()) {
                throw new Exception("Methods in " + klass + " marked @Solution must be public.");
            }
        }
        if (solutionMethods.isEmpty()) {
            throw new Exception(klass + " must contain at least one method marked @Solution.");
        }
        this.checkMethods = getTestClass().getAnnotatedMethods(Check.class);
        for (FrameworkMethod checkMethod : checkMethods) {
            if (!checkMethod.isPublic()) {
                throw new Exception("Methods in " + klass + " marked @Check must be public.");
            }
        }
        getChildren().add(new QuickTestRunner(klass));
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.METHOD)
    public static @interface Solution {
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target(ElementType.METHOD)
    public static @interface Check {
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
            IrModule module = new IrModule();

            Parameter[] irParameters = testMethod.getMethod().getParameters();
            Object[] irArgs = new Object[irParameters.length];
            for (int i = 0; i < irArgs.length; i++) {
                irArgs[i] = TestReflection.randIrVar(
                        irParameters[i].getName(),
                        irParameters[i].getAnnotations(),
                        irParameters[i].getType(),
                        module);
            }
            try {
                Set<Object> solutions = new HashSet<>();
                try {
                    IrBoolExpr irConstraint = (IrBoolExpr) testMethod.invokeExplosively(target, irArgs);
                    irConstraint = positive ? irConstraint : irConstraint.negate();
                    module.addConstraint(irConstraint);

                    Model irModel = new Model();
                    IrSolutionMap irSolutionMap = IrCompiler.compile(module, irModel);
                    Solver irSolver = irModel.getSolver();
                    for (FrameworkMethod checkMethod : checkMethods) {
                        checkMethod.invokeExplosively(target, irSolver);
                    }

                    randomizeStrategy(irSolver);
                    while (irSolver.solve()) {
                        solutions.add(TestReflection.value(irSolutionMap, irArgs));
                    }
                } catch (UnsatisfiableException e) {
                    // Do nothing.
                }

                for (FrameworkMethod solutionMethod : solutionMethods) {
                    Model model = new Model();
                    Class<?>[] parameters = solutionMethod.getMethod().getParameterTypes();
                    Object[] args = new Object[irArgs.length];
                    for (int i = 0; i < args.length; i++) {
                        args[i] = TestReflection.toVar(
                                irArgs[i],
                                parameters[i],
                                model);
                    }

                    Constraint constraint = (Constraint) solutionMethod.invokeExplosively(target, args);
                    constraint = positive ? constraint : constraint.getOpposite();
                    constraint.post();
                    Solver solver = model.getSolver();
                    randomizeStrategy(solver);
                    while (solver.solve()) {
                        Object solution = TestReflection.value(args);
                        if (!solutions.remove(solution)) {
                            fail("Missing" + (positive ? " " : " negative ") + "solution " + solution + "\nfrom " + Arrays.deepToString(irArgs));
                        }
                    }
                }

                if (!solutions.isEmpty()) {
                    fail("Wrong" + (positive ? " " : " negative ") + "solutions " + solutions + "\nfrom " + Arrays.deepToString(irArgs));
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
