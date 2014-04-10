package org.clafer.ir;

import java.util.HashSet;
import java.util.Set;
import org.clafer.Sample;
import org.clafer.test.TestReflection;
import static org.clafer.test.TestReflection.*;
import static org.clafer.test.TestUtil.*;
import org.clafer.common.Util;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import org.clafer.test.Annotations;
import org.clafer.test.Procedure;
import static org.junit.Assert.*;
import solver.Solver;
import solver.constraints.Constraint;

/**
 *
 * @author jimmy
 */
public class IrTest {

    protected void randomizedTest(TestCase testCase) {
        int repeat = Sample.Default;
        for (int i = 0; i < repeat; i++) {
            randomizedTest(testCase, true);
        }
        for (int i = 0; i < repeat; i++) {
            randomizedTest(testCase, false);
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

        Set<Object> solutions = new HashSet<>();
        IrSolutionMap irSolutionMap = IrCompiler.compile(module, irSolver);
        testCase.validateTranslation(irSolver);

        if (randomizeStrategy(irSolver).findSolution()) {
            do {
                solutions.add(testCase.solution(irSolutionMap));
            } while (irSolver.nextSolution());
        }

        Solver solver = new Solver();
        Constraint constraint = positive
                ? testCase.setup(solver)
                : testCase.setup(solver).getOpposite();
        solver.post(constraint);

        if (solver.findSolution()) {
            do {
                Object solution = testCase.solution(solver);
                if (!solutions.remove(testCase.solution(solver))) {
                    fail("Missing solution " + solution);
                }
            } while (solver.nextSolution());
        }

        if (!solutions.isEmpty()) {
            fail("Wrong solutions " + solutions);
        }
    }

    protected abstract class TestCase {

        void initialize() {
        }

        void validateTranslation(Solver solver) {
        }

        abstract IrBoolExpr setup(IrModule module);

        abstract Constraint setup(Solver solver);

        abstract Object solution(IrSolutionMap solution);

        abstract Object solution(Solver solver);
    }

    protected abstract class TestCaseByConvention extends TestCase {

        protected Procedure<IrBoolExpr> irSetup;
        protected Procedure<Constraint> setup;
        Object[] irVariables;
        Object[] variables;

        TestCaseByConvention() {
            this.irSetup = method(getClass(), "setup", IrBoolExpr.class);
            this.setup = method(getClass(), "setup", Constraint.class);
        }

        protected Object[] initializeVariables() {
            return null;
        }

        protected Annotations[] annotations() {
            return irSetup.getParameterAnnotations();
        }

        @Override
        protected IrBoolExpr setup(IrModule module) {
            Class<?>[] parameters = irSetup.getParameterTypes();
            Annotations[] annotations = annotations();
            boolean env = parameters.length > 0 && IrModule.class.equals(parameters[0]);
            irVariables = initializeVariables();
            if (irVariables == null) {
                irVariables = new Object[env ? parameters.length - 1 : parameters.length];
            }
            for (int i = 0; i < irVariables.length; i++) {
                if (irVariables[i] == null) {
                    irVariables[i] = create(module,
                            annotations[env ? i + 1 : i],
                            parameters[env ? i + 1 : i]);
                } else {
                    TestReflection.addVariables(module, irVariables[i]);
                }
            }
            Object[] args = env ? Util.cons(module, irVariables) : irVariables;
            return irSetup.invoke(this, args);
        }

        @Override
        protected Constraint setup(Solver solver) {
            Class<?>[] parameters = setup.getParameterTypes();
            boolean env = parameters.length > 0 && Solver.class.equals(parameters[0]);
            variables = new Object[irVariables.length];
            for (int i = 0; i < variables.length; i++) {
                variables[i] = create(solver, env ? parameters[i + 1] : parameters[i], irVariables[i]);
            }
            Object[] args = env ? Util.cons(solver, variables) : variables;
            return setup.invoke(this, args);
        }

        @Override
        protected Object solution(IrSolutionMap solution) {
            return value(solution, irVariables);
        }

        protected @Override
        Object solution(Solver solver) {
            return value(solver, variables);
        }

        Object create(IrModule module, Annotations annotations, Class<?> type) {
            return TestReflection.randIrVar(annotations, type, module);
        }

        Object create(Solver solver, Class<?> type, Object value) {
            return TestReflection.toVar(value, type, solver);
        }

        Object value(IrSolutionMap solution, Object var) {
            return TestReflection.value(solution, var);
        }

        Object value(Solver solver, Object var) {
            return TestReflection.value(var);
        }
    }
}
