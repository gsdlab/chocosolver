package org.clafer.ir;

import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;
import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.clafer.ClaferTest;
import org.clafer.Sample;
import org.clafer.common.Util;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import solver.Solver;
import solver.constraints.Constraint;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.SetVar;

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

    protected void randomizedTest2(TestCase2 testCase) {
        try {
            Method method = TestCase2.class.getDeclaredMethod("setup", IrModule.class);
            Sample sample = method.getAnnotation(Sample.class);
            int repeat = sample == null ? Sample.Default : sample.value();
            for (int i = 0; i < repeat; i++) {
                randomizedTest2(testCase, true);
            }
            for (int i = 0; i < repeat; i++) {
                randomizedTest2(testCase, false);
            }
        } catch (NoSuchMethodException e) {
            throw new Error(e);
        }
    }

    private void randomizedTest2(
            TestCase2 testCase,
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

    private static void addVariables(IrModule module, Object[] os) {
        for (Object o : os) {
            if (o instanceof IrVar) {
                module.addVariable((IrVar) o);
            } else {
                addVariables(module, (Object[]) o);
            }
        }
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
                int length = nonEmpty ? 1 + nextInt(3) : nextInt(3);
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

    private Object create(IrModule module, boolean positive, boolean nonEmpty, Class<?> type) {
        if (IrBoolVar.class.equals(type)) {
            IrVar var = randBool();
            module.addVariable(var);
            return var;
        } else if (IrIntVar.class.equals(type)) {
            IrVar var = positive ? randPositiveInt() : randInt();
            module.addVariable(var);
            return var;
        } else if (IrSetVar.class.equals(type)) {
            IrVar var = positive ? randPositiveSet() : randSet();
            module.addVariable(var);
            return var;
        } else if (IrStringVar.class.equals(type)) {
            IrVar var = randString();
            module.addVariable(var);
            return var;
        } else if (type.isArray()) {
            int length = nonEmpty ? 1 + nextInt(5) : nextInt(5);
            Object array = Array.newInstance(type.getComponentType(), length);
            for (int i = 0; i < length; i++) {
                Array.set(array, i, create(module, positive, nonEmpty, type.getComponentType()));
            }
            return array;
        }
        throw new IllegalStateException("Unexpected type " + type);
    }

    private Object create(Solver solver, Class<?> type, Object value) {
        if (BoolVar.class.equals(type)) {
            return toVar((IrBoolVar) value, solver);
        } else if (IntVar.class.equals(type)) {
            return toVar((IrIntVar) value, solver);
        } else if (SetVar.class.equals(type)) {
            return toVar((IrSetVar) value, solver).getSet();
        } else if (CSetVar.class.equals(type)) {
            return toVar((IrSetVar) value, solver);
        } else if (CStringVar.class.equals(type)) {
            return toVar((IrStringVar) value, solver);
        } else if (type.isArray()) {
            Object[] values = (Object[]) value;
            Object creates = Array.newInstance(type.getComponentType(), values.length);
            for (int i = 0; i < values.length; i++) {
                Object create = create(solver, type.getComponentType(), values[i]);
                Array.set(creates, i, create);
            }
            return creates;
        }
        throw new IllegalStateException("Unexpected type " + value.getClass());
    }

    private Object create(IrSolutionMap solution, Class<?> type, Object value) {
        if (boolean.class.equals(type)) {
            return solution.getValue((IrBoolVar) value);
        } else if (int.class.equals(type)) {
            return solution.getValue((IrIntVar) value);
        } else if (TIntSet.class.equals(type)) {
            return new TIntHashSet(solution.getValue((IrSetVar) value));
        } else if (String.class.equals(type)) {
            return solution.getValue((IrStringVar) value);
        } else if (type.isArray()) {
            Object[] values = (Object[]) value;
            Object creates = Array.newInstance(type.getComponentType(), values.length);
            for (int i = 0; i < values.length; i++) {
                Object create = create(solution, type.getComponentType(), values[i]);
                Array.set(creates, i, create);
            }
            return creates;
        }
        throw new IllegalStateException("Unexpected type " + value.getClass());
    }

    private static boolean hasAnnotation(Annotation[] annotations, Class<? extends Annotation> type) {
        return getAnnotation(annotations, type) != null;
    }

    private static <T> T getAnnotation(Annotation[] annotations, Class<T> type) {
        for (Annotation annotation : annotations) {
            if (type.equals(annotation.getClass())) {
                return (T) annotation;
            }
        }
        return null;
    }

    protected abstract class TestCase2 {

        void initialize() {
        }

        void validateTranslation(Solver solver) {
        }

        abstract IrBoolExpr setup(IrModule module);

        abstract Constraint setup(Solver solver);

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
    }

    protected abstract class TestCaseByConvention extends TestCase2 {

        Object[] variables;

        Object[] initializeVariables() {
            return null;
        }

        @Override
        IrBoolExpr setup(IrModule module) {
            for (Method method : getClass().getDeclaredMethods()) {
                if ("setup".equals(method.getName())
                        && IrBoolExpr.class.equals(method.getReturnType())) {
                    Class<?>[] parameters = method.getParameterTypes();
                    Annotation[][] parameterAnnotations = method.getParameterAnnotations();
                    boolean env = parameters.length > 0 && IrModule.class.equals(parameters[0]);
                    variables = initializeVariables();
                    if (variables == null) {
                        variables = new Object[parameters.length];
                        for (int i = 0; i < variables.length; i++) {
                            variables[i] = create(module,
                                    hasAnnotation(parameterAnnotations[env ? i + 1 : i], Positive.class),
                                    hasAnnotation(parameterAnnotations[env ? i + 1 : i], NonEmpty.class),
                                    parameters[env ? i + 1 : i]);
                        }
                    } else {
                        addVariables(module, variables);
                    }
                    Object[] args = env ? Util.cons(module, variables) : variables;

                    method.setAccessible(true);
                    try {
                        return (IrBoolExpr) method.invoke(this, args);
                    } catch (IllegalArgumentException e) {
                        StringBuilder argsToString = new StringBuilder();
                        for (int i = 0; i < args.length; i++) {
                            if (i > 0) {
                                argsToString.append(", ");
                            }
                            argsToString.append(args[i].getClass());
                        }
                        throw new Error(method + " cannot be invoked with (" + argsToString + ")", e);
                    } catch (IllegalAccessException e) {
                        throw new Error(e);
                    } catch (InvocationTargetException e) {
                        try {
                            throw e.getCause();
                        } catch (RuntimeException | Error ex) {
                            throw ex;
                        } catch (Throwable ex) {
                            throw new Error(ex);
                        }
                    }
                }
            }
            throw new Error();
        }

        @Override
        Constraint setup(Solver solver) {
            for (Method method : getClass().getDeclaredMethods()) {
                if ("setup".equals(method.getName())
                        && Constraint.class.equals(method.getReturnType())) {
                    Class<?>[] parameters = method.getParameterTypes();
                    boolean env = parameters.length > 0 && Solver.class.equals(parameters[0]);
                    Object[] value = new Object[variables.length];
                    for (int i = 0; i < value.length; i++) {
                        value[i] = create(solver, env ? parameters[i + 1] : parameters[i], variables[i]);
                    }
                    Object[] args = env ? Util.cons(solver, value) : value;
                    method.setAccessible(true);
                    try {
                        return (Constraint) method.invoke(this, args);
                    } catch (IllegalArgumentException e) {
                        StringBuilder argsToString = new StringBuilder();
                        for (int i = 0; i < args.length; i++) {
                            if (i > 0) {
                                argsToString.append(", ");
                            }
                            argsToString.append(args[i].getClass());
                        }
                        throw new Error(method + " cannot be invoked with (" + argsToString + ")", e);
                    } catch (IllegalAccessException e) {
                        throw new Error(e);
                    } catch (InvocationTargetException e) {
                        try {
                            throw e.getCause();
                        } catch (RuntimeException | Error ex) {
                            throw ex;
                        } catch (Throwable ex) {
                            throw new Error(ex);
                        }
                    }
                }
            }
            throw new Error();
        }

        @Override
        void check(IrSolutionMap solution) {
            for (Method method : getClass().getDeclaredMethods()) {
                if ("check".equals(method.getName())
                        && Void.TYPE.equals(method.getReturnType())) {
                    Class<?>[] parameters = method.getParameterTypes();
                    boolean env = parameters.length > 0 && IrSolutionMap.class.equals(parameters[0]);
                    Object[] value = new Object[variables.length];
                    for (int i = 0; i < value.length; i++) {
                        value[i] = create(solution, env ? parameters[i + 1] : parameters[i], variables[i]);
                    }
                    Object[] args = env ? Util.cons(solution, value) : value;
                    method.setAccessible(true);
                    try {
                        method.invoke(this, args);
                        return;
                    } catch (IllegalArgumentException e) {
                        StringBuilder argsToString = new StringBuilder();
                        for (int i = 0; i < args.length; i++) {
                            if (i > 0) {
                                argsToString.append(", ");
                            }
                            argsToString.append(args[i].getClass());
                        }
                        throw new Error(method + " cannot be invoked with (" + argsToString + ")", e);
                    } catch (IllegalAccessException e) {
                        throw new Error(e);
                    } catch (InvocationTargetException e) {
                        try {
                            throw e.getCause();
                        } catch (RuntimeException | Error ex) {
                            throw ex;
                        } catch (Throwable ex) {
                            throw new Error(ex);
                        }
                    }
                }
            }
            throw new Error();
        }
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
    protected @interface Min {

        int value();
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.FIELD})
    protected @interface Max {

        int value();
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.PARAMETER})
    protected @interface NonEmpty {
    }
}
