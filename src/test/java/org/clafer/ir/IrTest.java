package org.clafer.ir;

import gnu.trove.list.array.TIntArrayList;
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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.clafer.ClaferTest;
import org.clafer.Sample;
import org.clafer.common.Util;
import org.clafer.ir.compiler.IrCompiler;
import org.clafer.ir.compiler.IrSolutionMap;
import static org.junit.Assert.*;
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

    private static void addVariables(IrModule module, Object o) {
        if (o instanceof IrVar) {
            module.addVariable((IrVar) o);
        } else if (o instanceof Object[]) {
            for (Object os : (Object[]) o) {
                addVariables(module, os);
            }
        } else {
            throw new IllegalArgumentException();
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

        protected Method irSetup;
        protected Method setup;
        Object[] irVariables;
        Object[] variables;

        TestCaseByConvention() {
            for (Method method : getClass().getDeclaredMethods()) {
                if ("setup".equals(method.getName())) {
                    if (IrBoolExpr.class.equals(method.getReturnType())) {
                        assert irSetup == null;
                        irSetup = method;
                    } else if (Constraint.class.equals(method.getReturnType())) {
                        assert setup == null;
                        setup = method;
                    }
                }
            }
            assert irSetup != null;
            assert setup != null;
        }

        protected Object[] initializeVariables() {
            return null;
        }

        protected Annotations[] annotations() {
            Annotation[][] parameterAnnotations = irSetup.getParameterAnnotations();
            Annotations[] annotations = new Annotations[parameterAnnotations.length];
            for (int i = 0; i < annotations.length; i++) {
                annotations[i] = new Annotations();
                annotations[i].addAnnotations(parameterAnnotations[i]);
            }
            return annotations;
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
                    addVariables(module, irVariables[i]);
                }
            }
            Object[] args = env ? Util.cons(module, irVariables) : irVariables;
            irSetup.setAccessible(true);
            try {
                return (IrBoolExpr) irSetup.invoke(this, args);
            } catch (IllegalArgumentException e) {
                StringBuilder argsToString = new StringBuilder();
                for (int i = 0; i < args.length; i++) {
                    if (i > 0) {
                        argsToString.append(", ");
                    }
                    argsToString.append(args[i].getClass());
                }
                throw new Error(irSetup + " cannot be invoked with (" + argsToString + ")", e);
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

        @Override
        protected Constraint setup(Solver solver) {
            Class<?>[] parameters = setup.getParameterTypes();
            boolean env = parameters.length > 0 && Solver.class.equals(parameters[0]);
            variables = new Object[irVariables.length];
            for (int i = 0; i < variables.length; i++) {
                variables[i] = create(solver, env ? parameters[i + 1] : parameters[i], irVariables[i]);
            }
            Object[] args = env ? Util.cons(solver, variables) : variables;
            setup.setAccessible(true);
            try {
                return (Constraint) setup.invoke(this, args);
            } catch (IllegalArgumentException e) {
                StringBuilder argsToString = new StringBuilder();
                for (int i = 0; i < args.length; i++) {
                    if (i > 0) {
                        argsToString.append(", ");
                    }
                    argsToString.append(args[i].getClass());
                }
                throw new Error(setup + " cannot be invoked with (" + argsToString + ")", e);
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

        @Override
        protected Object solution(IrSolutionMap solution) {
            return value(solution, irVariables);
        }

        protected @Override
        Object solution(Solver solver) {
            return value(solver, variables);
        }

        Object create(IrModule module, Annotations annotations, Class<?> type) {
            if (IrBoolVar.class.equals(type)) {
                IrVar var = randBool();
                module.addVariable(var);
                return var;
            } else if (IrIntVar.class.equals(type)) {
                IrVar var = annotations.hasAnnotation(Positive.class) ? randPositiveInt() : randInt();
                module.addVariable(var);
                return var;
            } else if (IrSetVar.class.equals(type)) {
                IrVar var = annotations.hasAnnotation(Positive.class) ? randPositiveSet() : randSet();
                module.addVariable(var);
                return var;
            } else if (IrStringVar.class.equals(type)) {
                IrVar var = randString();
                module.addVariable(var);
                return var;
            } else if (int.class.equals(type)) {
                return annotations.hasAnnotation(Positive.class) ? nextIntBetween(0, 5) : nextIntBetween(-5, 5);
            } else if (IrDomain.class.equals(type)) {
                return annotations.hasAnnotation(Positive.class) ? randPositiveDomain() : randDomain();
            } else if (type.isArray()) {
                int length = annotations.hasAnnotation(NonEmpty.class) ? 1 + nextInt(4) : nextInt(4);
                Object array = Array.newInstance(type.getComponentType(), length);
                for (int i = 0; i < length; i++) {
                    Array.set(array, i, create(module, annotations, type.getComponentType()));
                }
                return array;
            }
            throw new IllegalStateException("Unexpected type " + type);
        }

        Object create(Solver solver, Class<?> type, Object value) {
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
            } else if (int.class.equals(type)) {
                return value;
            } else if (int[].class.equals(type)) {
                return ((IrDomain) value).getValues();
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

        Object value(IrSolutionMap solution, Object var) {
            if (var instanceof IrIntVar) {
                return solution.getValue((IrIntVar) var);
            } else if (var instanceof IrSetVar) {
                return new TIntHashSet(solution.getValue((IrSetVar) var));
            } else if (var instanceof IrStringVar) {
                return solution.getValue((IrStringVar) var);
            } else if (var instanceof Integer) {
                return var;
            } else if (var instanceof IrDomain) {
                return new TIntArrayList(((IrDomain) var).getValues());
            } else if (var instanceof Object[]) {
                Object[] vars = (Object[]) var;
                Object[] values = new Object[vars.length];
                for (int i = 0; i < values.length; i++) {
                    values[i] = value(solution, vars[i]);
                }
                return Arrays.asList(values);
            }
            throw new IllegalStateException("Unexpected type " + var.getClass());
        }

        Object value(Solver solver, Object var) {
            if (var instanceof IntVar) {
                return ((IntVar) var).getValue();
            } else if (var instanceof SetVar) {
                return new TIntHashSet(((SetVar) var).getValue());
            } else if (var instanceof CSetVar) {
                return new TIntHashSet(((CSetVar) var).getSet().getValue());
            } else if (var instanceof CStringVar) {
                CStringVar string = (CStringVar) var;
                char[] chars = new char[string.getLength().getValue()];
                for (int i = 0; i < chars.length; i++) {
                    chars[i] = (char) string.getChars()[i].getValue();
                }
                return new String(chars);
            } else if (var instanceof Integer) {
                return var;
            } else if (var instanceof int[]) {
                return new TIntArrayList((int[]) var);
            } else if (var instanceof Object[]) {
                Object[] vars = (Object[]) var;
                Object[] values = new Object[vars.length];
                for (int i = 0; i < values.length; i++) {
                    values[i] = value(solver, vars[i]);
                }
                return Arrays.asList(values);
            }
            throw new IllegalStateException("Unexpected type " + var.getClass());
        }
    }

    protected static class Annotations {

        private Map<Class<? extends Annotation>, Annotation> map;

        boolean hasAnnotation(Class<? extends Annotation> annotation) {
            return map != null && map.containsKey(annotation);
        }

        <T extends Annotation> T getAnnotation(Class<T> annotation) {
            if (map == null) {
                return null;
            }
            @SuppressWarnings("unchecked")
            T instance = (T) map.get(annotation);
            return instance;
        }

        <T extends Annotation> void addAnnotation(T annotation) {
            if (map == null) {
                map = new HashMap<>(1);
            }
            map.put(annotation.annotationType(), annotation);
        }

        @SafeVarargs
        final <T extends Annotation> void addAnnotations(T... annotations) {
            for (T annotation : annotations) {
                addAnnotation(annotation);
            }
        }
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.FIELD})
    protected @interface IrVarField {
    }

    @Retention(RetentionPolicy.RUNTIME)
    @Target({ElementType.PARAMETER})
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
