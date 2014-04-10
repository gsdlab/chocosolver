package org.clafer.test;

import gnu.trove.list.array.TIntArrayList;
import gnu.trove.set.hash.TIntHashSet;
import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.util.Arrays;
import static org.clafer.test.TestUtil.*;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrDomain;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrStringVar;
import org.clafer.ir.IrVar;
import org.clafer.ir.compiler.IrSolutionMap;
import solver.Solver;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
public class TestReflection {

    public static void addVariables(IrModule module, Object os) {
        if (os instanceof IrVar) {
            module.addVariable((IrVar) os);
        } else if (os instanceof Object[]) {
            for (Object o : (Object[]) os) {
                addVariables(module, o);
            }
        }
    }

    public static Object randIrVar(Annotations annotations, Class<?> type, IrModule module) {
        int low = annotations.hasAnnotation(Positive.class) ? 0 : -4;
        int high = 4;
        if (IrBoolVar.class.equals(type)) {
            IrVar var = randIrBoolVar();
            module.addVariable(var);
            return var;
        } else if (IrIntVar.class.equals(type)) {
            IrVar var = randIrIntVar(low, high);
            module.addVariable(var);
            return var;
        } else if (IrSetVar.class.equals(type)) {
            IrVar var = randIrSetVar(low, high);
            module.addVariable(var);
            return var;
        } else if (IrStringVar.class.equals(type)) {
            IrVar var = randIrStringVar();
            module.addVariable(var);
            return var;
        } else if (Term.class.equals(type)) {
            Term term = annotations.hasAnnotation(Fixed.class)
                    ? TestUtil.randFixedTerm()
                    : randTerm();
            module.addVariable(term.getIrVar());
            return term;
        } else if (int.class.equals(type)) {
            return randInt(low, high);
        } else if (IrDomain.class.equals(type)) {
            return annotations.hasAnnotation(NonEmpty.class)
                    ? randNonEmptyDomain(low, high)
                    : randDomain(low, high);
        } else if (type.isArray()) {
            int length = annotations.hasAnnotation(NonEmpty.class) ? randInt(1, 3) : randInt(0, 3);
            Object array = Array.newInstance(type.getComponentType(), length);
            for (int i = 0; i < length; i++) {
                Array.set(array, i, randIrVar(annotations, type.getComponentType(), module));
            }
            return array;
        }
        throw new IllegalStateException("Unexpected type " + type);
    }

    public static Object value(IrSolutionMap solution, Object irVar) {
        if (irVar instanceof IrIntVar) {
            return solution.getValue((IrIntVar) irVar);
        } else if (irVar instanceof IrSetVar) {
            return new TIntHashSet(solution.getValue((IrSetVar) irVar));
        } else if (irVar instanceof IrStringVar) {
            return solution.getValue((IrStringVar) irVar);
        } else if (irVar instanceof Integer) {
            return irVar;
        } else if (irVar instanceof Term) {
            return ((Term) irVar).getValue(solution);
        } else if (irVar instanceof IrDomain) {
            return new TIntArrayList(((IrDomain) irVar).getValues());
        } else if (irVar instanceof Object[]) {
            Object[] vars = (Object[]) irVar;
            Object[] values = new Object[vars.length];
            for (int i = 0; i < values.length; i++) {
                values[i] = value(solution, vars[i]);
            }
            return Arrays.asList(values);
        }
        throw new IllegalStateException("Unexpected type " + irVar.getClass());
    }

    public static Object toVar(Object irVar, Class<?> type, Solver solver) {
        if (BoolVar.class.equals(type)) {
            return TestUtil.toVar((IrBoolVar) irVar, solver);
        } else if (IntVar.class.equals(type)) {
            if (irVar instanceof Term) {
                return ((Term) irVar).toChocoVar(solver);
            }
            return TestUtil.toVar((IrIntVar) irVar, solver);
        } else if (SetVar.class.equals(type)) {
            return TestUtil.toVar((IrSetVar) irVar, solver).getSet();
        } else if (TestUtil.CSetVar.class.equals(type)) {
            return TestUtil.toVar((IrSetVar) irVar, solver);
        } else if (TestUtil.CStringVar.class.equals(type)) {
            return TestUtil.toVar((IrStringVar) irVar, solver);
        } else if (int.class.equals(type)) {
            return irVar;
        } else if (int[].class.equals(type)) {
            return ((IrDomain) irVar).getValues();
        } else if (type.isArray()) {
            Object[] irVars = (Object[]) irVar;
            Object creates = Array.newInstance(type.getComponentType(), irVars.length);
            for (int i = 0; i < irVars.length; i++) {
                Object create = toVar(irVars[i], type.getComponentType(), solver);
                Array.set(creates, i, create);
            }
            return creates;
        }
        throw new IllegalStateException("Unexpected type " + irVar.getClass());
    }

    public static Object value(Object var) {
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
                values[i] = value(vars[i]);
            }
            return Arrays.asList(values);
        }
        throw new IllegalStateException("Unexpected type " + var.getClass());
    }

    public static <T> Procedure<T> method(Class<?> c, String name, Class<T> returnType) {
        for (Method method : c.getDeclaredMethods()) {
            if (returnType.equals(method.getReturnType())) {
                return new Procedure<>(method);
            }
        }
        throw new IllegalArgumentException();
    }
}
