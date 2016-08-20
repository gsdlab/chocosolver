package org.clafer.test;

import gnu.trove.list.array.TIntArrayList;
import gnu.trove.set.hash.TIntHashSet;
import java.lang.annotation.Annotation;
import java.lang.reflect.Array;
import java.util.Arrays;
import org.chocosolver.solver.Model;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.CStringVar;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.clafer.domain.Domain;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrModule;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrStringVar;
import org.clafer.ir.IrVar;
import org.clafer.ir.compiler.IrSolutionMap;
import static org.clafer.test.TestUtil.randBool;
import static org.clafer.test.TestUtil.randBoolVar;
import static org.clafer.test.TestUtil.randDomain;
import static org.clafer.test.TestUtil.randElement;
import static org.clafer.test.TestUtil.randInt;
import static org.clafer.test.TestUtil.randIntVar;
import static org.clafer.test.TestUtil.randIrBoolVar;
import static org.clafer.test.TestUtil.randIrIntVar;
import static org.clafer.test.TestUtil.randIrSetVar;
import static org.clafer.test.TestUtil.randIrStringVar;
import static org.clafer.test.TestUtil.randNonEmptyDomain;
import static org.clafer.test.TestUtil.randNonEmptyIrStringVar;
import static org.clafer.test.TestUtil.randNonEmptyStringVar;
import static org.clafer.test.TestUtil.randSetVar;
import static org.clafer.test.TestUtil.randSetVarNoCard;
import static org.clafer.test.TestUtil.randStringVar;
import static org.clafer.test.TestUtil.randTerm;

/**
 *
 * @author jimmy
 */
public class TestReflection {

    private static int pow2(int n) {
        return 1 << n;
    }

    private static int nChooseR(int n, int r) {
        return factorial(n) / (factorial(n - r) * factorial(r));
    }

    private static int factorial(int n) {
        if (n > 16) {
            throw new IllegalArgumentException();
        }
        int factorial = 1;
        for (int i = 2; i <= n; i++) {
            factorial *= i;
        }
        return factorial;
    }

    public static int countSolutions(Object o) {
        if (o instanceof IntVar) {
            return ((IntVar) o).getDomainSize();
        } else if (o instanceof SetVar) {
            SetVar var = (SetVar) o;
            int envSize = var.getUB().size();
            int kerSize = var.getLB().size();
            if (!var.hasCard()) {
                return pow2(var.getUB().size() - var.getLB().size());
            }
            int lb = var.getCard().getLB();
            int ub = var.getCard().getUB();
            if (kerSize >= lb && envSize <= ub) {
                return pow2(var.getUB().size() - var.getLB().size());
            }
            int count = 0;
            for (int i = lb; i <= ub; i = var.getCard().nextValue(i)) {
                count += nChooseR(envSize - kerSize, i);
            }
            return count;
        } else if (o instanceof CStringVar) {
            CStringVar var = (CStringVar) o;
            int count = 1;
            int i;
            for (i = 0; i < var.getLength().getLB(); i++) {
                count *= var.getChars()[i].getDomainSize();
            }
            int exactCount = count;
            for (; i < var.getLength().getUB(); i++) {
                exactCount *= var.getChars()[i].getDomainSize() - 1;
                count += exactCount;
            }
            return count;
        } else if (o instanceof Object[]) {
            int count = 1;
            for (Object os : (Object[]) o) {
                count *= countSolutions(os);
            }
            return count;
        } else {
            return 1;
        }
    }

    public static Object randIrVar(String name, Annotation[] annotations, Class<?> type, IrModule module) {
        return randIrVar(name, annotations, type, module, null);
    }

    public static Object randIrVar(String name, Annotation[] annotations, Class<?> type, IrModule module, Integer sameLength) {
        int low = hasAnnotation(Positive.class, annotations) ? 0 : -4;
        int high = 4;
        if (IrBoolVar.class.equals(type)) {
            IrVar var = randIrBoolVar();
            module.addVariable(var);
            return var;
        } else if (IrIntVar.class.equals(type)) {
            IrVar var = randIrIntVar(name, low, high);
            module.addVariable(var);
            return var;
        } else if (IrSetVar.class.equals(type)) {
            IrVar var = randIrSetVar(low, high);
            module.addVariable(var);
            return var;
        } else if (IrStringVar.class.equals(type)) {
            IrVar var = hasAnnotation(NonEmpty.class, annotations)
                    ? randNonEmptyIrStringVar()
                    : randIrStringVar();
            module.addVariable(var);
            return var;
        } else if (Term.class.equals(type)) {
            Term term = hasAnnotation(Fixed.class, annotations)
                    ? TestUtil.randFixedTerm()
                    : randTerm();
            module.addVariable(term.getIrVar());
            return term;
        } else if (boolean.class.equals(type)) {
            return randBool();
        } else if (int.class.equals(type)) {
            return randInt(low, high);
        } else if (Domain.class.equals(type)) {
            return hasAnnotation(NonEmpty.class, annotations)
                    ? randNonEmptyDomain(low, high)
                    : randDomain(low, high);
        } else if (type.isEnum()) {
            return randElement(type.getEnumConstants());
        } else if (type.isArray()) {
            int length
                    = sameLength == null
                            ? hasAnnotation(NonEmpty.class, annotations) ? randInt(1, 3) : randInt(0, 3)
                            : sameLength;
            Integer recurSameLength = null;
            if (hasAnnotation(SameLength.class, annotations)) {
                recurSameLength = hasAnnotation(NonEmpty.class, annotations) ? randInt(1, 3) : randInt(0, 3);
            }
            Object array = Array.newInstance(type.getComponentType(), length);
            for (int i = 0; i < length; i++) {
                Array.set(array, i, randIrVar(name + "[" + i + "]", annotations, type.getComponentType(), module, recurSameLength));
            }
            return array;
        }
        throw new IllegalStateException("Unexpected type " + type);
    }

    public static Object randVar(String name, Annotation[] annotations, Class<?> type, Model model) {
        return randVar(name, annotations, type, model, null);
    }

    private static Object randVar(String name, Annotation[] annotations, Class<?> type, Model model, Integer sameLength) {
        int low = hasAnnotation(Positive.class, annotations) ? 0 : -4;
        int high = 4;
        if (BoolVar.class.equals(type)) {
            return randBoolVar(model);
        } else if (IntVar.class.equals(type)) {
            return randIntVar(name, low, high, model);
        } else if (SetVar.class.equals(type)) {
            return hasAnnotation(NoCard.class, annotations)
                    ? randSetVarNoCard(low, high, model)
                    : randSetVar(low, high, model);
        } else if (CStringVar.class.equals(type)) {
            return hasAnnotation(NonEmpty.class, annotations)
                    ? randNonEmptyStringVar(model)
                    : randStringVar(model);
        } else if (boolean.class.equals(type)) {
            return randBool();
        } else if (int.class.equals(type)) {
            return randInt(low, high);
        } else if (Domain.class.equals(type)) {
            return hasAnnotation(NonEmpty.class, annotations)
                    ? randNonEmptyDomain(low, high)
                    : randDomain(low, high);
        } else if (type.isArray()) {
            int length
                    = sameLength == null
                            ? hasAnnotation(NonEmpty.class, annotations) ? randInt(1, 3) : randInt(0, 3)
                            : sameLength;
            Integer recurSameLength = null;
            if (hasAnnotation(SameLength.class, annotations)) {
                recurSameLength = hasAnnotation(NonEmpty.class, annotations) ? randInt(1, 3) : randInt(0, 3);
            }
            Object array = Array.newInstance(type.getComponentType(), length);
            for (int i = 0; i < length; i++) {
                Array.set(array, i, randVar(name + "[" + i + "]", annotations, type.getComponentType(), model, recurSameLength));
            }
            return array;
        }
        throw new IllegalStateException("Unexpected type " + type);
    }

    public static Object toVar(Object irVar, Class<?> type, Model model) {
        if (BoolVar.class.equals(type)) {
            return TestUtil.toVar((IrBoolVar) irVar, model);
        } else if (IntVar.class.equals(type)) {
            if (irVar instanceof Term) {
                return ((Term) irVar).toChocoVar(model);
            }
            return TestUtil.toVar((IrIntVar) irVar, model);
        } else if (SetVar.class.equals(type)) {
            return TestUtil.toVar((IrSetVar) irVar, model);
        } else if (CStringVar.class.equals(type)) {
            return TestUtil.toVar((IrStringVar) irVar, model);
        } else if (boolean.class.equals(type)) {
            return irVar;
        } else if (int.class.equals(type)) {
            return irVar;
        } else if (type.isEnum()) {
            return irVar;
        } else if (int[].class.equals(type)) {
            return ((Domain) irVar).getValues();
        } else if (type.isArray()) {
            Object[] irVars = (Object[]) irVar;
            Object creates = Array.newInstance(type.getComponentType(), irVars.length);
            for (int i = 0; i < irVars.length; i++) {
                Object create = toVar(irVars[i], type.getComponentType(), model);
                Array.set(creates, i, create);
            }
            return creates;
        }
        throw new IllegalStateException("Unexpected type " + irVar.getClass());
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
        } else if (irVar instanceof Domain) {
            return new TIntArrayList(((Domain) irVar).getValues());
        } else if (irVar instanceof Boolean) {
            return irVar;
        } else if (irVar instanceof Integer) {
            return irVar;
        } else if (irVar instanceof Enum) {
            return irVar;
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

    public static Object value(Object var) {
        if (var instanceof IntVar) {
            return ((IntVar) var).getValue();
        } else if (var instanceof SetVar) {
            return new TIntHashSet(((SetVar) var).getLB().toArray());
        } else if (var instanceof CStringVar) {
            CStringVar string = (CStringVar) var;
            char[] chars = new char[string.getLength().getValue()];
            for (int i = 0; i < chars.length; i++) {
                chars[i] = (char) string.getChars()[i].getValue();
            }
            return new String(chars);
        } else if (var instanceof Boolean) {
            return var;
        } else if (var instanceof Integer) {
            return var;
        } else if (var instanceof Enum) {
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

    public static Object value(Class<?> type, Object var) {
        if (boolean.class.equals(type)) {
            if (var instanceof Boolean) {
                return var;
            }
            return ((BoolVar) var).getValue() == 1;
        } else if (var instanceof IntVar) {
            return ((IntVar) var).getValue();
        } else if (var instanceof SetVar) {
            int[] set = ((SetVar) var).getLB().toArray();
            return int[].class.equals(type)
                    ? set
                    : new TIntHashSet(set);
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
            return int[].class.equals(type)
                    ? var
                    : new TIntArrayList((int[]) var);
        } else if (var instanceof Object[]) {
            Object[] vars = (Object[]) var;
            Object values = Array.newInstance(type.getComponentType(), vars.length);
            for (int i = 0; i < Array.getLength(values); i++) {
                Array.set(values, i, value(type.getComponentType(), vars[i]));
            }
            return type.isArray()
                    ? values
                    : Arrays.asList(values);
        }
        throw new IllegalStateException("Unexpected type " + var.getClass());
    }

    public static boolean hasAnnotation(Class<?> type, Annotation[] annotations) {
        for (Annotation annotation : annotations) {
            if (type.equals(annotation.annotationType())) {
                return true;
            }
        }
        return false;
    }
}
