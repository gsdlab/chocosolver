package org.clafer.ir.compiler;

import java.util.Map;
import org.clafer.collection.Either;
import org.clafer.ir.IrBoolConstant;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntConstant;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrSetConstant;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrStringVar;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;

/**
 * Maps IR non-constant variables to their translated Choco variables. IR
 * variables can be optimized away. The get&lt;Type&gt;Var methods will return
 * either the constant it optimized to or the Choco variable. The
 * get&lt;Type&gt;Value will return the value, regardless of the optimizations.
 * The get&lt;Type&gt;Value methods are undefined if the solver has not found a
 * solution yet.
 *
 * @author jimmy
 */
public class IrSolutionMap {

    private final Map<IrIntVar, IrIntVar> coalescedIntVars;
    private final Map<IrIntVar, IntVar> intVars;
    private final Map<IrSetVar, IrSetVar> coalescedSetVars;
    private final Map<IrSetVar, SetVar> setVars;

    IrSolutionMap(
            Map<IrIntVar, IrIntVar> coalescedIntVars,
            Map<IrIntVar, IntVar> intVars,
            Map<IrSetVar, IrSetVar> coalescedSetVars,
            Map<IrSetVar, SetVar> setVars) {
        this.coalescedIntVars = coalescedIntVars;
        this.intVars = intVars;
        this.coalescedSetVars = coalescedSetVars;
        this.setVars = setVars;
    }

    public Either<Boolean, BoolVar> getVar(IrBoolVar var) {
        IrBoolVar boolVar = (IrBoolVar) coalescedIntVars.get(var);
        if (boolVar == null) {
            boolVar = var;
        }
        if (boolVar instanceof IrBoolConstant) {
            return Either.left(((IrBoolConstant) boolVar).getValue());
        }
        return Either.right((BoolVar) intVars.get(boolVar));
    }

    public Either<Boolean, BoolVar>[] getVars(IrBoolVar... vars) {
        @SuppressWarnings("unchecked")
        Either<Boolean, BoolVar>[] bvars = (Either<Boolean, BoolVar>[]) new Either<?, ?>[vars.length];
        for (int i = 0; i < bvars.length; i++) {
            bvars[i] = getVar(vars[i]);
        }
        return bvars;
    }

    public boolean getValue(IrBoolVar var) {
        Either<Boolean, BoolVar> boolVar = getVar(var);
        return boolVar.isLeft()
                ? boolVar.getLeft()
                : boolVar.getRight().getValue() != 0;
    }

    public boolean[] getValues(IrBoolVar... vars) {
        boolean[] bvalues = new boolean[vars.length];
        for (int i = 0; i < bvalues.length; i++) {
            bvalues[i] = getValue(vars[i]);
        }
        return bvalues;
    }

    public Either<Integer, IntVar> getVar(IrIntVar var) {
        IrIntVar intVar = coalescedIntVars.get(var);
        if (intVar == null) {
            intVar = var;
        }
        if (intVar instanceof IrIntConstant) {
            return Either.left(((IrIntConstant) intVar).getValue());
        }
        if (intVar instanceof IrBoolConstant) {
            return Either.left(((IrBoolConstant) intVar).getValue() ? 1 : 0);
        }
        return Either.right(intVars.get(intVar));
    }

    public Either<Integer, IntVar>[] getVars(IrIntVar... vars) {
        @SuppressWarnings("unchecked")
        Either<Integer, IntVar>[] ivars = (Either<Integer, IntVar>[]) new Either<?, ?>[vars.length];
        for (int i = 0; i < ivars.length; i++) {
            ivars[i] = getVar(vars[i]);
        }
        return ivars;
    }

    public IntVar[] getIntVars() {
        return intVars.values().toArray(new IntVar[intVars.size()]);
    }

    public int getValue(IrIntVar var) {
        Either<Integer, IntVar> intVar = getVar(var);
        return intVar.isLeft()
                ? intVar.getLeft()
                : intVar.getRight().getValue();
    }

    public int[] getValues(IrIntVar... vars) {
        int[] ivalues = new int[vars.length];
        for (int i = 0; i < ivalues.length; i++) {
            ivalues[i] = getValue(vars[i]);
        }
        return ivalues;
    }

    public Either<int[], SetVar> getVar(IrSetVar var) {
        IrSetVar setVar = coalescedSetVars.get(var);
        if (setVar == null) {
            setVar = var;
        }
        if (setVar instanceof IrSetConstant) {
            return Either.left(((IrSetConstant) setVar).getValue());
        }
        return Either.right(setVars.get(setVar));
    }

    public Either<int[], SetVar>[] getVars(IrSetVar... vars) {
        @SuppressWarnings("unchecked")
        Either<int[], SetVar>[] svars = (Either<int[], SetVar>[]) new Either<?, ?>[vars.length];
        for (int i = 0; i < svars.length; i++) {
            svars[i] = getVar(vars[i]);
        }
        return svars;
    }

    public SetVar[] getSetVars() {
        return setVars.values().toArray(new SetVar[setVars.size()]);
    }

    public int[] getValue(IrSetVar var) {
        Either<int[], SetVar> setVar = getVar(var);
        return setVar.isLeft()
                ? setVar.getLeft()
                : setVar.getRight().getValues();
    }

    public int[][] getValues(IrSetVar... vars) {
        int[][] svalues = new int[vars.length][];
        for (int i = 0; i < svalues.length; i++) {
            svalues[i] = getValue(vars[i]);
        }
        return svalues;
    }

    public String getValue(IrStringVar var) {
        int[] charints = getValues(var.getCharVars());
        char[] chars = new char[charints.length];
        for (int i = 0; i < chars.length; i++) {
            assert charints[i] >= Character.MIN_VALUE && charints[i] <= Character.MAX_VALUE;
            chars[i] = (char) charints[i];
        }
        int length = getValue(var.getLengthVar());
        return new String(chars, 0, length);
    }

    public String[] getValues(IrStringVar... vars) {
        String[] svalues = new String[vars.length];
        for (int i = 0; i < svalues.length; i++) {
            svalues[i] = getValue(vars[i]);
        }
        return svalues;
    }
}
