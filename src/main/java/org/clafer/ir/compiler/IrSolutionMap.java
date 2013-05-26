package org.clafer.ir.compiler;

import org.clafer.collection.ReadMap;
import org.clafer.ir.IrBoolConstant;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntConstant;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrSetConstant;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrUtil;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.SetVar;

/**
 * Maps IR non-constant variables to their translated Choco variables. The
 * get&lt;Type&gt;Var methods fail on constants. The get&lt;Type&gt;Value
 * methods handles constants, and is the preferred way of lookup.
 *
 * @author jimmy
 */
public class IrSolutionMap {

    private final ReadMap<IrBoolVar, BoolVar> boolVars;
    private final ReadMap<IrIntVar, IntVar> intVars;
    private final ReadMap<IrSetVar, SetVar> setVars;

    IrSolutionMap(ReadMap<IrBoolVar, BoolVar> boolVars, ReadMap<IrIntVar, IntVar> intVars, ReadMap<IrSetVar, SetVar> setVars) {
        this.boolVars = boolVars.readOnly();
        this.intVars = intVars.readOnly();
        this.setVars = setVars.readOnly();
    }

    public BoolVar getBoolVar(IrBoolVar var) {
        return IrUtil.notNull("Bool var " + var + " not par of IR solution", boolVars.get(var));
    }

    public boolean getBoolValue(IrBoolVar var) {
        if (var instanceof IrBoolConstant) {
            return ((IrBoolConstant) var).getValue();
        }
        return getBoolVar(var).getValue() != 0;
    }

    public BoolVar[] getBoolVars(IrBoolVar... vars) {
        BoolVar[] bvars = new BoolVar[vars.length];
        for (int i = 0; i < bvars.length; i++) {
            bvars[i] = getBoolVar(vars[i]);
        }
        return bvars;
    }

    public boolean[] getBoolValues(IrBoolVar... vars) {
        boolean[] bvalues = new boolean[vars.length];
        for (int i = 0; i < bvalues.length; i++) {
            bvalues[i] = getBoolValue(vars[i]);
        }
        return bvalues;
    }

    public BoolVar[] getBoolVars() {
        return boolVars.getValues().toArray(new BoolVar[boolVars.size()]);
    }

    public IntVar getIntVar(IrIntVar var) {
        return IrUtil.notNull("Int var " + var + " not par of IR solution", intVars.get(var));
    }

    public int getIntValue(IrIntVar var) {
        if (var instanceof IrIntConstant) {
            return ((IrIntConstant) var).getValue();
        }
        return getIntVar(var).getValue();
    }

    public IntVar[] getIntVars(IrIntVar... vars) {
        IntVar[] ivars = new IntVar[vars.length];
        for (int i = 0; i < ivars.length; i++) {
            ivars[i] = getIntVar(vars[i]);
        }
        return ivars;
    }

    public int[] getIntValues(IrIntVar... vars) {
        int[] ivalues = new int[vars.length];
        for (int i = 0; i < ivalues.length; i++) {
            ivalues[i] = getIntValue(vars[i]);
        }
        return ivalues;
    }

    public IntVar[] getIntVars() {
        return intVars.getValues().toArray(new IntVar[intVars.size()]);
    }

    public SetVar getSetVar(IrSetVar var) {
        return IrUtil.notNull("Set var " + var + " not par of IR solution", setVars.get(var));
    }

    public int[] getSetValue(IrSetVar var) {
        if (var instanceof IrSetConstant) {
            return ((IrSetConstant) var).getValue();
        }
        return getSetVar(var).getValue();
    }

    public SetVar[] getSetVars(IrSetVar... vars) {
        SetVar[] svars = new SetVar[vars.length];
        for (int i = 0; i < svars.length; i++) {
            svars[i] = getSetVar(vars[i]);
        }
        return svars;
    }

    public int[][] getSetValues(IrSetVar... vars) {
        int[][] svalues = new int[vars.length][];
        for (int i = 0; i < svalues.length; i++) {
            svalues[i] = getSetValue(vars[i]);
        }
        return svalues;
    }

    public SetVar[] getSetVars() {
        return setVars.getValues().toArray(new SetVar[setVars.size()]);
    }
}
