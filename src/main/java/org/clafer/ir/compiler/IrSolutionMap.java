package org.clafer.ir.compiler;

import org.clafer.collection.ReadMap;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrUtil;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.SetVar;

/**
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
        return IrUtil.notNull("Set var " + var + " not par of IR solution", boolVars.get(var));
    }

    public BoolVar[] getBoolVars(IrBoolVar... vars) {
        BoolVar[] bvars = new BoolVar[vars.length];
        for (int i = 0; i < bvars.length; i++) {
            bvars[i] = getBoolVar(vars[i]);
        }
        return bvars;
    }

    public BoolVar[] getBoolVars() {
        return boolVars.getValues().toArray(new BoolVar[boolVars.size()]);
    }

    public IntVar getIntVar(IrIntVar var) {
        return IrUtil.notNull("Set var " + var + " not par of IR solution", intVars.get(var));
    }

    public IntVar[] getIntVars(IrIntVar... vars) {
        IntVar[] ivars = new IntVar[vars.length];
        for (int i = 0; i < ivars.length; i++) {
            ivars[i] = getIntVar(vars[i]);
        }
        return ivars;
    }

    public IntVar[] getIntVars() {
        return intVars.getValues().toArray(new IntVar[intVars.size()]);
    }

    public SetVar getSetVar(IrSetVar var) {
        return IrUtil.notNull("Set var " + var + " not par of IR solution", setVars.get(var));
    }

    public SetVar[] getSetVars(IrSetVar... vars) {
        SetVar[] svars = new SetVar[vars.length];
        for (int i = 0; i < svars.length; i++) {
            svars[i] = getSetVar(vars[i]);
        }
        return svars;
    }

    public SetVar[] getSetVars() {
        return setVars.getValues().toArray(new SetVar[setVars.size()]);
    }
}
