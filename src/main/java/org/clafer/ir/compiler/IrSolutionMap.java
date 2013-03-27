package org.clafer.ir.compiler;

import org.clafer.Check;
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

    public IntVar getIntVar(IrIntVar var) {
        return IrUtil.notNull("Set var " + var + " not par of IR solution", intVars.get(var));
    }

    public SetVar getSetVar(IrSetVar var) {
        return IrUtil.notNull("Set var " + var + " not par of IR solution", setVars.get(var));
    }

    public BoolVar[] getBoolVars() {
        return boolVars.getValues().toArray(new BoolVar[boolVars.size()]);
    }

    public IntVar[] getIntVars() {
        return intVars.getValues().toArray(new IntVar[intVars.size()]);
    }

    public SetVar[] getSetVars() {
        return setVars.getValues().toArray(new SetVar[setVars.size()]);
    }
}
