package org.clafer.ir.compiler;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
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

    public BoolVar getBoolVar(IrBoolVar var) {
        IrBoolVar boolVar = (IrBoolVar) coalescedIntVars.get(var);
        if (boolVar == null) {
            boolVar = var;
        }
        return (BoolVar) intVars.get(boolVar);
    }

    public boolean getBoolValue(IrBoolVar var) {
        IrBoolVar boolVar = (IrBoolVar) coalescedIntVars.get(var);
        if (boolVar == null) {
            boolVar = var;
        }
        if (var instanceof IrBoolConstant) {
            return ((IrBoolConstant) var).getValue();
        }
        return intVars.get(boolVar).getValue() != 0;
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

    public IntVar getIntVar(IrIntVar var) {
        IrIntVar intVar = coalescedIntVars.get(var);
        if (intVar == null) {
            intVar = var;
        }
        return intVars.get(intVar);
    }

    public int getIntValue(IrIntVar var) {
        IrIntVar intVar = coalescedIntVars.get(var);
        if (intVar == null) {
            intVar = var;
        }
        if (intVar instanceof IrIntConstant) {
            return ((IrIntConstant) intVar).getValue();
        }
        return intVars.get(intVar).getValue();
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

    public SetVar getSetVar(IrSetVar var) {
        IrSetVar setVar = coalescedSetVars.get(var);
        if (setVar == null) {
            setVar = var;
        }
        return setVars.get(setVar);
    }

    public int[] getSetValue(IrSetVar var) {
        IrSetVar setVar = coalescedSetVars.get(var);
        if (setVar == null) {
            setVar = var;
        }
        if (setVar instanceof IrSetConstant) {
            return ((IrSetConstant) setVar).getValue();
        }
        return setVars.get(setVar).getValue();
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
}
