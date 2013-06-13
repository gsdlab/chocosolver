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

    private final Map<IrBoolVar, BoolVar> boolVars;
    private final Map<IrIntVar, IrIntVar> coalescedIntVars;
    private final Map<IrIntVar, IntVar> intVars;
    private final Map<IrSetVar, SetVar> setVars;

    IrSolutionMap(Map<IrBoolVar, BoolVar> boolVars,
            Map<IrIntVar, IrIntVar> coalescedIntVars,
            Map<IrIntVar, IntVar> intVars,
            Map<IrSetVar, SetVar> setVars) {
        this.boolVars = boolVars;
        this.coalescedIntVars = coalescedIntVars;
        this.intVars = intVars;
        this.setVars = setVars;
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
        return boolVars.values().toArray(new BoolVar[boolVars.size()]);
    }

    public BoolVar[] getBoolDecisionVars() {
        List<BoolVar> decisionVars = new ArrayList<BoolVar>(boolVars.size());
        for (Entry<IrBoolVar, BoolVar> entry : boolVars.entrySet()) {
            if (entry.getKey().isDecision()) {
                decisionVars.add(entry.getValue());
            }
        }
        return decisionVars.toArray(new BoolVar[decisionVars.size()]);
    }

    public IntVar getIntVar(IrIntVar var) {
        IrIntVar intVar = coalescedIntVars.get(var);
        if (intVar == null) {
            intVar = var;
        }
        return IrUtil.notNull("Int var " + var + " not par of IR solution", intVars.get(intVar));
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
        return intVars.values().toArray(new IntVar[intVars.size()]);
    }

    public IntVar[] getIntDecisionVars() {
        List<IntVar> decisionVars = new ArrayList<IntVar>(intVars.size());
        for (Entry<IrIntVar, IntVar> entry : intVars.entrySet()) {
            if (entry.getKey().isDecision()) {
                decisionVars.add(entry.getValue());
            }
        }
        return decisionVars.toArray(new IntVar[decisionVars.size()]);
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
        return setVars.values().toArray(new SetVar[setVars.size()]);
    }

    public SetVar[] getSetDecisionVars() {
        List<SetVar> decisionVars = new ArrayList<SetVar>(setVars.size());
        for (Entry<IrSetVar, SetVar> entry : setVars.entrySet()) {
            if (entry.getKey().isDecision()) {
                decisionVars.add(entry.getValue());
            }
        }
        return decisionVars.toArray(new SetVar[decisionVars.size()]);
    }
}
