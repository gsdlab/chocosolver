package org.clafer.ir.compiler;

import java.util.List;
import solver.variables.IntVar;
import solver.variables.SetVar;

/**
 *
 * @author jimmy
 */
public class IrSolutionMap {

    private final List<IntVar> intVars;
    private final List<SetVar> setVars;

    public IrSolutionMap(List<IntVar> intVars, List<SetVar> setVars) {
        this.intVars = intVars;
        this.setVars = setVars;
    }

    public IntVar[] getIntVars() {
        return intVars.toArray(new IntVar[intVars.size()]);
    }

    public SetVar[] getSetVars() {
        return setVars.toArray(new SetVar[setVars.size()]);
    }
}
