package org.clafer.ir.compiler;

import java.util.Map;
import org.chocosolver.solver.Solution;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.SetVar;
import org.clafer.collection.Either;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.IrSetVar;
import org.clafer.ir.IrStringVar;
import org.clafer.ir.analysis.deduction.Coalesce;

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

    private final Coalesce coalesce;
    private final Map<IrIntVar, IntVar> intVars;
    private final Map<IrSetVar, SetVar> setVars;

    IrSolutionMap(
            Coalesce coalesce,
            Map<IrIntVar, IntVar> intVars,
            Map<IrSetVar, SetVar> setVars) {
        this.coalesce = coalesce;
        this.intVars = intVars;
        this.setVars = setVars;
    }

    public Either<Boolean, BoolVar> getVar(IrBoolVar var) {
        IrBoolVar boolVar = coalesce.get(var);
        if (boolVar.isConstant()) {
            return Either.left(boolVar.getLowBound() == 1 ? Boolean.TRUE : Boolean.FALSE);
        }
        return Either.right((BoolVar) intVars.get(boolVar));
    }

    protected int getIntVal(IntVar var) {
        return var.getValue();
    }

    protected int[] getSetVal(SetVar var) {
        return var.getValue().toArray();
    }

    public IrSolutionMap fromSolution(final Solution solution) {
        return new IrSolutionMap(coalesce, intVars, setVars) {

            @Override
            protected int getIntVal(IntVar var) {
                return solution.getIntVal(var);
            }

            @Override
            protected int[] getSetVal(SetVar var) {
                return solution.getSetVal(var);
            }
        };
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
                : getIntVal(boolVar.getRight()) != 0;
    }

    public boolean[] getValues(IrBoolVar... vars) {
        boolean[] bvalues = new boolean[vars.length];
        for (int i = 0; i < bvalues.length; i++) {
            bvalues[i] = getValue(vars[i]);
        }
        return bvalues;
    }

    public Either<Integer, IntVar> getVar(IrIntVar var) {
        IrIntVar intVar = coalesce.get(var);
        if (intVar.isConstant()) {
            return Either.left(intVar.getLowBound());
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
                : getIntVal(intVar.getRight());
    }

    public int[] getValues(IrIntVar... vars) {
        int[] ivalues = new int[vars.length];
        for (int i = 0; i < ivalues.length; i++) {
            ivalues[i] = getValue(vars[i]);
        }
        return ivalues;
    }

    public Either<int[], SetVar> getVar(IrSetVar var) {
        IrSetVar setVar = coalesce.get(var);
        if (setVar.isConstant()) {
            return Either.left(setVar.getKer().getValues());
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
                : getSetVal(setVar.getRight());
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
