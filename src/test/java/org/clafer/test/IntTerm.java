package org.clafer.test;

import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.compiler.IrSolutionMap;
import static org.clafer.test.TestUtil.toVar;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.variables.IntVar;

/**
 *
 * @author jimmy
 */
public class IntTerm implements Term {

    private final IrIntVar var;

    IntTerm(IrIntVar var) {
        this.var = var;
    }

    @Override
    public IrIntExpr toIrExpr() {
        return var;
    }

    @Override
    public IrIntVar getIrVar() {
        return var;
    }

    @Override
    public IntVar toChocoVar(Solver solver) {
        return toVar(var, solver);
    }

    @Override
    public int getValue(IrSolutionMap map) {
        return map.getValue(var);
    }
}
