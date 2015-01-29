package org.clafer.test;

import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.compiler.IrSolutionMap;
import static org.clafer.test.TestUtil.toVar;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.variables.BoolVar;

/**
 *
 * @author jimmy
 */
public class BoolTerm implements Term {

    private final IrBoolVar var;

    BoolTerm(IrBoolVar var) {
        this.var = var;
    }

    @Override
    public IrBoolExpr toIrExpr() {
        return var;
    }

    @Override
    public IrIntVar getIrVar() {
        return var;
    }

    @Override
    public BoolVar toChocoVar(Solver solver) {
        return toVar(var, solver);
    }

    @Override
    public int getValue(IrSolutionMap map) {
        return map.getValue((IrIntVar) var);
    }
}
