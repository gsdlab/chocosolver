package org.clafer.test;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.variables.BoolVar;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrBoolVar;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.compiler.IrSolutionMap;
import static org.clafer.test.TestUtil.toVar;

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
    public BoolVar toChocoVar(Model model) {
        return toVar(var, model);
    }

    @Override
    public int getValue(IrSolutionMap map) {
        return map.getValue((IrIntVar) var);
    }

    @Override
    public String toString() {
        return var.toString();
    }
}
