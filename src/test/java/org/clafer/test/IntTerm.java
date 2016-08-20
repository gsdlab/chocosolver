package org.clafer.test;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.variables.IntVar;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import org.clafer.ir.compiler.IrSolutionMap;
import static org.clafer.test.TestUtil.toVar;

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
    public IntVar toChocoVar(Model model) {
        return toVar(var, model);
    }

    @Override
    public int getValue(IrSolutionMap map) {
        return map.getValue(var);
    }

    @Override
    public String toString() {
        return var.toString();
    }
}
