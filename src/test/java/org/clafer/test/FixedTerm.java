package org.clafer.test;

import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import static org.clafer.ir.Irs.constant;
import org.clafer.ir.compiler.IrSolutionMap;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.Var;

/**
 *
 * @author jimmy
 */
public class FixedTerm implements Term {

    private final int c;

    FixedTerm(int c) {
        this.c = c;
    }

    @Override
    public IrIntExpr toIrExpr() {
        return constant(c);
    }

    @Override
    public IrIntVar getIrVar() {
        return constant(c);
    }

    @Override
    public IntVar toChocoVar(Solver solver) {
        return Var.fixed(c, solver);
    }

    @Override
    public int getValue(IrSolutionMap map) {
        return c;
    }

    @Override
    public String toString() {
        return Integer.toString(c);
    }
}
