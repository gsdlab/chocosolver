package org.clafer.test;

import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import static org.clafer.ir.Irs.constant;
import org.clafer.ir.compiler.IrSolutionMap;
import solver.Solver;
import solver.variables.IntVar;
import solver.variables.VF;

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
        return VF.fixed(c, solver);
    }

    @Override
    public int getValue(IrSolutionMap map) {
        return c;
    }
}
