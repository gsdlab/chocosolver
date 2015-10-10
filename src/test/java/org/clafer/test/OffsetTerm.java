package org.clafer.test;

import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import static org.clafer.ir.Irs.add;
import org.clafer.ir.compiler.IrSolutionMap;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.Var;

/**
 *
 * @author jimmy
 */
public class OffsetTerm implements Term {

    private final Term view;
    private final int offset;

    OffsetTerm(Term view, int offset) {
        this.view = view;
        this.offset = offset;
    }

    @Override
    public IrIntExpr toIrExpr() {
        return add(view.toIrExpr(), offset);
    }

    @Override
    public IrIntVar getIrVar() {
        return view.getIrVar();
    }

    @Override
    public IntVar toChocoVar(Solver solver) {
        return Var.offset(view.toChocoVar(solver), offset);
    }

    @Override
    public int getValue(IrSolutionMap map) {
        return view.getValue(map) + offset;
    }

    @Override
    public String toString() {
        return view + " + " + offset;
    }
}
