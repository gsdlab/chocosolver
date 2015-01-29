package org.clafer.test;

import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import static org.clafer.ir.Irs.minus;
import org.clafer.ir.compiler.IrSolutionMap;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.Var;

/**
 *
 * @author jimmy
 */
public class MinusTerm implements Term {

    private final Term view;

    MinusTerm(Term view) {
        this.view = view;
    }

    @Override
    public IrIntExpr toIrExpr() {
        return minus(view.toIrExpr());
    }

    @Override
    public IrIntVar getIrVar() {
        return view.getIrVar();
    }

    @Override
    public IntVar toChocoVar(Solver solver) {
        return Var.minus(view.toChocoVar(solver));
    }

    @Override
    public int getValue(IrSolutionMap map) {
        return -view.getValue(map);
    }
}
