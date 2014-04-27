package org.clafer.test;

import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import static org.clafer.ir.Irs.not;
import org.clafer.ir.compiler.IrSolutionMap;
import solver.Solver;
import solver.variables.BoolVar;
import solver.variables.IntVar;
import solver.variables.Var;

/**
 *
 * @author jimmy
 */
public class NotTerm implements Term {

    private final Term view;

    NotTerm(Term view) {
        this.view = view;
    }

    @Override
    public IrIntExpr toIrExpr() {
        return not((IrBoolExpr) view.toIrExpr());
    }

    @Override
    public IrIntVar getIrVar() {
        return view.getIrVar();
    }

    @Override
    public IntVar toChocoVar(Solver solver) {
        return Var.not((BoolVar) view.toChocoVar(solver));
    }

    @Override
    public int getValue(IrSolutionMap map) {
        return 1 - view.getValue(map);
    }
}
