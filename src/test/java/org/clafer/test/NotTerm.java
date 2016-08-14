package org.clafer.test;

import org.chocosolver.solver.Model;
import org.clafer.ir.IrBoolExpr;
import org.clafer.ir.IrIntExpr;
import org.clafer.ir.IrIntVar;
import static org.clafer.ir.Irs.not;
import org.clafer.ir.compiler.IrSolutionMap;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;
import org.chocosolver.solver.variables.Var;

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
    public IntVar toChocoVar(Model model) {
        return ((BoolVar) view.toChocoVar(model)).not();
    }

    @Override
    public int getValue(IrSolutionMap map) {
        return 1 - view.getValue(map);
    }

    @Override
    public String toString() {
        return "!" + view;
    }
}
